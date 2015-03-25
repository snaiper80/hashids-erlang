-module(hashids).
-export([new/0, new/1, encode/2, decode/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([consistent_shuffle/2, hash/2, unhash/2]).
-endif.

% Type declaration
-record (hashids_context, {
    salt       = [] :: list(),
    min_length = 0  :: non_neg_integer(),
    alphabet   = [] :: list(),
    seperators = [] :: list(),
    guards     = [] :: list()
}).

-type hashids_context() :: #hashids_context{}.

-export_type([hashids_context/0]).

% Constants
-define(VERSION,              "0.1.0").

-define(MIN_ALPHABET_LEN,     16).
-define(SEP_DIV,              3.5).
-define(GUARD_DIV,            12).

-define(DEFAULT_ALPHABET,     "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").
-define(DEFAULT_SEPS,         "cfhistuCFHISTU").

%% @doc make a new hashids context (convenient function)
%% @spec new() -> hashids_context().
-spec new() -> hashids_context().
new() ->
    new([]).

%% @doc make a new hashids context
%% @spec new([] | [{salt | default_alphabet | min_hash_length, any()}]) -> hashids_context().
-spec new([] | [{salt | default_alphabet | min_hash_length, any()}]) -> hashids_context().
new(Opts) ->
    Salt            = get_value(salt, Opts, []),
    NotUniqAlphabet = get_value(default_alphabet, Opts, ?DEFAULT_ALPHABET),
    MinHashLength   = erlang:max(get_value(min_hash_length, Opts, 0), 0),
    
    % validate options
    Alphabet = unique(NotUniqAlphabet),
    
    valid = validate_alphabet(Alphabet),
    ok    = validate_salt(Salt),

    {Seps, ShuffledAlphabet}           = setup_sep(Alphabet, Salt),
    {Guards, FinalSeps, FinalAlphabet} = setup_guard(Seps, ShuffledAlphabet),

    % New HashID Context
    #hashids_context {
        salt       = Salt,
        min_length = MinHashLength,
        alphabet   = FinalAlphabet,
        seperators = FinalSeps,
        guards     = Guards
    }.

%% @doc encode numbers 
%% @spec encode(hashids_context(), integer() | [integer(), ...]) -> string().
-spec encode(hashids_context(), integer() | [integer(), ...]) -> string().
encode(_, N) when is_integer(N), N < 0  ->
    "";
encode(Context, N) when is_integer(N), N >= 0 ->
    encode(Context, [N]);
encode(Context, N) when is_list(N) ->
    case lists:any(fun(E) -> is_integer(E) == false orelse E < 0 end, N) of
        true -> "";
        _    -> internal_encode(Context, N)
    end.

%% @doc decode hash string
%% @spec -spec decode(hashids_context(), string()) -> [integer(), ...].
-spec decode(hashids_context(), string()) -> [integer(), ...].
decode(_, []) ->
    "";
decode(Context, HashStr) when is_list(HashStr) ->
    internal_decode(Context, HashStr).


%% ===================================================================
%% Private
%% ===================================================================
internal_encode(_, []) ->
    "";
internal_encode(#hashids_context { salt       = Salt,
                                   min_length = MinHashLength,
                                   alphabet   = Alphabet,
                                   seperators = Seps,
                                   guards     = Guards}, N) ->

    HashInt            = hash_numbers(N),
    {FinalAlphabet, R} = pre_encode(N, HashInt, Salt, Alphabet, Seps),

    NewR =  if  length(R) < MinHashLength ->
                HT = (HashInt + lists:nth(1, R)) rem length(Guards),
                NR = [lists:nth(HT + 1, Guards)] ++ R,

                if  length(NR) < MinHashLength ->
                        HT1 = (HashInt + lists:nth(3, NR)) rem length(Guards),
                        NR ++ [lists:nth(HT1 + 1, Guards)];
                    true ->
                        NR
                end;
            true ->
                R
    end,

    post_encode(NewR, FinalAlphabet, MinHashLength).


pre_encode(N, HashInt, Salt, Alphabet, Seps) ->
    Lottery = lists:nth(HashInt rem length(Alphabet) + 1, Alphabet),

    {FinalAlphabet, R, _} = lists:foldl(fun(E, {Alpha, R0, I}) ->
                                    Buf     = [Lottery] ++ Salt ++ Alpha,
                                    Alpha1  = consistent_shuffle(Alpha, lists:sublist(Buf, 1, length(Alpha))),
                                    Last    = hash(E, Alpha1),

                                    R1      = R0 ++ Last,
                                    
                                    if  (I + 1) < length(N) ->
                                            E1 = E rem (lists:nth(1, Last) + I),
                                            R2 = R1 ++ [lists:nth((E1 rem length(Seps)) + 1, Seps)];
                                        true ->
                                            R2 = R1
                                    end,

                                    {Alpha1, R2, I + 1}

                                end, {Alphabet, [Lottery], 0}, N),

    {FinalAlphabet, R}.

post_encode(R, Alphabet, MinHashLength) when length(R) < MinHashLength ->
    HalfLen           = length(Alphabet) div 2,
    ShuffledAlphabet  = consistent_shuffle(Alphabet, Alphabet),

    R2 = lists:sublist(ShuffledAlphabet, HalfLen + 1, length(ShuffledAlphabet) - HalfLen) ++ 
         R ++ 
         lists:sublist(ShuffledAlphabet, 1, HalfLen),

    Excess = length(R2) - MinHashLength,
    if  Excess > 0 -> lists:sublist(R2, Excess div 2 + 1, MinHashLength);
        true       -> R2
    end;
post_encode(R, _, _) ->
    R.

internal_decode(#hashids_context { salt       = Salt,
                                   alphabet   = Alphabet,
                                   seperators = Seps,
                                   guards     = Guards} = Context, HashStr) ->
    
    HashBreakdown = replace_chars_with_whitespace_in(Guards, HashStr),
    HashArray     = string:tokens(HashBreakdown, " "),

    Breakdown = lists:nth(breakdown_index(length(HashArray)), HashArray),
    Result    = decode_breakdown_hash(Breakdown, Salt, Seps, Alphabet),

    case encode(Context, Result) of
        HashStr ->
            Result;
        _ ->
            []
    end.


replace_chars_with_whitespace_in(Check, Replace) when is_list(Check), is_list(Replace) ->
    lists:map(fun(E) -> 
        case lists:member(E, Check) of
            true -> $\ ;
            _    ->  E
        end
    end, Replace).


breakdown_index(3) -> 2;
breakdown_index(2) -> 2;
breakdown_index(_) -> 1.


decode_breakdown_hash([], _, _, _) ->
    [];
decode_breakdown_hash(Breakdown, Salt, Seps, Alphabet) when is_list(Breakdown), length(Breakdown) > 0 ->
    [Lottery | T] = Breakdown,
    
    Replaced = replace_chars_with_whitespace_in(Seps, T),
    {_, R}   = lists:foldl( fun(E, {Alpha, Acc}) -> 
                                Buf    = [Lottery] ++ Salt ++ Alpha,
                                Alpha1 = consistent_shuffle(Alpha, lists:sublist(Buf, 1, length(Alpha))),
                                    
                                {Alpha1, [unhash(E, Alpha1) | Acc]}

                            end, {Alphabet, []}, string:tokens(Replaced, " ")),

    lists:reverse(R).


validate_alphabet(Alphabet) when is_list(Alphabet) ->
    ok = check_alphabet_len(Alphabet),
    ok = contains_space_in_alphabet(Alphabet),

    valid.

check_alphabet_len(Alphabet) when length(Alphabet) < ?MIN_ALPHABET_LEN ->
    {error, too_short_alphabet};
check_alphabet_len(_) ->
    ok.

contains_space_in_alphabet(Alphabet) ->
    case lists:member($\ , Alphabet) of
        true ->
            {error, alphabet_error};
        false ->
            ok
    end.


validate_salt(Salt) when is_list(Salt) -> ok;
validate_salt(_)                       -> {error, invalid_salt}.


setup_sep(Alphabet, Salt) -> 
    % seps should contain only characters present in alphabet; alphabet should not contains seps
    {NotIn, In}        = lists:partition(fun(S) -> lists:member(S, Alphabet) end, ?DEFAULT_SEPS),
    UnshuffledAlphabet = lists:subtract(Alphabet, NotIn),
    UnshuffledSeps     = lists:subtract(?DEFAULT_SEPS, In),

    Seps                               = consistent_shuffle(UnshuffledSeps, Salt),
    {AdjustedSeps, AdjustedAlphabet}   = calculate_seps(Seps, UnshuffledAlphabet),
    ShuffledAlphabet                   = consistent_shuffle(AdjustedAlphabet, Salt),

    {AdjustedSeps, ShuffledAlphabet}.


calculate_seps(Seps, Alphabet) when length(Alphabet) == 0; 
                                    length(Alphabet) div length(Seps) > ?SEP_DIV ->
    
    SepLength = ceiling(length(Alphabet) / ?SEP_DIV),
    Length = case SepLength of
                    1 -> 2;
                    _ -> SepLength
             end,

    Diff = Length - length(Seps),
    if  Diff > 0 ->
            {lists:append(Seps, lists:sublist(Alphabet, Diff)), lists:sublist(Alphabet, Diff + 1, length(Alphabet) - Diff) };
        true ->
            {lists:sublist(Seps, 1, Length), Alphabet}
    end;
calculate_seps(Seps, Alphabet) ->
    {Seps, Alphabet}.


setup_guard(Seps, Alphabet) ->
    GC = ceiling(length(Alphabet) / ?GUARD_DIV),
    calculate_guard(GC, Seps, Alphabet).

calculate_guard(GC, Seps, Alphabet) when length(Alphabet) < 3 ->
    {lists:sublist(Seps, 1, GC), lists:sublist(Seps, GC + 1, length(Seps) - GC), Alphabet};
calculate_guard(GC, Seps, Alphabet) ->
    {lists:sublist(Alphabet, 1, GC), Seps, lists:sublist(Alphabet, GC + 1, length(Alphabet) - GC)}.


hash_numbers(Numbers) when is_list(Numbers) ->
    {HashInt, _} = lists:foldl(fun(Ele, {H, I}) -> {H + (Ele rem (I + 100)), I + 1} end, {0, 0}, Numbers),
    HashInt.


hash(Input, Alphabet) ->
    hash_loop(Input, Alphabet, []).


hash_loop(0, _, Acc) ->
    Acc;
hash_loop(N, Alphabet, Acc) ->
    Len = length(Alphabet),
    hash_loop(N div Len, Alphabet, [lists:nth((N rem Len) + 1, Alphabet) | Acc]).


unhash(Input, Alphabet) ->
    {ok, Num} = unhash_loop(Alphabet, Input, 1, 0),
    Num.

unhash_loop(_, Input, I, Num) when I > length(Input)->
    {ok, Num};
unhash_loop(Alphabet, Input, I, Num) ->
    Pos = string:chr(Alphabet, lists:nth(I, Input)),
    case Pos of
        0 ->
            {error, cant_unhash};
        _ ->
            H = (Pos - 1) * trunc(math:pow(length(Alphabet), length(Input) - I)),
            unhash_loop(Alphabet, Input, I + 1, Num + H)
    end.


%% ===================================================================
%% Helper Functions
%% ===================================================================
get_value(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        _ -> Default
    end.


unique([])      -> [];
unique([H | T]) -> [H | [X || X <- unique(T), X =/= H]].
    

consistent_shuffle(Alphabet, []) ->
    Alphabet;
consistent_shuffle(Alphabet, Salt) ->
    SaltLength = length(Salt),

    {Shuffled, _, _, _}  =  lists:foldr(
                                fun(_, {_, _, _, 0} = Acc) ->
                                        Acc;
                                   (_, {Al, V, P, L}) -> 
                                        V1 = V rem SaltLength,
                                        N  = lists:nth(V1 + 1, Salt),
                                        P1 = P + N,

                                        J   = (N + V1 + P1) rem L,
                                        Al1 = swap(Al, J, L),

                                        {Al1, V1 + 1, P + N, L - 1}

                                end, {Alphabet, 0, 0, length(Alphabet) - 1}, Alphabet),

    Shuffled.


swap(List, S1, S2) -> 
    {List2, [F | List3]} = lists:split(S1, List),
    LT                   = List2 ++ [lists:nth(S2 + 1, List) | List3],
    {List4, [_ | List5]} = lists:split(S2, LT),
    
    List4 ++ [F | List5].


ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

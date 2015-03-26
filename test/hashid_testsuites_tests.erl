-module(hashid_testsuites_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PARAMSTRING_REGEX, "#\\s+salt:\\s+'([^']*)'\\s+min_len:\\s+(\\d+)\\s+alphabet:\\s+(?:'([^']+)'|(<default>))$").

setup() ->
    ok.

teardown(_) ->
    ok.

make_tests_from_file(FileName) ->
    BaseFiles = filelib:wildcard(FileName),

    [begin
        [Spec | TestStrs] = readlines(F),
        lists:filter(fun(T) -> is_atom(T) == false end, generate_tests(hashid_context_from_spec(Spec), TestStrs))
    end || F <- BaseFiles].


readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    [unicode:characters_to_list(Bin) || Bin <- binary:split(unicode:characters_to_binary(Data), <<"\n">>, [global]), Bin =/= << >>].


make_new_hashid([_, Salt, Len, "<default>"])    -> 
    hashids:new([ {salt, Salt}, {min_hash_length, list_to_integer(Len)}]);
make_new_hashid([_, Salt, Len, Alphabet])       ->
    hashids:new([ {salt, Salt}, {min_hash_length, list_to_integer(Len)}, {default_alphabet, Alphabet}]);
make_new_hashid([_, Salt, Len, _, "<default>"]) -> 
    hashids:new([ {salt, Salt}, {min_hash_length, list_to_integer(Len)}]);
make_new_hashid([_, Salt, Len, _, Alphabet])    -> 
    hashids:new([ {salt, Salt}, {min_hash_length, list_to_integer(Len)}, {default_alphabet, Alphabet}]).
    

hashid_context_from_spec(Spec) when is_list(Spec) ->
    case re:run(Spec, ?PARAMSTRING_REGEX, [{capture, all, list}, unicode]) of
        {match, Captured} ->
            make_new_hashid(Captured);
        nomatch -> 
            invalid
    end.


generate_tests(Ctx, TestCaseStr) ->
    [begin
        case re:run(Str, "^\\[([\\d ]+)\\]\\s+(.+)$", [{capture, all, list}, unicode]) of
            {match, C} ->
                [_, NumStr, Encoded] = C,
                NumList = [ list_to_integer(N) || N <- string:tokens(NumStr, " "), N =/= []],
                ?_assertMatch(Encoded, hashids:encode(Ctx, NumList));
            nomatch ->
                case string:tokens(Str, " ") of
                    ["#", _] ->
                        invalid;
                    [NumStr, Encoded] ->
                        ?_assertMatch(Encoded, hashids:encode(Ctx, list_to_integer(NumStr)));
                    _ ->
                        invalid
                end
        end
    end || Str <- TestCaseStr, Str =/= []].

base_suites_test_() ->
    [
        {
            "Base Suites Tests",
            setup, 
            fun setup/0,
            fun teardown/1,
            fun(_) -> make_tests_from_file("../deps/hashids_test_suites/*/base/*") end
        },
        {
            "Large Suites Tests",
            setup, 
            fun setup/0,
            fun teardown/1,
            fun(_) -> make_tests_from_file("../deps/hashids_test_suites/*/large/*") end
        }
    ].


-endif.
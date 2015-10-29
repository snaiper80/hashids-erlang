-module(hashids_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_ALPHABET,     "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").

teardown(_) -> ok.

%% ===================================================================
%% Tests
%% ===================================================================

setup_context_test_() ->
    [
        {
            "Setup Context Tests",
            setup, fun() -> ok end,
            fun teardown/1,
            fun(_) ->
                    [
                        ?_assertMatch({ hashids_context, [], 0,
                                        "gjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890",
                                        "cfhistuCFHISTU",
                                        "abde"
                                      }, hashids:new()),

                        ?_assertMatch({ hashids_context, "123", 16,
                                        "9a645b8e72d",
                                        "fc01",
                                        "3"
                                      }, hashids:new([{salt, "123"},
                                                      {min_hash_length, 16},
                                                      {default_alphabet, "0123456789abcdef"}])),

                        ?_assertMatch({ hashids_context, "this is my salt", 8,
                                        "5N6y2rljDQak4xgzn8ZR1oKYLmJpEbVq3OBv9WwXPMe7",
                                        "UHuhtcITCsFifS",
                                        "AdG0"
                                      }, hashids:new([{salt, "this is my salt"}, {min_hash_length, 8}])),

                        ?_assertMatch({ hashids_context, "", 0,
                                        "01",
                                        "fhistuCFHISTU",
                                        "c"
                                      }, hashids:new([{default_alphabet, "cfhistuCFHISTU01"}])),

                        % exception
                        ?_assertException(error,
                                          {badmatch, {error, too_short_alphabet}},
                                          hashids:new([{salt, ""},
                                                       {min_hash_length, 0},
                                                       {default_alphabet, "shortalphabet"}])),

                        ?_assertException(error,
                                          {badmatch, {error, alphabet_error}},
                                          hashids:new([{salt, ""},
                                                       {min_hash_length, 0},
                                                       {default_alphabet, "abc odefghijklmnopqrstuv"}])),

                        ?_assertException(error,
                                          {badmatch, {error, invalid_salt}},
                                          hashids:new([{salt, 123}]))
                    ]
            end
        }
    ].

encode_test_() ->
    [
        {
            "Encoding Tests with Salt",
            setup, fun() -> hashids:new([{salt, "this is my salt"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("NkK9",                 hashids:encode(Ctx, 12345)),
                        ?_assertMatch("aBMswoO2UB3Sj",        hashids:encode(Ctx, [683, 94108, 123, 5])),
                        ?_assertMatch("1Wc8cwcE",             hashids:encode(Ctx, [5, 5, 5, 5])),
                        ?_assertMatch("kRHnurhptKcjIDTWC3sx", hashids:encode(Ctx, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])),
                        ?_assertMatch("5x",                   hashids:encode(Ctx, 0))
                    ]
            end
        },
        {
            "Encoding Tests with NO Salt",
            setup, fun() -> hashids:new() end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("",                               hashids:encode(Ctx, -1)),
                        ?_assertMatch("",                               hashids:encode(Ctx, [10, -10])),
                        ?_assertMatch("jR",                             hashids:encode(Ctx, 1)),
                        ?_assertMatch("K8Kv7r8",                        hashids:encode(Ctx, 987654321)),
                        ?_assertMatch("QkoW1vt955nxCVVjZDt5VD2PTgBP72", hashids:encode(Ctx, [21979508, 35563591, 57543099, 93106690, 150649789])),

                        % mininum length
                        ?_assertMatch("QkoW1vt955nxCVVjZDt5VD2PTgBP72", hashids:encode(Ctx, [21979508, 35563591, 57543099, 93106690, 150649789]))
                    ]
            end
        },
        {
            "Encoding Tests / Check mininum length",
            setup, fun() -> hashids:new([{min_hash_length, 18}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("J4q2VolejRejNmGQBW",             hashids:encode(Ctx, 1)),
                        ?_assertMatch("Q16nIJGnhxVRXtwovKu1ypq6hGV5o9", hashids:encode(Ctx, [4140, 21147, 115975, 678570, 4213597, 27644437]))
                    ]
            end
        },
        {
            "Encoding Tests with a Custom alphabet",
            setup, fun() -> hashids:new([{default_alphabet, "ABCDEFGhijklmn34567890-:"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch(":mi8C8F7Am", hashids:encode(Ctx, [1,2,3,4,5]))
                    ]
            end
        },
        {
            "Encoding Tests / DO NOT PRODUCE repeating patterns for incremented numbers",
            setup, fun() -> hashids:new() end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("wpfLh9iwsqt0uyCEFjHM", hashids:encode(Ctx, lists:seq(1, 10)))                    ]
            end
        },
        {
            "Encoding Tests / DO NOT PRODUCE similarities between incrementing number hashes",
            setup, fun() -> hashids:new() end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("jR", hashids:encode(Ctx, 1)),
                        ?_assertMatch("k5", hashids:encode(Ctx, 2)),
                        ?_assertMatch("l5", hashids:encode(Ctx, 3)),
                        ?_assertMatch("mO", hashids:encode(Ctx, 4)),
                        ?_assertMatch("nR", hashids:encode(Ctx, 5))
                    ]
            end
        }
    ].


decode_test_() ->
    [
        {
            "Decoding Tests",
            setup, fun() -> hashids:new([{salt, "this is my salt"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch([],                              hashids:decode(Ctx, "")),
                        ?_assertMatch([12345],                         hashids:decode(Ctx, "NkK9")),
                        ?_assertMatch([683, 94108, 123, 5],            hashids:decode(Ctx, "aBMswoO2UB3Sj")),
                        ?_assertMatch([5, 5, 5, 5],                    hashids:decode(Ctx, "1Wc8cwcE")),
                        ?_assertMatch([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], hashids:decode(Ctx, "kRHnurhptKcjIDTWC3sx"))
                    ]
            end
        },
        {
            "Decoding Tests / Different Salt",
            setup, fun() -> ok end,
            fun teardown/1,
            fun(_) ->
                    [
                        ?_assertMatch([12345], hashids:decode(hashids:new([{salt, "this is my salt"}]), "NkK9")),
                        ?_assertMatch([],      hashids:decode(hashids:new([{salt, "123456"}]),          "NkK9"))
                    ]
            end
        },
        {
            "Decoding Tests / Different Salt",
            setup, fun() -> hashids:new([{salt, "this is my salt"}, {min_hash_length, 8}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch([1],            hashids:decode(Ctx, "gB0NV05e")),
                        ?_assertMatch([25, 100, 950], hashids:decode(Ctx, "mxi8XH87")),
                        ?_assertMatch([5,200,195, 1], hashids:decode(Ctx, "KQcmkIW8hX"))
                    ]
            end
        },
        {
            "Decoding Tests / Different Salt",
            setup, fun() -> hashids:new([{salt, "this is my salt"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertException(error,
                                          {badmatch, {error, cannot_unhash}},
                                           hashids:decode(Ctx, "asdf-'"))
                    ]
            end
        }
    ].

encode_decode_hex_test_() ->
    [
        {
            "Encoding Hex String Tests",
            setup, fun() -> hashids:new([{salt, "this is my salt"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("",                         hashids:encode_hex(Ctx, "")),
                        ?_assertMatch("lzY",                      hashids:encode_hex(Ctx, "FA")),
                        ?_assertMatch("MemE",                     hashids:encode_hex(Ctx, "26dd")),
                        ?_assertMatch("eBMrb",                    hashids:encode_hex(Ctx, "FF1A")),
                        ?_assertMatch("D9NPE",                    hashids:encode_hex(Ctx, "12abC")),
                        ?_assertMatch("9OyNW",                    hashids:encode_hex(Ctx, "185b0")),
                        ?_assertMatch("MRWNE",                    hashids:encode_hex(Ctx, "17b8d")),
                        ?_assertMatch("4o6Z7KqxE",                hashids:encode_hex(Ctx, "1d7f21dd38")),
                        ?_assertMatch("ooweQVNB",                 hashids:encode_hex(Ctx, "20015111d")),
                        ?_assertMatch("j436e8M4Mjfry18O4xnNcJV",  hashids:encode_hex(Ctx, "4f5ce2be8afa96092600000134")),
                        ?_assertMatch("yNyaoWeKWVINWqvaM9bw",     hashids:encode_hex(Ctx, "507f191e810c19729de860ea"))

                    ]
            end
        },
        {
            "Decoding HashString to hex string Tests",
            setup, fun() -> hashids:new([{salt, "this is my salt"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("",                            hashids:decode_hex(Ctx, "")),
                        ?_assertMatch("FA",                          hashids:decode_hex(Ctx, "lzY")),
                        ?_assertMatch("26DD",                        hashids:decode_hex(Ctx, "MemE")),
                        ?_assertMatch("FF1A",                        hashids:decode_hex(Ctx, "eBMrb")),
                        ?_assertMatch("4F5CE2BE8AFA96092600000134",  hashids:decode_hex(Ctx, "j436e8M4Mjfry18O4xnNcJV"))
                    ]
            end
        },
        {
            "Encoding Hex String Tests BUT invalid hexstring",
            setup, fun() -> hashids:new([{salt, "this is my salt"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertException(error, badarg, hashids:encode_hex(Ctx, "HH"))
                    ]
            end
        }
    ].

consistent_shuffle_test_() ->
    [
        {
            "consistent_shuffle function Tests",
            setup, fun() -> ok end,
            fun teardown/1,
            fun(_) ->
                    [
                        ?_assertMatch(?DEFAULT_ALPHABET, hashids:consistent_shuffle(?DEFAULT_ALPHABET, "")),

                        ?_assertMatch("ba",    hashids:consistent_shuffle("ab", "this is my salt")),
                        ?_assertMatch("bca",   hashids:consistent_shuffle("abc", "this is my salt")),
                        ?_assertMatch("cadb",  hashids:consistent_shuffle("abcd", "this is my salt")),
                        ?_assertMatch("dceba", hashids:consistent_shuffle("abcde", "this is my salt")),

                        ?_assertMatch("f17a8zvCwo0iuqYDXlJ4RmAS2end5ghTcpjbOWLK9GFyE6xUI3ZBMQtPsNHrkV",
                                      hashids:consistent_shuffle(?DEFAULT_ALPHABET, "salt")),

                        ?_assertMatch("fcaodykrgqvblxjwmtupzeisnh",
                                      hashids:consistent_shuffle("abcdefghijklmnopqrstuvwxyz",
                                      "this is my salt"))
                    ]
            end
        }
    ].

private_hash_unhash_test_() ->
    [
        {
            "hash Tests",
            setup, fun() -> ok end,
            fun teardown/1,
            fun(_) ->
                    [
                        ?_assertMatch("a",          hashids:hash(0,       "abcdefg")),
                        ?_assertMatch("bf",         hashids:hash(12,      "abcdefg")),
                        ?_assertMatch("ga",         hashids:hash(42,      "abcdefg")),
                        ?_assertMatch("cde",        hashids:hash(123,     "abcdefg")),
                        ?_assertMatch("cggc",       hashids:hash(1024,    "abcdefg")),
                        ?_assertMatch("bbadeefc",   hashids:hash(950000,  "abcdefg")),
                        ?_assertMatch("ääå-ÅÅÄö",   hashids:hash(950000,  "åäö-ÅÄÖ")),
                        ?_assertMatch("ebfbfaea",   hashids:hash(3500000, "abcdefg")),
                        ?_assertMatch("1y-y-X1X",   hashids:hash(3500000, "Xyz01-å"))
                    ]
            end
        },
        {
            "unhash Tests",
            setup, fun() -> ok end,
            fun teardown/1,
            fun(_) ->
                    [
                        ?_assertMatch(59,         hashids:unhash("abbd", "abcdefg")),
                        ?_assertMatch(66,         hashids:unhash("abcd", "abcdefg")),
                        ?_assertMatch(100,        hashids:unhash("acac", "abcdefg")),
                        ?_assertMatch(139,        hashids:unhash("acfg", "abcdefg")),
                        ?_assertMatch(218,        hashids:unhash("x21y", "xyz1234")),
                        ?_assertMatch(440,        hashids:unhash("yy44", "xyz1234")),
                        ?_assertMatch(1045,       hashids:unhash("1xzz", "xyz1234"))
                    ]
            end
        }
    ].

getter_test_() ->
    [
        {
            "getter Tests",
            setup, fun() -> hashids:new([{salt, "this is my salt"},
                                         {min_hash_length, 18},
                                         {default_alphabet, "ABCDEFGhijklmn34567890-:"}]) end,
            fun teardown/1,
            fun(Ctx) ->
                    [
                        ?_assertMatch("this is my salt",  hashids:salt(Ctx)),
                        ?_assertMatch("D8nG:mk-95jE07l6", hashids:alphabet(Ctx)),
                        ?_assertMatch(18,                 hashids:min_hash_length(Ctx))
                    ]
            end
        }
    ].

-endif.
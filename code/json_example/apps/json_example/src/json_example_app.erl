%%%-------------------------------------------------------------------
%% @doc json_example public API
%% @end
%%%-------------------------------------------------------------------

-module(json_example_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] json_example_app: " ++ S ++ "~n")).
-define(INFO(S), io:fwrite("[INFO] json_example_app: " ++ S ++ "~n")).
-define(INFO(S, Args), io:fwrite("[INFO] json_example_app: " ++ S ++ "~n", Args)).

start(_StartType, _StartArgs) ->
    ?DEBUG("start/2"),
    using_jsone(),
    using_jsone_japanese(),
    json_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
using_jsone() ->
    ?DEBUG("using_jsone/0"),
    ?INFO("-----------------------------------"),
    ?INFO("using_jsone: json decode"),
    ?INFO("using_jsone: array ~w", [jsone:decode(<<"[1, 2, 3]">>)]),
    ?INFO("using_jsone: key, value ~w", [jsone:decode(<<"{\"key\": \"value\"}">>)]),
    key,
    ?INFO("using_jsone: attempt atom ~w", [
      jsone:decode(
        <<"{\"key\": \"value\"}">>,
        [{keys, attempt_atom}])
    ]),
    ?INFO(
      "using_jsone: tuple format ~w", 
      [
        jsone:decode(
          <<"{\"key\": \"value\"}">>, 
          [{object_format, tuple}])]),
    ?INFO(
      "using_jsone: proplist format ~w", 
      [
        jsone:decode(
          <<"{\"key\": \"value\"}">>, 
          [{object_format, proplist}])]),
    ?INFO(
      "using_jsone: try succeed ~w", 
      [
        jsone:try_decode(
          <<"[1,1,2]">>)
      ]),
    ?INFO(
      "using_jsone: try failure ~w", 
      [
        jsone:try_decode(
          <<"{[1,1,2] \"undefined\" }">>)
      ]),
    ?INFO("-----------------------------------"),
    ?INFO("using_jsone: json encode"),
    ?INFO("using_jsone: array ~w", [jsone:encode([1,2,3])]),
    ?INFO("using_jsone: map ~w", [jsone:encode(#{<<"key">> => <<"value">>})]),
    ?INFO("using_jsone: tuple ~w", [jsone:encode({[{<<"key">>, <<"value">>}]})]),
    ?INFO("using_jsone: proplist ~w", [jsone:encode([{<<"key">>, <<"value">>}])]),
    ?INFO("using_jsone: atom key ~w", [jsone:encode(#{key => <<"value">>})]),
    ?INFO(
      "using_jsone: try succeed ~w", 
      [
        jsone:try_encode(#{<<"key">> => <<"value">>})
      ]),
    ?INFO(
      "using_jsone: try failure ~w", 
      [
        jsone:try_encode(#{1234 => <<"value">>})
      ]),
    %%     ?INFO("using_jsone: non string key ~w", [
    %%       jsone:encode(
    %%         {[{123 => <<"value">>}]},
    %%         [{object_key_type, scalar}])
    %%     ]),
    ?INFO("using_jsone: undefined to null ~w", [
      jsone:encode(undefined, 
      [undefined_as_null])
    ]),
    Data = [
      true, 
      #{
        <<"1">> => 2, 
        <<"array">> => [
          [[[1]]], 
          #{<<"ab">> => <<"cd">>}, 
          [], 
          #{}, 
          false
        ]
      }, 
      null
    ],
    ?INFO("using_jsone: pritty print ~w", [
      jsone:encode(Data, 
      [{indent, 2}, {space, 1}])
    ]),
    ?INFO("using_jsone: integer ~w", [
      jsone:encode(1)
    ]),
    ?INFO("using_jsone: float ~w", [
      jsone:encode(1.23)
    ]),
    ?INFO("using_jsone: decimal format ~w", [
      jsone:encode(1.23, [{float_format, [{decimals, 4}]}])
    ]),
    ?INFO("using_jsone: decimal compact format ~w", [
      jsone:encode(1.23, [{float_format, [{decimals, 4}, compact]}])
    ]),
    ?INFO("-----------------------------------"),
    ?INFO("using_jsone: json encode and decode"),
    DecResult = jsone:decode(<<"{\"key\": \"value\"}">>),
    ?INFO("using_jsone: encode json after decode ~w", [
      jsone:encode(DecResult)
    ]).

using_jsone_japanese() ->
    ?DEBUG("using_jsone_japanese/0"),
    ?INFO("-----------------------------------"),
    ?INFO("using_jsone_japanese: json decode"),
    ?INFO("using_jsone_japanese: key, value japanese ~w", [
      jsone:try_decode(
        <<"{\"家事指示\": \"洗濯物\"}">>,
        [{allow_ctrl_chars, true}]
      )
    ]),
    ?INFO("using_jsone_japanese: tuple format japanese ~w", [
      jsone:try_decode(
        <<"{\"家事指示\": \"洗濯物\"}">>,
        [
          {object_format, tuple},
          {allow_ctrl_chars, true}
        ]
      )
    ]),
    ?INFO("using_jsone_japanese: proplist format japanese ~w", [
      jsone:try_decode(
        <<"{\"家事指示\": \"洗濯物\"}">>,
        [
          {object_format, proplist},
          {allow_ctrl_chars, true}
        ]
      )
    ]),
    ?INFO("-----------------------------------"),
    ?INFO("using_jsone_japanese: json encode"),
    ?INFO("using_jsone_japanese: map japanese ~w", [
      jsone:try_encode(
        #{<<"家事指示"/utf8>> => <<"洗濯物"/utf8>>}
      )
    ]),
    ?INFO("using_jsone_japanese: map japanese with option~w", [
      jsone:try_encode(
        #{<<"家事指示"/utf8>> => <<"洗濯物"/utf8>>},
        [native_utf8, native_forward_slash]
      )
    ]),
    ?INFO("using_jsone_japanese: tuple japanese ~w", [
      jsone:try_encode(
        {[{<<"家事指示"/utf8>>, <<"洗濯物"/utf8>>}]}
      )
    ]),
    ?INFO("using_jsone_japanese: proplist japanese ~w", [
      jsone:try_encode(
        [{<<"家事指示"/utf8>>, <<"洗濯物"/utf8>>}]
      )
    ]),
    ?INFO("-----------------------------------"),
    ?INFO("using_jsone_japanese: json encode and decode"),
    case jsone:try_decode(
        <<"{\"家事指示\": \"洗濯物\"}"/utf8>>) of
        {ok, DecResult, _} ->
        ?INFO("using_jsone_japanese: encode after decode ~w", [
          jsone:try_encode(
            DecResult,
            [native_forward_slash])]);
        {error, ErrResult} ->
          ?INFO("decode error ~w", [ErrResult])
    end.

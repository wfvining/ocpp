-module(ocpp_rpc_test).

-include_lib("eunit/include/eunit.hrl").

-define(FRAMEWORK_ERROR, {error, rpc_framework_error}).
-define(BOOT_NOTIFICATION_REQUEST,
        "{\"reason\": \"PowerUp\", \"chargingStation\": "
        "{\"model\": \"testModel\", \"vendor\": \"testVendor\"}}").

decode_framework_error_test_() ->
    {"Invalid RPC messages result in a framework_error",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      {inparallel,
       [not_list(),
        list_too_short(),
        typeid_not_int(),
        message_id_not_string(),
        message_id_too_long(),
        action_not_string(),
        bad_json()]}}}.

message_id_test() ->
    {"MessageID can be exactly 36 characters",
     {setup,
      fun init_schemas/0,
      fun(_) -> ok end,
      [?_assertMatch(
          {ok, _},
          ocpp_rpc:decode(
            <<"[2, \"abcdefghijklmnopqrstuvwxyzabcdeabcde\",",
              " \"BootNotification\",",
              ?BOOT_NOTIFICATION_REQUEST, "]">>))]}}.

init_schemas() ->
    ocpp_schema:init_schemas(
      filename:join(code:priv_dir(ocpp), "json_schemas")).

bad_json() ->
    [?_assertEqual(
        ?FRAMEWORK_ERROR,
        ocpp_rpc:decode(<<"[2, \"αβ\", \"BootNotification\", ",
                          ?BOOT_NOTIFICATION_REQUEST, "]">>))].

action_not_string() ->
    ?_assertEqual(?FRAMEWORK_ERROR,
                  ocpp_rpc:decode(
                    list_to_binary(
                      "[2, \"testid\", 23, "?BOOT_NOTIFICATION_REQUEST"]"))).

message_id_too_long() ->
    {"MessageID must be less than 36 characters",
     [?_assertEqual(
         ?FRAMEWORK_ERROR,
         ocpp_rpc:decode(
           <<"[2, \"abcdefghijklmnopqrstuvwxyzabcdeabcdeXXX\", \"BootNotification\"",
             ?BOOT_NOTIFICATION_REQUEST, "]">>))]}.

message_id_not_string() ->
    [?_assertEqual(?FRAMEWORK_ERROR, ocpp_rpc:decode(list_to_binary(Message)))
     || Message <- ["[2, 42, \"BootNotification\", "
                    ?BOOT_NOTIFICATION_REQUEST "]",
                    "[3, true, {\"currentTime\": \"2013-02-01T20:53:32.486Z\","
                    "\"interval\": 300, \"status\": \"Accepted\"}]",
                    "[4, null, \"NotSupported\", \"\", {}]"]].

typeid_not_int() ->
    Payload = "\"msgid\", \"BootNotification\", " ?BOOT_NOTIFICATION_REQUEST,
    [?_assertEqual(?FRAMEWORK_ERROR, ocpp_rpc:decode(list_to_binary(Message)))
     || Message <- ["[1.4, " ++ Payload ++ "]",
                    "[true, " ++ Payload ++ "]",
                    "[\"\"," ++ Payload ++ "]"]].

list_too_short() ->
    [?_assertEqual(?FRAMEWORK_ERROR, ocpp_rpc:decode(list_to_binary(Message)))
     || Message <- ["[]", "[2]", "[2, \"msgid\"]",
                    "[2, \"msgid\", \"BootRequest\"]",
                    "[3]", "[3, \"msgid\"]"]].

not_list() ->
    [?_assertEqual(?FRAMEWORK_ERROR, ocpp_rpc:decode(Message))
     || Message <- [<<"1.2">>, <<"{\"foo\": 32}">>, <<"\"string\"">>]].

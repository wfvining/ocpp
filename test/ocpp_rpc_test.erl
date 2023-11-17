-module(ocpp_rpc_test).

-include_lib("eunit/include/eunit.hrl").

-define(BOOT_NOTIFICATION_REQUEST,
        "{\"reason\": \"PowerUp\", \"chargingStation\": "
        "{\"model\": \"testModel\", \"vendorName\": \"testVendor\"}}").
-define(MESSAGE_ID, atom_to_binary(?FUNCTION_NAME)).

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

message_id_test_() ->
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

unknown_message_type_test_() ->
    {"decoding a message with type ID other than 2, 3, 4 "
     "results in a 'MessageTypeNotSupported' error",
     {inparallel,
      [?_assertEqual(
          {error, ocpp_error:new('MessageTypeNotSupported', <<"-1">>)},
          ocpp_rpc:decode(<<"[1, true, false]">>)),
       ?_assertEqual(
          {error, ocpp_error:new('MessageTypeNotSupported', <<"-1">>)},
          ocpp_rpc:decode(<<"[-1]">>)),
       ?_assertEqual(
          {error, ocpp_error:new('MessageTypeNotSupported', <<"-1">>)},
          ocpp_rpc:decode(<<"[5, \"testid\", \"foo\", {}]">>))]}}.

bad_ocpp_message_test_() ->
    MessageID = atom_to_binary(?FUNCTION_NAME),
    {"decoding a syntactically incorrect OCPP message "
     "results in a protocol_error",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      {inparallel,
       [?_assertEqual(
           {error, ocpp_error:new('ProtocolError', MessageID)},
           ocpp_rpc:decode(
             <<"[2, ",
               "\"", MessageID/binary, "\", ",
               "\"BootNotification\", ",
               Payload/binary, "]">>))
        || Payload <- [<<"\"string\"">>, <<"[1, 2, {}]">>, <<"1">>,
                       <<"true">>, <<"null">>, <<"1.5">>]]}}}.

missing_property_test_() ->
    MessageID = ?MESSAGE_ID,
    {"decoding a rpccall with a payload missing required properties "
     "results in an occurrence_violation",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      [?_assertEqual(
          {error, ocpp_error:new('OccurrenceConstraintViolation', ?MESSAGE_ID)},
          ocpp_rpc:decode(
            <<"[2, \"", MessageID/binary, "\", "
              "\"BootNotification\", {\"reason\": \"PowerUp\"}]">>))]}}.

extra_property_test_() ->
    MessageID = ?MESSAGE_ID,
    {"decoding a rpccall with a payload containing extra properties "
     "results in an occurrence_violation",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      [?_assertEqual(
          {error, ocpp_error:new('OccurrenceConstraintViolation', ?MESSAGE_ID)},
          ocpp_rpc:decode(
            <<"[2, \"", MessageID/binary, "\", "
              "\"BootNotification\", "
              "{\"reason\": \"PowerUp\", "
              "\"chargingStation\": "
              "{\"model\": \"foo\", \"vendorName\": \"bar\"}, "
              "\"extra\": 2}]">>))]}}.

bad_property_test_() ->
    MessageID = ?MESSAGE_ID,
    {"decoding an rpccall with a payload containing a property with an "
     "invalid value results in a property_violation",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      [?_assertEqual(
          {error, ocpp_error:new('PropertyConstraintViolation', ?MESSAGE_ID)},
          ocpp_rpc:decode(
            <<"[2, \"", MessageID/binary, "\", "
              "\"BootNotification\", "
              "{\"reason\": \"PowerUp\", "
              "\"chargingStation\": "
              "{\"model\": \"foo\", \"vendorName\": "
              "\"abcdefghijklmnopqrstuvwxyz01234567890-too_long-"
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}}]">>))]}}.

bad_type_test_() ->
    MessageID = ?MESSAGE_ID,
    {"invalid enum results in a type_violation",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      [?_assertEqual(
          {error, ocpp_error:new('TypeConstraintViolation', ?MESSAGE_ID)},
          ocpp_rpc:decode(
            <<"[2, \"", MessageID/binary, "\", "
              "\"BootNotification\", "
              "{\"reason\": \"PowerDown\", "
              "\"chargingStation\": "
              "{\"model\": \"foo\", \"vendorName\": \"bar\"}}]">>)),
       ?_assertEqual(
          {error, ocpp_error:new('TypeConstraintViolation', ?MESSAGE_ID)},
          ocpp_rpc:decode(
            <<"[2, \"", MessageID/binary, "\", "
              "\"BootNotification\", "
              "{\"reason\": \"PowerUp\", "
              "\"chargingStation\": \"notastring\"}]">>))]}}.

unknown_action_test_() ->
    MessageID = ?MESSAGE_ID,
    {"decoding a request with an unknown action "
     "results in a not_implemented error",
     {setup, fun init_schemas/0, fun(_) -> ok end,
      ?_assertEqual(
         {error, ocpp_error:new('NotImplemented', MessageID)},
         ocpp_rpc:decode(
           <<"[2, \"", MessageID/binary, "\", ",
             "\"NoSuchAction\", {}]">>))}}.

init_schemas() ->
    ocpp_schema:init_schemas(
      filename:join(code:priv_dir(ocpp), "json_schemas")).

bad_json() ->
    [?_assertEqual(
        {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)},
        %% This JSON fails to parse because of a bad string
        ocpp_rpc:decode(<<"[2, \"αβ\", \"BootNotification\", ",
                          ?BOOT_NOTIFICATION_REQUEST, "]">>))].

action_not_string() ->
    ?_assertEqual({error, ocpp_error:new('RpcFrameworkError', <<"testid">>)},
                  ocpp_rpc:decode(
                    list_to_binary(
                      "[2, \"testid\", 23, "?BOOT_NOTIFICATION_REQUEST"]"))).

message_id_too_long() ->
    {"MessageID must be less than 36 characters",
     [?_assertEqual(
         {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)},
         ocpp_rpc:decode(
           <<"[2, \"abcdefghijklmnopqrstuvwxyzabcdeabcdeXXX\", \"BootNotification\"",
             ?BOOT_NOTIFICATION_REQUEST, "]">>))]}.

message_id_not_string() ->
    [?_assertEqual({error, ocpp_error:new('RpcFrameworkError', <<"-1">>)},
                   ocpp_rpc:decode(list_to_binary(Message)))
     || Message <- ["[2, 42, \"BootNotification\", "
                    ?BOOT_NOTIFICATION_REQUEST "]",
                    "[3, true, {\"currentTime\": \"2013-02-01T20:53:32.486Z\","
                    "\"interval\": 300, \"status\": \"Accepted\"}]",
                    "[4, null, \"NotSupported\", \"\", {}]"]].

typeid_not_int() ->
    Payload = "\"msgid\", \"BootNotification\", " ?BOOT_NOTIFICATION_REQUEST,
    [?_assertEqual({error, ocpp_error:new('RpcFrameworkError', <<"-1">>)}, ocpp_rpc:decode(list_to_binary(Message)))
     || Message <- ["[1.4, " ++ Payload ++ "]",
                    "[true, " ++ Payload ++ "]",
                    "[\"\"," ++ Payload ++ "]"]].

list_too_short() ->
    [?_assertEqual({error, ocpp_error:new('RpcFrameworkError', <<"-1">>)},
                   ocpp_rpc:decode(list_to_binary(Message)))
     || Message <- ["[]", "[2]", "[2, \"msgid\"]",
                    "[2, \"msgid\", \"BootRequest\"]",
                    "[3]", "[3, \"msgid\"]"]].

not_list() ->
    [?_assertEqual({error, ocpp_error:new('RpcFrameworkError', <<"-1">>)},
                   ocpp_rpc:decode(Message))
     || Message <- [<<"1.2">>, <<"{\"foo\": 32}">>, <<"\"string\"">>]].

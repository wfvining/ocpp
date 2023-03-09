%%% @doc Erlang representations of OCPP request messages.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_request).

-export([from_json/2]).
-export([boot_notification/2, charging_station/2, charging_station/3]).

-export_type([request/0]).

-type boot_reason() :: application_reset
                     | firmware_update
                     | local_reset
                     | power_up
                     | remote_reset
                     | scheduled_reset
                     | triggered
                     | unknown
                     | watchdog.

-type modem() :: #{ iccid => binary(), imsi => binary() }.

-type charging_station() :: #{ model := binary(),
                               vendor_name := binary(),
                               serial_number => binary(),
                               firmware_version => binary(),
                               modem => modem() }.

-type boot_notification() :: #{ reason := boot_reason(),
                                charging_station := charging_station() }.

-type payload() :: boot_notification().

-type action() :: boot_notification.
                %% | transaction ... eventually this will include all requests.

-type request() :: {action(), payload()}.

%% @doc construct a new request object from a decoded JSON value.
-spec from_json(Action :: binary(), Payload :: #{binary() => Value})
               -> request() when Value :: binary()
                                        | number()
                                        | boolean()
                                        | null
                                        | [Value]
                                        | #{binary() => Value}.
from_json(<<"BootNotification">>, Payload) ->
    {boot_notification,
     boot_notification(
       boot_reason(maps:get(<<"reason">>, Payload)),
       charging_station_from_json(maps:without([<<"reason">>], Payload)))}.

boot_reason(<<"ApplicationReset">>) -> application_reset;
boot_reason(<<"FirmwareUpdate">>) -> firmware_update;
boot_reason(<<"LocalReset">>) -> local_reset;
boot_reason(<<"PowerUp">>) -> power_up;
boot_reason(<<"RemoteReset">>) -> remote_reset;
boot_reason(<<"ScheduledReset">>) -> scheduled_reset;
boot_reason(<<"Triggered">>) -> triggered;
boot_reason(<<"Unknown">>) -> unknown;
boot_reason(<<"Watchdog">>) -> watchdog.

charging_station_from_json(Payload) ->
    charging_station(
      maps:get(<<"model">>, Payload),
      maps:get(<<"vendorName">>, Payload),
      dict:from_list(
        maps:fold(
          fun (<<"modem">>, Modem, Acc) -> [{modem, modem_from_json(Modem)}|Acc];
              (<<"serialNumber">>, SN, Acc) -> [{serial_number, SN}|Acc];
              (<<"firmwareVersion">>, FwVer, Acc) -> [{firmware_version, FwVer}|Acc]
          end, [],
          maps:without([<<"model">>, <<"vendorName">>], Payload)))).

modem_from_json(Modem) ->
    maps:fold(
     fun (<<"iccid">>, ICCID, Acc) -> maps:put(iccid, ICCID, Acc);
         (<<"imsi">>, IMSI, Acc) -> maps:put(imsi, IMSI, Acc)
     end, #{}, Modem).

%% @doc Create a new `BootNotificationRequest'.
-spec boot_notification(Reason :: boot_reason(),
                        Station :: charging_station()) ->
          boot_notification().
boot_notification(Reason, Station) ->
    #{reason => Reason, charging_station => Station}.

%% @doc Create a new `ChargingStationType' object.
-spec charging_station(Model :: binary(), VendorName :: binary()) ->
          charging_station().
charging_station(Model, VendorName) ->
    charging_station(Model, VendorName, dict:new()).

%% @doc Create a new `ChargingStationType' object.
%%
%% XXX does not do any validation of its input. It is possible to
%%     construct an invalid `charging_station' with this function.
-spec charging_station(Model :: binary(),
                       VendorName :: binary(),
                       OptionalAttributes :: dict:dict()) ->
          charging_station().
charging_station(Model, VendorName, OptionalAttributes) ->
    maps:merge(maps:from_list(dict:to_list(OptionalAttributes)),
               #{model => Model, vendor_name => VendorName}).

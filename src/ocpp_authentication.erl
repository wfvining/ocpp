%%% @doc Authenticating charging stations with the CSMS.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_authentication).

-export([install/1]).
-export([set_password/1, verify/2]).

-record(basic_auth, {username :: binary(),
                     salt :: binary(),
                     password_hash :: binary()}).

install(Nodes) ->
    mnesia:create_table(basic_auth, [{attributes, record_info(fields, basic_auth)},
                                     {disc_copies, Nodes}]).

%% @doc Set a new random password for a charging station.
%%
%% Returns the new password.
%% @end
-spec set_password(Id :: binary()) -> binary().
set_password(Id) ->
    {Password, Salt} = random_password(),
    PasswordHash = hash_password(Password, Salt),
    mnesia:write(#basic_auth{username = Id, salt = Salt, password_hash = PasswordHash}),
    Password.

%% @doc Generate a random passowrd and salt
-spec random_password() -> {Password :: binary(), Salt :: binary()}.
random_password() ->
    {random_printable(40), random_printable(20)}.

random_printable(NumBytes) ->
    << <<(32 + (Byte rem ($~ - $ ))):8>>
       || <<Byte:8>> <= crypto:strong_rand_bytes(NumBytes)>>.

%% @doc Compute the hash of `Password' with the given `Salt'.
-spec hash_password(Password :: binary(), Salt :: binary()) -> binary().
hash_password(Password, Salt) ->
    %% NOTE: because we only allow randomly generated 40 character
    %%       passwords SHA256 is probably sufficient. However, for a
    %%       production system it would likely be advisable to replace
    %%       this with a more secure hash such as pbkdf2 or bcrypt.
    crypto:hash(sha256, <<Password/binary, Salt/binary>>).

-spec verify(Id :: binary(), Password :: binary()) -> boolean().
verify(Id, Password) ->
    F = fun() ->
                [Auth] = mnesia:read({basic_auth, Id}),
                Auth
        end,
    case mnesia:transaction(F) of
        {atomic, Auth} ->
            Hash = hash_password(Password, Auth#basic_auth.salt),
            Hash =:= Auth#basic_auth.password_hash;
        _ -> false
    end.

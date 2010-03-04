%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_sdb.erl
%%% Author  : Eric Cestari <eric@ohmforce.com>
%%% Purpose : Authentication on SimpleDB schema
%%% Created : 26/02/09 by Eric Cestari <eric@ohmforce.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_sdb).
-author('eric@ohmforce.com').
%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users_number/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).


-include("ejabberd.hrl").
-define(DOMAIN, "users").
% key : user@host
% attributes : user host password
% Domain users
start(Host)->
    Domain = get_domain(Host),
    erlsdb:start(),
    {ok, Domains, _Token}  = erlsdb:list_domains(),
    case lists:member(Domain, Domains) of 
        false ->
            erlsdb:create_domain(Domain),
            ?INFO_MSG("SimpleDB domain ~s created", [Domain]);
        true -> ok
    end,
    ok.
    
plain_password_required() ->
    true.

check_password(User, Server, Password) ->
    LUser = jlib:nameprep(User),
    LServer = jlib:nodeprep(Server),
    Salt = os:getenv("EJABBERD_SALT"),
    case catch erlsdb:get_attributes(get_domain(Server), LUser ++"@" ++LServer, ["password"]) of
        {ok, [{"password", Hash}]} -> 
            Computed = sha2:hexdigest256(Password ++ Salt),
            Hash == Computed;
        _ -> false
    end.

check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

is_user_exists(User, Server) ->
    LUser = jlib:nameprep(User),
    LServer = jlib:nodeprep(Server),
    case catch erlsdb:get_attributes(get_domain(Server), LUser ++"@" ++LServer) of
    {ok, []} -> false;
	{ok, _U} ->
	    true;
	_ ->
	    false
    end.

set_password(User, Server, Password) ->
    LUser = jlib:nameprep(User),
    LServer = jlib:nodeprep(Server),
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    Salt = os:getenv("EJABBERD_SALT"),
	    Hash = sha2:hexdigest256(Password ++ Salt),
	    erlsdb:replace_attributes(get_domain(Server), LUser ++"@" ++LServer, [{"password", Hash}]),
	    ok
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(User, Server, Password) ->
    LUser = jlib:nameprep(User),
    LServer = jlib:nameprep(Server),
    Domain = get_domain(Server),
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    case catch erlsdb:get_attributes(Domain,  LUser ++"@" ++LServer) of
            {ok, []} -> 
                Salt = os:getenv("EJABBERD_SALT"),
                erlsdb:put_attributes(Domain, LUser ++"@" ++LServer, [{"password", sha2:hexdigest256(Password ++ Salt)}, {"name", LUser}, {"host", LServer}]),
                {atomic, ok} ;
	        {ok, _U} ->
	            {atomic, exists};
	        _ ->
	            {aborted, "An error occured"}
            end
    end.

dirty_get_registered_users()->
    get_users([], nil, none).

get_vh_registered_users(Host)->
    get_users([], "['host' = '"++ Host ++"']", none).

get_vh_registered_users_number(Host)->
   {ok,[{"Domain",[{"Count",SCount}]}],nil} = erlsdb:s("select count(*) from " ++ get_domain(Host) ++" where host = '"++Host++"'"),
   {Count, _} = string:to_integer(SCount),
   Count.

get_users(Acc, _Query, nil)-> Acc;
get_users([], Query, none) ->
    {ok, Users, T} = erlsdb:q("users", Query, 250, nil),
    NewAcc = lists:map(fun(JID)->
            [U, S] = string:tokens(JID, "@"),
            {U,S}
        end, Users),
    get_users(NewAcc,Query, T);
get_users(Acc, Query, Token) ->
    {ok, Users, T} = erlsdb:q("users", Query, 250, Token),
    NewAcc = lists:map(fun(JID)->
            [U, S] = string:tokens(JID, "@"),
            {U,S}
        end, Users) ++ Acc,
    get_users(NewAcc,Query, T).


get_password(_User, _Server) ->
    false.
    %LUser = jlib:nodeprep(User),
    %LServer = jlib:nameprep(Server),
    %JID = LUser ++ "@" ++LServer,
    %case catch erlsdb:get_attributes(?DOMAIN, JID, ["password"]) of
    %{ok, [{"password", Password}]} ->
	%    Password;
	%_ ->
	%    false
    %end.

get_password_s(_User, _Server) ->
    [].
    %LUser = jlib:nodeprep(User),
    %LServer = jlib:nameprep(Server),
    %JID = LUser ++ "@" ++LServer,
    %case catch erlsdb:get_attributes(?DOMAIN, JID, ["password"]) of
    %{ok, [{"password", Password}]} ->
	%    Password;
	%_ ->
	%    []
    %end.
 
%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(User, Server) ->
    LUser = jlib:nameprep(User),
    LServer = jlib:nodeprep(Server),
    catch erlsdb:delete_attributes(get_domain(Server), LUser ++"@" ++LServer, ["password", "user", "host"]),
	ok.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    LUser = jlib:nameprep(User),
    LServer = jlib:nodeprep(Server),
    case catch erlsdb:get_attributes(get_domain(Server), LUser ++"@" ++LServer, ["password"]) of
    {ok, [{"password", Password}]} ->
			remove_user(User, Server),
			ok;
	{ok,[_]} ->
			not_allowed;
	{ok, []} ->
			not_exists;
	_ -> bad_request
	end.
 
get_domain(Host)->
    case ejabberd_config:get_local_option({sdb_domain, Host}) of
        undefined -> ?DOMAIN;
        Domain -> Domain
    end.
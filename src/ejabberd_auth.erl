%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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

%% TODO: Use the functions in ejabberd auth to add and remove users.

-module(ejabberd_auth).
-author('alexey@process-one.net').

%% External exports
-export([start/0,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 is_user_exists_in_other_modules/3,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/1,
	 ctl_process_get_registered/3
	]).

-export([auth_modules/1]).

-include("ejabberd.hrl").
-include("ejabberd_ctl.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    lists:foreach(
      fun(Host) ->
	      lists:foreach(
		fun(M) ->
			M:start(Host)
		end, auth_modules(Host))
      end, ?MYHOSTS).

plain_password_required(Server) ->
    lists:any(
      fun(M) ->
	      M:plain_password_required()
      end, auth_modules(Server)).

check_password(User, Server, Password) ->
    lists:any(
      fun(M) ->
	      M:check_password(User, Server, Password)
      end, auth_modules(Server)).

check_password(User, Server, Password, StreamID, Digest) ->
    lists:any(
      fun(M) ->
	      M:check_password(User, Server, Password, StreamID, Digest)
      end, auth_modules(Server)).

%% We do not allow empty password:
set_password(_User, _Server, "") ->
    {error, not_allowed};
set_password(User, Server, Password) ->
    lists:foldl(
      fun(M, {error, _}) ->
	      M:set_password(User, Server, Password);
	 (_M, Res) ->
	      Res
      end, {error, not_allowed}, auth_modules(Server)).

%% We do not allow empty password:
try_register(_User, _Server, "") ->
    {error, not_allowed};    
try_register(User, Server, Password) ->
    case is_user_exists(User,Server) of
	true ->
	    {atomic, exists};
	false ->
	    case lists:member(jlib:nameprep(Server), ?MYHOSTS) of
		true ->
		    lists:foldl(
		      fun(_M, {atomic, ok} = Res) ->
			      Res;
			 (M, _) ->
			      M:try_register(User, Server, Password)
		      end, {error, not_allowed}, auth_modules(Server));
		false ->
		    {error, not_allowed}
	    end
    end.

%% Registered users list do not include anonymous users logged
dirty_get_registered_users() ->
    lists:flatmap(
      fun(M) ->
	      M:dirty_get_registered_users()
      end, auth_modules()).

%% Registered users list do not include anonymous users logged
get_vh_registered_users(Server) ->
    lists:flatmap(
      fun(M) ->
	      M:get_vh_registered_users(Server)
      end, auth_modules(Server)).

get_vh_registered_users(Server, Opts) ->
    lists:flatmap(
      fun(M) ->
	      M:get_vh_registered_users(Server, Opts)
      end, auth_modules(Server)).

get_vh_registered_users_number(Server) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 1) of
		    true ->
			M:get_vh_registered_users_number(Server);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

get_vh_registered_users_number(Server, Opts) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 2) of
		    true ->
			M:get_vh_registered_users_number(Server, Opts);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

get_password(User, Server) ->
    lists:foldl(
      fun(M, false) ->
	      M:get_password(User, Server);
	 (_M, Password) ->
	      Password
      end, false, auth_modules(Server)).

get_password_s(User, Server) ->
    case get_password(User, Server) of
	false ->
	    "";
	Password ->
	    Password
    end.

%% Returns true if the user exists in the DB or if an anonymous user is logged
%% under the given name
is_user_exists(User, Server) ->
    lists:any(
      fun(M) ->
	      M:is_user_exists(User, Server)
      end, auth_modules(Server)).

%% Check if the user exists in all authentications module except the module
%% passed as parameter
is_user_exists_in_other_modules(Module, User, Server) ->
    lists:any(
      fun(M) ->
	      M:is_user_exists(User, Server)
      end, auth_modules(Server)--[Module]).

remove_user(User, Server) ->
    lists:foreach(
      fun(M) ->
	      M:remove_user(User, Server)
      end, auth_modules(Server)).

remove_user(User, Server, Password) ->
    lists:foreach(
      fun(M) ->
	      M:remove_user(User, Server, Password)
      end, auth_modules(Server)).

ctl_process_get_registered(_Val, Host, ["registered-users"]) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    NewLine = io_lib:format("~n", []),
    SUsers = lists:sort(Users),
    FUsers = lists:map(fun({U, _S}) -> [U, NewLine] end, SUsers),
    io:format("~s", [FUsers]),
    {stop, ?STATUS_SUCCESS};
ctl_process_get_registered(Val, _Host, _Args) ->
    Val.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
%% Return the lists of all the auth modules actually used in the
%% configuration
auth_modules() ->
    lists:usort(
      lists:flatmap(
	fun(Server) ->
		auth_modules(Server)
	end, ?MYHOSTS)).

%% Return the list of authenticated modules for a given host
auth_modules(Server) ->
    LServer = jlib:nameprep(Server),
    Method = ejabberd_config:get_local_option({auth_method, LServer}),
    Methods = if
		  Method == undefined -> [];
		  is_list(Method) -> Method;
		  is_atom(Method) -> [Method]
	      end,
    [list_to_atom("ejabberd_auth_" ++ atom_to_list(M)) || M <- Methods].

%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth_odbc).
-author('alexey@process-one.net').

%% External exports
-export([start/1,
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
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    ejabberd_ctl:register_commands(
      Host,
      [{"registered-users", "list all registered users"}],
      ejabberd_auth, ctl_process_get_registered),
    ok.

plain_password_required() ->
    false.

check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    case catch odbc_queries:get_password(LServer, Username) of
		{selected, ["password"], [{Password}]} ->
		    Password /= "";
		_ ->
		    false
	    end
    end.

check_password(User, Server, Password, StreamID, Digest) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    case catch odbc_queries:get_password(LServer, Username) of
		{selected, ["password"], [{Passwd}]} ->
		    DigRes = if
				 Digest /= "" ->
				     Digest == sha:sha(StreamID ++ Passwd);
				 true ->
				     false
			     end,
		    if DigRes ->
			    true;
		       true ->
			    (Passwd == Password) and (Password /= "")
		    end;
		_ ->
		    false
	    end
    end.

set_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    {error, invalid_jid};
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    Pass = ejabberd_odbc:escape(Password),
	    LServer = jlib:nameprep(Server),
	    catch odbc_queries:set_password_t(LServer, Username, Pass)
    end.


try_register(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    {error, invalid_jid};
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    Pass = ejabberd_odbc:escape(Password),
	    LServer = jlib:nameprep(Server),
	    case catch odbc_queries:add_user(LServer, Username, Pass) of
		{updated, 1} ->
		    {atomic, ok};
		_ ->
		    {atomic, exists}
	    end
    end.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(
      fun(Server) ->
	      get_vh_registered_users(Server)
      end, Servers).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer, Opts) of
	{selected, ["username"], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_ ->
	    0
    end.

get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_Other ->
	    0
    end.

get_password(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    case catch odbc_queries:get_password(LServer, Username) of
		{selected, ["password"], [{Password}]} ->
		    Password;
		_ ->
		    false
	    end
    end.

get_password_s(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    "";
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    case catch odbc_queries:get_password(LServer, Username) of
		{selected, ["password"], [{Password}]} ->
		    Password;
		_ ->
		    ""
	    end
    end.

is_user_exists(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    case catch odbc_queries:get_password(LServer, Username) of
		{selected, ["password"], [{_Password}]} ->
		    true;
		_ ->
		    false
	    end
    end.

remove_user(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    error;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    catch odbc_queries:del_user(LServer, Username),
	    ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
			       [User, Server])
    end.

remove_user(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    error;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    Pass = ejabberd_odbc:escape(Password),
	    LServer = jlib:nameprep(Server),
	    F = fun() ->
			Result = odbc_queries:del_user_return_password(
				   LServer, Username, Pass),
			case Result of
			    {selected, ["password"], [{Password}]} ->
				ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
						   [User, Server]),
				ok;
			    {selected, ["password"], []} ->
				not_exists;
			    _ ->
				not_allowed
			end
		end,
	    {atomic, Result} = odbc_queries:sql_transaction(LServer, F),
	    Result
    end.

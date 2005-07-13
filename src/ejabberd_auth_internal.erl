%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Authentification via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth_internal).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-record(passwd, {us, password}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    mnesia:create_table(passwd, [{disc_copies, [node()]},
				 {attributes, record_info(fields, passwd)}]),
    update_table(),
    ok.

plain_password_required() ->
    false.

check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
	[#passwd{password = Password}] ->
	    true;
	_ ->
	    false
    end.

check_password(User, Server, Password, StreamID, Digest) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
	[#passwd{password = Passwd}] ->
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
    end.

set_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			mnesia:write(#passwd{us = US,
					     password = Password})
		end,
	    mnesia:transaction(F)
    end.


try_register(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			case mnesia:read({passwd, US}) of
			    [] ->
				mnesia:write(#passwd{us = US,
						     password = Password}),
				ok;
			    [_E] ->
				exists
			end
		end,
	    mnesia:transaction(F)
    end.

dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(
      passwd,
      [{#passwd{us = '$1', _ = '_'},
	[{'==', {element, 2, '$1'}, LServer}],
	['$1']}]).

get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
	[#passwd{password = Password}] ->
	    Password;
	_ ->
	    false
    end.

get_password_s(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
	[#passwd{password = Password}] ->
	    Password;
	_ ->
	    []
    end.

is_user_exists(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
	[] ->
	    false;
	[_] ->
	    true;
	_ ->
	    false
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({passwd, US})
        end,
    mnesia:transaction(F),
    ejabberd_hooks:run(remove_user, LServer, [User, Server]).

remove_user(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		case mnesia:read({passwd, US}) of
		    [#passwd{password = Password}] ->
			mnesia:delete({passwd, US}),
			ok;
		    [_] ->
			not_allowed;
		    _ ->
			not_exists
		end
        end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ejabberd_hooks:run(remove_user, LServer, [User, Server]),
	    ok;
	{atomic, Res} ->
	    Res;
	_ ->
	    bad_request
    end.


update_table() ->
    Fields = record_info(fields, passwd),
    case mnesia:table_info(passwd, attributes) of
	Fields ->
	    ok;
	[user, password] ->
	    ?INFO_MSG("Converting passwd table from "
		      "{user, password} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     ejabberd_auth_internal_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, passwd},
			      {attributes, record_info(fields, passwd)}]),
	    mnesia:transform_table(passwd, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(ejabberd_auth_internal_tmp_table),
			 mnesia:foldl(
			   fun(#passwd{us = U} = R, _) ->
				   mnesia:dirty_write(
				     ejabberd_auth_internal_tmp_table,
				     R#passwd{us = {U, Host}})
			   end, ok, passwd)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(passwd),
	    F2 = fun() ->
			 mnesia:write_lock_table(passwd),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, ejabberd_auth_internal_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(ejabberd_auth_internal_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating passwd table", []),
	    mnesia:transform_table(passwd, ignore, Fields)
    end.




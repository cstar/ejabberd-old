%%%-------------------------------------------------------------------
%%% File    : ejabberd_admin.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Administrative functions and commands
%%% Created :  7 May 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(ejabberd_admin).
-author('mickael.remond@process-one.net').

-export([start/0, stop/0,
	 %% Server
	 status/0, reopen_log/0,
	 %% Accounts
	 register/3, unregister/2,
	 registered_users/1,
	 %% Migration
	 import_file/1, import_dir/1,
	 %% Purge DB
	 delete_expired_messages/0, delete_old_messages/1,
	 %% Mnesia
	 backup_mnesia/1, restore_mnesia/1,
	 dump_mnesia/1, load_mnesia/1,
	 install_fallback_mnesia/1,
	 dump_to_textfile/1,
	 restore/1 % Still used by some modules
	]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").

start() ->
    ejabberd_commands:register_commands(commands()).

stop() ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% ejabberd commands
%%%

commands() ->
    [
     %% The commands status, stop and restart are implemented also in ejabberd_ctl
     %% They are defined here so that other interfaces can use them too
     #ejabberd_commands{name = status, tags = [server],
			desc = "Get status of the ejabberd server",
			module = ?MODULE, function = status,
			args = [], result = {res, restuple}},
     #ejabberd_commands{name = stop, tags = [server],
			desc = "Stop ejabberd gracefully",
			module = init, function = stop,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = restart, tags = [server],
			desc = "Restart ejabberd gracefully",
			module = init, function = restart,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = reopen_log, tags = [logs, server],
			desc = "Reopen the log files",
			module = ?MODULE, function = reopen_log,
			args = [], result = {res, rescode}},

     #ejabberd_commands{name = register, tags = [accounts],
			desc = "Register a user",
			module = ?MODULE, function = register,
			args = [{user, string}, {host, string}, {password, string}],
			result = {res, restuple}},
     #ejabberd_commands{name = unregister, tags = [accounts],
			desc = "Unregister a user",
			module = ?MODULE, function = unregister,
			args = [{user, string}, {host, string}],
			result = {res, restuple}},
     #ejabberd_commands{name = registered_users, tags = [accounts],
			desc = "List all registered users in HOST",
			module = ?MODULE, function = registered_users,
			args = [{host, string}],
			result = {users, {list, {username, string}}}},

     #ejabberd_commands{name = import_file, tags = [mnesia],
			desc = "Import user data from jabberd-1.4 spool file",
			module = ?MODULE, function = import_file,
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = import_dir, tags = [mnesia],
			desc = "Import user data from jabberd-1.4 spool dir",
			module = ?MODULE, function = import_dir,
			args = [{file, string}],
			result = {res, restuple}},

     #ejabberd_commands{name = delete_expired_messages, tags = [purge],
			desc = "Delete expired offline messages from database",
			module = ?MODULE, function = delete_expired_messages,
			args = [], result = {res, rescode}},
     #ejabberd_commands{name = delete_old_messages, tags = [purge],
			desc = "Delete offline messages older than DAYS",
			module = ?MODULE, function = delete_old_messages,
			args = [{days, integer}], result = {res, rescode}},

     #ejabberd_commands{name = backup, tags = [mnesia],
			desc = "Store the database to backup file",
			module = ?MODULE, function = backup_mnesia,
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = restore, tags = [mnesia],
			desc = "Restore the database from backup file",
			module = ?MODULE, function = restore_mnesia,
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump, tags = [mnesia],
			desc = "Dump the database to text file",
			module = ?MODULE, function = dump_mnesia,
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = load, tags = [mnesia],
			desc = "Restore the database from text file",
			module = ?MODULE, function = load_mnesia,
			args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = install_fallback, tags = [mnesia],
			desc = "Install the database from a fallback file",
			module = ?MODULE, function = install_fallback_mnesia,
			args = [{file, string}], result = {res, restuple}}
    ].


%%%
%%% Server management
%%%

status() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    String1 = io_lib:format("The node ~p is ~p. Status: ~p",
			    [node(), InternalStatus, ProvidedStatus]),
    {Is_running, String2} =
	case lists:keysearch(ejabberd, 1, application:which_applications()) of
	    false ->
		{ejabberd_not_running, "ejabberd is not running in that node."};
	    {value, {_, _, Version}} ->
		{ok, io_lib:format("ejabberd ~s is running in that node", [Version])}
	end,
    {Is_running, String1 ++ String2}.

reopen_log() ->
    ejabberd_hooks:run(reopen_log_hook, []),
    %% TODO: Use the Reopen log API for logger_h ?
    ejabberd_logger_h:reopen_log(),
    ok.


%%%
%%% Account management
%%%

register(User, Host, Password) ->
    case ejabberd_auth:try_register(User, Host, Password) of
	{atomic, ok} ->
	    {ok, io_lib:format("User ~s@~s succesfully registered", [User, Host])};
	{atomic, exists} ->
	    String = io_lib:format("User ~s@~s already registered at node ~p",
				   [User, Host, node()]),
	    {exists, String};
	{error, Reason} ->
	    String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
				   [User, Host, node(), Reason]),
	    {cannot_register, String}
    end.

unregister(User, Host) ->
    ejabberd_auth:remove_user(User, Host),
    {ok, ""}.

registered_users(Host) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort(Users),
    lists:map(fun({U, _S}) -> U end, SUsers).


%%%
%%% Migration management
%%%

import_file(Path) ->
    case jd2ejd:import_file(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't import jabberd 1.4 spool file ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_import_file, String}
    end.

import_dir(Path) ->
    case jd2ejd:import_dir(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't import jabberd 1.4 spool dir ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_import_dir, String}
    end.


%%%
%%% Purge DB
%%%

delete_expired_messages() ->
    {atomic, ok} = mod_offline:remove_expired_messages(),
    ok.

delete_old_messages(Days) ->
    {atomic, _} = mod_offline:remove_old_messages(Days),
    ok.


%%%
%%% Mnesia management
%%%

backup_mnesia(Path) ->
    case mnesia:backup(Path) of
        ok ->
	    {ok, ""};
	{error, Reason} ->
	    String = io_lib:format("Can't store backup in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_backup, String}
    end.

restore_mnesia(Path) ->
    case ejabberd_admin:restore(Path) of
	{atomic, _} ->
	    {ok, ""};
	{error, Reason} ->
	    String = io_lib:format("Can't restore backup from ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_restore, String};
	{aborted,{no_exists,Table}} ->
	    String = io_lib:format("Can't restore backup from ~p at node ~p: Table ~p does not exist.",
				   [filename:absname(Path), node(), Table]),
	    {table_not_exists, String};
	{aborted,enoent} ->
	    String = io_lib:format("Can't restore backup from ~p at node ~p: File not found.",
				   [filename:absname(Path), node()]),
	    {file_not_found, String}
    end.

%% Mnesia database restore
%% This function is called from ejabberd_ctl, ejabberd_web_admin and
%% mod_configure/adhoc 
restore(Path) ->
    mnesia:restore(Path, [{keep_tables,keep_tables()},
			  {default_op, skip_tables}]).

%% This function return a list of tables that should be kept from a previous
%% version backup.
%% Obsolete tables or tables created by module who are no longer used are not
%% restored and are ignored.
keep_tables() ->
    lists:flatten([acl, passwd, config, local_config, disco_publish,
		   keep_modules_tables()]).

%% Returns the list of modules tables in use, according to the list of actually
%% loaded modules
keep_modules_tables() ->		      
    lists:map(fun(Module) -> module_tables(Module) end,
	      gen_mod:loaded_modules(?MYNAME)).

%% TODO: This mapping should probably be moved to a callback function in each
%% module.
%% Mapping between modules and their tables
module_tables(mod_announce) -> [motd, motd_users];
module_tables(mod_irc) -> [irc_custom];
module_tables(mod_last) -> [last_activity];
module_tables(mod_muc) -> [muc_room, muc_registered];
module_tables(mod_offline) -> [offline_msg];
module_tables(mod_privacy) -> [privacy];
module_tables(mod_private) -> [private_storage];
module_tables(mod_pubsub) -> [pubsub_node];
module_tables(mod_roster) -> [roster];
module_tables(mod_shared_roster) -> [sr_group, sr_user];
module_tables(mod_vcard) -> [vcard, vcard_search];
module_tables(_Other) -> [].

dump_mnesia(Path) ->
    case dump_to_textfile(Path) of
	ok ->
	    {ok, ""};
	{error, Reason} ->
            String = io_lib:format("Can't store dump in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_dump, String}
    end.

dump_to_textfile(File) ->
    dump_to_textfile(mnesia:system_info(is_running), file:open(File, [write])).
dump_to_textfile(yes, {ok, F}) ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs = lists:filter(
	     fun(T) ->
		     case mnesia:table_info(T, storage_type) of
			 disc_copies -> true;
			 disc_only_copies -> true;
			 _ -> false
		     end
	     end, Tabs1),
    Defs = lists:map(
	     fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
			    {attributes, mnesia:table_info(T, attributes)}]}
	     end,
	     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_, {ok, F}) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, {error, Reason}) ->
    {error, Reason}.

dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
		     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).

load_mnesia(Path) ->
    case mnesia:load_textfile(Path) of
        {atomic, ok} ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't load dump in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_load, String}
    end.

install_fallback_mnesia(Path) ->
    case mnesia:install_fallback(Path) of
	ok ->
	    {ok, ""};
	{error, Reason} ->
	    String = io_lib:format("Can't install fallback from ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_fallback, String}
    end.


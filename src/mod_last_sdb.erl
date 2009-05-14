%%%----------------------------------------------------------------------
%%% File    : mod_last_sdb.erl
%%% Author  : Eric Cestari <eric@ohmforce.com>
%%% Purpose : jabber:iq:last support (JEP-0012)
%%% Created : 26/02/09 by Eric Cestari <eric@ohmforce.com>
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

-module(mod_last_sdb).
-author('eric@ohmforce.com').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq/3,
	 process_sm_iq/3,
	 on_presence_update/4,
	 store_last_info/4,
	 get_last_info/2,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-define(DOMAIN, "last_activity").

start(Host, Opts) ->
    erlsdb:start(),
    {ok, Domains, _Token}  = erlsdb:list_domains(),
    case lists:member(?DOMAIN, Domains) of 
        false ->
            erlsdb:create_domain(?DOMAIN),
            ?INFO_MSG("SimpleDB domain ~s created", [?DOMAIN]);
        true -> ok
    end,
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_LAST,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_LAST,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host,
		       ?MODULE, on_presence_update, 50).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_LAST).

%%%
%%% Uptime of ejabberd node
%%%

process_local_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Sec = get_node_uptime(),
	    IQ#iq{type = result,
		  sub_el =  [{xmlelement, "query",
			      [{"xmlns", ?NS_LAST},
			       {"seconds", integer_to_list(Sec)}],
			      []}]}
    end.

%% @spec () -> integer()
%% @doc Get the uptime of the ejabberd node, expressed in seconds.
%% When ejabberd is starting, ejabberd_config:start/0 stores the datetime.
get_node_uptime() ->
    case ejabberd_config:get_local_option(node_start) of
	{_, _, _} = StartNow ->
	    now_to_seconds(now()) - now_to_seconds(StartNow);
	_undefined ->
	    trunc(element(1, erlang:statistics(wall_clock))/1000)
    end.

now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.


%%%
%%% Serve queries about user last online
%%%

process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    User = To#jid.luser,
	    Server = To#jid.lserver,
	    {Subscription, _Groups} =
		ejabberd_hooks:run_fold(
		  roster_get_jid_info, Server,
		  {none, []}, [User, Server, From]),
	    if
		(Subscription == both) or (Subscription == from) ->
		    UserListRecord = ejabberd_hooks:run_fold(
				       privacy_get_user_list, Server,
				       #userlist{},
				       [User, Server]),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, Server,
			   allow,
			   [User, Server, UserListRecord,
			    {From, To,
			     {xmlelement, "presence", [], []}},
			    out]) of
			allow ->
			    get_last(IQ, SubEl, User, Server);
			deny ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		    end;
		true ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	    end
    end.

get_last(IQ, SubEl, LUser, LServer) ->
    case get_last_info(LUser, LServer) of
	not_found ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
	 {ok, TimeStamp, Status}->
	    TimeStamp2 = now_to_seconds(now()),
	    Sec = TimeStamp2 - TimeStamp,
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_LAST},
			      {"seconds", integer_to_list(Sec)}],
			     [{xmlcdata, Status}]}]}
    end.



on_presence_update(User, Server, _Resource, Status) ->
    TimeStamp = now_to_seconds(now()),
    store_last_info(User, Server, TimeStamp, Status).

store_last_info(User, Server, TimeStamp, Status) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    JID = LUser ++ "@" ++ LServer,
    catch erlsdb:replace_attributes(?DOMAIN, JID, [{"timestamp", TimeStamp}, {"status", Status}]).

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, Timestamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) ->
    JID = LUser ++ "@" ++ LServer,
    case catch erlsdb:get_attributes(?DOMAIN, JID) of
	{'EXIT', _Reason} ->
	    not_found;
	{ok, []} ->
	    not_found;
	{ok,Activity} ->
	    {ok, proplists:get_value("timestamp", Activity), proplists:get_value("status", Activity)}
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
   JID = LUser ++ "@" ++LServer,
   catch erlsdb:delete_attributes(?DOMAIN, JID, ["timestamp", "status"]).



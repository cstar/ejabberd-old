%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sm).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start_link/0, init/0, open_session/2, close_session/2,
	 get_user_resources/1,
	 set_presence/3,
	 unset_presence/2,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 register_iq_handler/3,
	 register_iq_handler/4,
	 unregister_iq_handler/1
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(session, {ur, user, pid}).
-record(presence, {ur, user, priority}).

start_link() ->
    Pid = proc_lib:spawn_link(ejabberd_sm, init, []),
    register(ejabberd_sm, Pid),
    {ok, Pid}.

init() ->
    update_tables(),
    mnesia:create_table(session, [{ram_copies, [node()]},
				  {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, user),
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:create_table(presence,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, presence)}]),
    mnesia:add_table_index(presence, user),
    mnesia:subscribe(system),
    ets:new(sm_iqtable, [named_table]),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p~nwhen processing: ~p",
			       [Reason, {From, To, Packet}]);
		_ ->
		    ok
	    end,
	    loop();
	{open_session, User, Resource, From} ->
	    register_connection(User, Resource, From),
	    loop();
	{close_session, User, Resource} ->
	    remove_connection(User, Resource),
	    loop();
	{mnesia_system_event, {mnesia_down, Node}} ->
	    clean_table_from_bad_node(Node),
	    loop();
	{register_iq_handler, XMLNS, Module, Function} ->
	    ets:insert(sm_iqtable, {XMLNS, Module, Function}),
	    loop();
	{register_iq_handler, XMLNS, Module, Function, Opts} ->
	    ets:insert(sm_iqtable, {XMLNS, Module, Function, Opts}),
	    loop();
	{unregister_iq_handler, XMLNS} ->
	    case ets:lookup(sm_iqtable, XMLNS) of
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
		_ ->
		    ok
	    end,
	    ets:delete(sm_iqtable, XMLNS),
	    mod_disco:unregister_feature(XMLNS),
	    loop();
	_ ->
	    loop()
    end.


open_session(User, Resource) ->
    ejabberd_sm ! {open_session, User, Resource, self()}.

close_session(User, Resource) ->
    ejabberd_sm ! {close_session, User, Resource}.


register_connection(User, Resource, Pid) ->
    LUser = jlib:nodeprep(User),
    LResource = jlib:resourceprep(Resource),
    UR = {LUser, LResource},
    F = fun() ->
		Ss = mnesia:wread({session, UR}),
		mnesia:write(#session{ur = UR, user = LUser, pid = Pid}),
		Ss
        end,
    case mnesia:transaction(F) of
	{atomic, Ss} ->
	    lists:foreach(
	      fun(R) ->
		      R#session.pid ! replaced
	      end, Ss);
	_ ->
	    false
    end.


remove_connection(User, Resource) ->
    LUser = jlib:nodeprep(User),
    LResource = jlib:resourceprep(Resource),
    UR = {LUser, LResource},
    F = fun() ->
		mnesia:delete({session, UR})
        end,
    mnesia:transaction(F).


clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       session,
		       [{#session{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
        end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    #jid{user = User,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
    {xmlelement, Name, Attrs, _Els} = Packet,
    case LResource of
	"" ->
	    case Name of
		"presence" ->
		    FromU = jlib:jid_replace_resource(From, ""),
		    {Pass, Subsc} =
			case xml:get_attr_s("type", Attrs) of
			    "subscribe" ->
				{mod_roster:in_subscription(User,
							    FromU,
							    subscribe),
				 true};
			    "subscribed" ->
				{mod_roster:in_subscription(User,
							    FromU,
							    subscribed),
				 true};
			    "unsubscribe" ->
				{mod_roster:in_subscription(User,
							    FromU,
							    unsubscribe),
				 true};
			    "unsubscribed" ->
				{mod_roster:in_subscription(User,
							    FromU,
							    unsubscribed),
				 true};
			    _ ->
				{true, false}
			end,
		    if Pass ->
			    LFrom = jlib:jid_tolower(From),
			    Resources = get_user_resources(User),
			    if
				Resources /= [] ->
				    lists:foreach(
				      fun(R) ->
					      if LFrom /=
						 {LUser, LServer, R} ->
						      ejabberd_sm !
							  {route,
							   From,
							   jlib:jid_replace_resource(To, R),
							   Packet};
						 true ->
						      ok
					      end
				      end, Resources);
				true ->
				    if
					Subsc ->
					    catch mod_offline:store_packet(
						    From, To, Packet);
					true ->
					    ok
				    end
			    end;
		       true ->
			    ok
		    end;
		"message" ->
		    route_message(From, To, Packet);
		"iq" ->
		    process_iq(From, To, Packet);
		"broadcast" ->
		    lists:foreach(
		      fun(R) ->
			      ejabberd_sm ! {route,
					     From,
					     jlib:jid_replace_resource(To, R),
					     Packet}
		      end, get_user_resources(User));
		_ ->
		    ok
	    end;
	_ ->
	    LUR = {LUser, LResource},
	    case mnesia:dirty_read({session, LUR}) of
		[] ->
		    case Name of
			"message" ->
			    route_message(From, To, Packet);
			"iq" ->
			    case xml:get_attr_s("type", Attrs) of
				"error" -> ok;
				"result" -> ok;
				_ ->
				    Err =
					jlib:make_error_reply(
					  Packet, ?ERR_RECIPIENT_UNAVAILABLE),
				    ejabberd_router:route(To, From, Err)
			    end;
			_ ->
			    ?DEBUG("packet droped~n", [])
		    end;
		[Sess] ->
		    Pid = Sess#session.pid,
		    ?DEBUG("sending to process ~p~n", [Pid]),
		    Pid ! {route, From, To, Packet}
	    end
    end.

route_message(From, To, Packet) ->
    #jid{luser = LUser} = To,
    case catch lists:max(get_user_present_resources(LUser)) of
	{'EXIT', _} ->
	    case xml:get_tag_attr_s("type", Packet) of
		"error" ->
		    ok;
		_ ->
		    case ejabberd_auth:is_user_exists(LUser) of
			true ->
			    case catch mod_offline:store_packet(
					 From, To, Packet) of
				{'EXIT', _} ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_SERVICE_UNAVAILABLE),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    ok
			    end;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_ITEM_NOT_FOUND),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
	{_, R} ->
	    ejabberd_sm ! {route,
			   From,
			   jlib:jid_replace_resource(To, R),
			   Packet}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_resources(User) ->
    LUser = jlib:nodeprep(User),
    case catch mnesia:dirty_index_read(session, LUser, #session.user) of
	{'EXIT', _Reason} ->
	    [];
	Rs ->
	    lists:map(fun(R) ->
			      element(2, R#session.ur)
		      end, Rs)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_presence(User, Resource, Priority) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		UR = {User, Resource},
		mnesia:write(#presence{ur = UR, user = LUser,
				       priority = Priority})
	end,
    mnesia:transaction(F).

unset_presence(User, Resource) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		UR = {User, Resource},
		mnesia:delete({presence, UR})
	end,
    mnesia:transaction(F),
    catch mod_last:on_presence_update(LUser).

get_user_present_resources(LUser) ->
    case catch mnesia:dirty_index_read(presence, LUser, #presence.user) of
	{'EXIT', _Reason} ->
	    [];
	Rs ->
	    lists:map(fun(R) ->
			      {R#presence.priority, element(2, R#presence.ur)}
		      end, Rs)
    end.

dirty_get_sessions_list() ->
    mnesia:dirty_all_keys(session).

dirty_get_my_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{pid = '$1', _ = '_'},
	[{'==', {node, '$1'}, node()}],
	['$_']}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	#iq{xmlns = XMLNS} ->
	    case ets:lookup(sm_iqtable, XMLNS) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ),
		    if
			ResIQ /= ignore ->
			    ejabberd_router ! {route,
					       To,
					       From,
					       jlib:iq_to_xml(ResIQ)};
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(Module, Function, Opts,
					  From, To, IQ);
		[] ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
		    ejabberd_router ! {route, To, From, Err}
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.

register_iq_handler(XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, XMLNS, Module, Fun}.

register_iq_handler(XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, XMLNS}.



update_tables() ->
    case catch mnesia:table_info(session, attributes) of
	[ur, user, node] ->
	    mnesia:delete_table(session);
	[ur, user, pid] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.


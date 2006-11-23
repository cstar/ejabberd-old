%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sm).
-author('alexey@sevcom.net').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
	 open_session/4, close_session/4,
	 bounce_offline_message/3,
	 disconnect_removed_user/2,
	 get_user_resources/2,
	 set_presence/5,
	 unset_presence/5,
	 close_session_unset_presence/5,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 register_iq_handler/4,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 ctl_process/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_ctl.hrl").

-record(session, {sid, usr, us, priority}).
-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.

open_session(SID, User, Server, Resource) ->
    set_session(SID, User, Server, Resource, undefined),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
		       [SID, JID]).

close_session(SID, User, Server, Resource) ->
    F = fun() ->
		mnesia:delete({session, SID})
        end,
    mnesia:sync_dirty(F),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
		       [SID, JID]).

bounce_offline_message(From, To, Packet) ->
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err),
    stop.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jlib:make_jid("", "", ""),
		      jlib:make_jid(User, Server, ""),
		      {xmlelement, "broadcast", [],
		       [{exit, "User removed"}]}).

get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	Ss ->
	    [element(3, S#session.usr) || S <- clean_session_list(Ss)]
    end.

set_presence(SID, User, Server, Resource, Priority) ->
    set_session(SID, User, Server, Resource, Priority).

unset_presence(SID, User, Server, Resource, Status) ->
    set_session(SID, User, Server, Resource, undefined),
    ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

close_session_unset_presence(SID, User, Server, Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
		       [User, Server, Resource, Status]).


dirty_get_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{usr = '$1', _ = '_'},
	[],
	['$1']}]).

dirty_get_my_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{sid = {'_', '$1'}, _ = '_'},
	[{'==', {node, '$1'}, node()}],
	['$_']}]).

get_vh_session_list(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(
      session,
      [{#session{usr = '$1', _ = '_'},
	[{'==', {element, 2, '$1'}, LServer}],
	['$1']}]).

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun}.

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:subscribe(system),
    ets:new(sm_iqtable, [named_table]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(offline_message_hook, Host,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, Host,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_ctl:register_commands(
      [{"connected-users", "list all established sessions"},
       {"connected-users-number", "print a number of established sessions"},
       {"user-resources user server", "print user's connected resources"}],
      ?MODULE, ctl_process),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
	[{_, Module, Function, Opts}] ->
	    gen_iq_handler:stop_iq_handler(Module, Function, Opts);
	_ ->
	    ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_session(SID, User, Server, Resource, Priority) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    F = fun() ->
		mnesia:write(#session{sid = SID,
				      usr = USR,
				      us = US,
				      priority = Priority})
	end,
    mnesia:sync_dirty(F).

clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       session,
		       [{#session{sid = {'_', '$1'}, _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete({session, E#session.sid})
			      end, Es)
        end,
    mnesia:sync_dirty(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
    {xmlelement, Name, Attrs, _Els} = Packet,
    case LResource of
	"" ->
	    case Name of
		"presence" ->
		    {Pass, Subsc} =
			case xml:get_attr_s("type", Attrs) of
			    "subscribe" ->
				Reason = xml:get_path_s(
					   Packet,
					   [{elem, "status"}, cdata]),
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   LServer,
				   false,
				   [User, Server, From, subscribe, Reason]),
				 true};
			    "subscribed" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   LServer,
				   false,
				   [User, Server, From, subscribed, ""]),
				 true};
			    "unsubscribe" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   LServer,
				   false,
				   [User, Server, From, unsubscribe, ""]),
				 true};
			    "unsubscribed" ->
				{ejabberd_hooks:run_fold(
				   roster_in_subscription,
				   LServer,
				   false,
				   [User, Server, From, unsubscribed, ""]),
				 true};
			    _ ->
				{true, false}
			end,
		    if Pass ->
			    LFrom = jlib:jid_tolower(From),
			    PResources = get_user_present_resources(
					   LUser, LServer),
			    lists:foreach(
			      fun({_, R}) ->
				      if LFrom /=
					 {LUser, LServer, R} ->
					      do_route(
						From,
						jlib:jid_replace_resource(To, R),
						Packet);
					 true ->
					      ok
				      end
			      end, PResources);
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
			      do_route(From,
				       jlib:jid_replace_resource(To, R),
				       Packet)
		      end, get_user_resources(User, Server));
		_ ->
		    ok
	    end;
	_ ->
	    USR = {LUser, LServer, LResource},
	    case mnesia:dirty_index_read(session, USR, #session.usr) of
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
		Ss ->
		    Session = lists:max(Ss),
		    Pid = element(2, Session#session.sid),
		    ?DEBUG("sending to process ~p~n", [Pid]),
		    Pid ! {route, From, To, Packet}
	    end
    end.

route_message(From, To, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioRes = get_user_present_resources(LUser, LServer),
    case catch lists:max(PrioRes) of
	{Priority, _R} when is_integer(Priority), Priority >= 0 ->
	    lists:foreach(
	      %% Route messages to all priority that equals the max, if
	      %% positive
	      fun({P, R}) when P == Priority ->
		      LResource = jlib:resourceprep(R),
		      USR = {LUser, LServer, LResource},
		      case mnesia:dirty_index_read(session, USR, #session.usr) of
			  [] ->
			      ok;				% Race condition
			  Ss ->
			      Session = lists:max(Ss),
			      Pid = element(2, Session#session.sid),
			      ?DEBUG("sending to process ~p~n", [Pid]),
			      Pid ! {route, From, To, Packet}
		      end;
		 %% Ignore other priority:
		 ({_Prio, _Res}) ->
		      ok
	      end,
	      PrioRes);
	_ ->
	    case xml:get_tag_attr_s("type", Packet) of
		"error" ->
		    ok;
		_ ->
		    case ejabberd_auth:is_user_exists(LUser, LServer) of
			true ->
			    ejabberd_hooks:run(offline_message_hook,
					       LServer,
					       [From, To, Packet]);
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		    end
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) ->
    Res;
clean_session_list([S], Res) ->
    [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if
	S1#session.usr == S2#session.usr ->
	    if
		S1#session.sid > S2#session.sid ->
		    clean_session_list([S1 | Rest], Res);
		true ->
		    clean_session_list([S2 | Rest], Res)
	    end;
	true ->
	    clean_session_list([S2 | Rest], [S1 | Res])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_present_resources(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	Ss ->
	    [{S#session.priority, element(3, S#session.usr)} ||
		S <- clean_session_list(Ss), is_integer(S#session.priority)]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),

    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    USR = {LUser, LServer, LResource},
    %% A connection exist with the same resource. We replace it:
    SIDs = mnesia:dirty_select(
	     session,
	     [{#session{sid = '$1', usr = USR, _ = '_'}, [], ['$1']}]),
    if
	SIDs == [] -> ok;
	true ->
	    MaxSID = lists:max(SIDs),
	    lists:foreach(
	      fun({_, Pid} = S) when S /= MaxSID ->
		      Pid ! replaced;
		 (_) -> ok
	      end, SIDs)
    end.

check_max_sessions(LUser, LServer) ->
    %% If the max number of sessions for a given is reached, we replace the
    %% first one
    SIDs =  mnesia:dirty_select(
             session,
             [{#session{sid = '$1', usr = {LUser, LServer, '_'}, _ = '_'}, [], ['$1']}]),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(SIDs) =< MaxSessions -> ok;
       true -> {_, Pid} = lists:min(SIDs),
               Pid ! replaced
    end.


%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(
	   Host, max_user_sessions, jlib:make_jid(LUser, Host, "")) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_SESSIONS
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
	#iq{xmlns = XMLNS} ->
	    Host = To#jid.lserver,
	    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ),
		    if
			ResIQ /= ignore ->
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(ResIQ));
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(Host, Module, Function, Opts,
					  From, To, IQ);
		[] ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end;
	reply ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.


ctl_process(_Val, ["connected-users"]) ->
    USRs = dirty_get_sessions_list(),
    NewLine = io_lib:format("~n", []),
    SUSRs = lists:sort(USRs),
    FUSRs = lists:map(fun({U, S, R}) -> [U, $@, S, $/, R, NewLine] end, SUSRs),
    io:format("~s", [FUSRs]),
    {stop, ?STATUS_SUCCESS};
ctl_process(_Val, ["connected-users-number"]) ->
    N = length(dirty_get_sessions_list()),
    io:format("~p~n", [N]),
    {stop, ?STATUS_SUCCESS};
ctl_process(_Val, ["user-resources", User, Server]) ->
    Resources =  get_user_resources(User, Server),
    NewLine = io_lib:format("~n", []),
    SResources = lists:sort(Resources),
    FResources = lists:map(fun(R) -> [R, NewLine] end, SResources),
    io:format("~s", [FResources]),
    {stop, ?STATUS_SUCCESS};
ctl_process(Val, _Args) ->
    Val.


update_tables() ->
    case catch mnesia:table_info(session, attributes) of
	[ur, user, node] ->
	    mnesia:delete_table(session);
	[ur, user, pid] ->
	    mnesia:delete_table(session);
	[usr, us, pid] ->
	    mnesia:delete_table(session);
	[sid, usr, us, priority] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case lists:member(presence, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(presence);
	false ->
	    ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.


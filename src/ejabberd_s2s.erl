%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : S2S connections manager
%%% Created :  7 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_s2s).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
	 have_connection/1,
	 has_key/2,
	 try_register/1,
	 remove_connection/3,
	 dirty_get_connections/0,
	 allow_host/2,
	 ctl_process/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_ctl.hrl").

-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER, 1).
-define(DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE, 1).

-record(s2s, {fromto, pid, key}).
-record(state, {}).

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

remove_connection(FromTo, Pid, Key) ->
    case catch mnesia:dirty_match_object(s2s, #s2s{fromto = FromTo,
						   pid = Pid,
						   _ = '_'}) of
	[#s2s{pid = Pid, key = Key}] ->
	    F = fun() ->
			mnesia:delete_object(#s2s{fromto = FromTo,
						  pid = Pid,
						  key = Key})
		end,
	    mnesia:transaction(F);
	_ ->
	    ok
    end.

have_connection(FromTo) ->
    case catch mnesia:dirty_read(s2s, FromTo) of
        [_] ->
            true;
        _ ->
            false
    end.

has_key(FromTo, Key) ->
    case mnesia:dirty_select(s2s,
			     [{#s2s{fromto = FromTo, key = Key, _ = '_'},
			       [],
			       ['$_']}]) of
	[] ->
	    false;
	_ ->
	    true
    end.

try_register(FromTo) ->
    Key = randoms:get_string(),
    MaxS2SConnectionsNumber = max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    F = fun() ->
		L = mnesia:read({s2s, FromTo}),
		NeededConnections = needed_connections_number(
				      L, MaxS2SConnectionsNumber,
				      MaxS2SConnectionsNumberPerNode),
		if
		    NeededConnections > 0 ->
			mnesia:write(#s2s{fromto = FromTo,
					  pid = self(),
					  key = Key}),
			{key, Key};
		    true ->
			false
		end
	end,
    case mnesia:transaction(F) of
        {atomic, Res} ->
            Res;
        _ ->
            false
    end.

dirty_get_connections() ->
    mnesia:dirty_all_keys(s2s).

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
    mnesia:create_table(s2s, [{ram_copies, [node()]}, {type, bag},
			      {attributes, record_info(fields, s2s)}]),
    mnesia:add_table_copy(s2s, node(), ram_copies),
    mnesia:subscribe(system),
    ejabberd_ctl:register_commands(
      [{"incoming-s2s-number", "print number of incoming s2s connections on the node"},
       {"outgoing-s2s-number", "print number of outgoing s2s connections on the node"}],
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
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~nwhen processing: ~p",
                       [Reason, {From, To, Packet}]);
        _ ->
            ok
    end,
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
clean_table_from_bad_node(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       s2s,
		       [{#s2s{pid = '$1', _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete_object(E)
			      end, Es)
	end,
    mnesia:transaction(F).

do_route(From, To, Packet) ->
    ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, Packet, 8]),
    case find_connection(From, To) of
	{atomic, Pid} when pid(Pid) ->
	    ?DEBUG("sending to process ~p~n", [Pid]),
	    {xmlelement, Name, Attrs, Els} = Packet,
	    NewAttrs = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						  jlib:jid_to_string(To),
						  Attrs),
	    send_element(Pid, {xmlelement, Name, NewAttrs, Els}),
	    ok;
	{aborted, _Reason} ->
	    case xml:get_tag_attr_s("type", Packet) of
		"error" -> ok;
		"result" -> ok;
		_ ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end,
	    false
    end.

find_connection(From, To) ->
    #jid{lserver = MyServer} = From,
    #jid{lserver = Server} = To,
    FromTo = {MyServer, Server},
    MaxS2SConnectionsNumber = max_s2s_connections_number(FromTo),
    MaxS2SConnectionsNumberPerNode =
	max_s2s_connections_number_per_node(FromTo),
    ?DEBUG("Finding connection for ~p~n", [FromTo]),
    case catch mnesia:dirty_read(s2s, FromTo) of
	{'EXIT', Reason} ->
	    {aborted, Reason};
	[] ->
	    %% We try to establish all the connections if the host is not a
	    %% service and if the s2s host is not blacklisted or
	    %% is in whitelist:
	    case not is_service(From, To) andalso allow_host(MyServer, Server) of
		true ->
		    NeededConnections = needed_connections_number(
					  [], MaxS2SConnectionsNumber,
					  MaxS2SConnectionsNumberPerNode),
		    open_several_connections(
		      NeededConnections, MyServer,
		      Server, From, FromTo,
		      MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode);
		false ->
		    {aborted, error}
	    end;
	L when is_list(L) ->
	    NeededConnections = needed_connections_number(
				  L, MaxS2SConnectionsNumber,
				  MaxS2SConnectionsNumberPerNode),
	    if
		NeededConnections > 0 ->
		    %% We establish the missing connections for this pair.
		    open_several_connections(
		      NeededConnections, MyServer,
		      Server, From, FromTo,
		      MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode);
		true ->
		    %% We choose a connexion from the pool of opened ones.
		    {atomic, choose_connection(From, L)}
	    end
    end.

choose_connection(From, Connections) ->
    choose_pid(From, [C#s2s.pid || C <- Connections]).

choose_pid(From, Pids) ->
    Pids1 = case [P || P <- Pids, node(P) == node()] of
		[] -> Pids;
		Ps -> Ps
	    end,
    % Use sticky connections based on the JID of the sender (whithout
    % the resource to ensure that a muc room always uses the same
    % connection)
    Pid = lists:nth(erlang:phash(jlib:jid_remove_resource(From), length(Pids1)),
		    Pids1),
    ?DEBUG("Using ejabberd_s2s_out ~p~n", [Pid]),
    Pid.

open_several_connections(N, MyServer, Server, From, FromTo,
			 MaxS2SConnectionsNumber,
			 MaxS2SConnectionsNumberPerNode) ->
    ConnectionsResult =
	[new_connection(MyServer, Server, From, FromTo,
			MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode)
	 || _N <- lists:seq(1, N)],
    case [PID || {atomic, PID} <- ConnectionsResult] of
	[] ->
	    hd(ConnectionsResult);
	PIDs ->
	    {atomic, choose_pid(From, PIDs)}
    end.

new_connection(MyServer, Server, From, FromTo,
	       MaxS2SConnectionsNumber, MaxS2SConnectionsNumberPerNode) ->
    Key = randoms:get_string(),
    {ok, Pid} = ejabberd_s2s_out:start(
		  MyServer, Server, {new, Key}),
    F = fun() ->
		L = mnesia:read({s2s, FromTo}),
		NeededConnections = needed_connections_number(
				      L, MaxS2SConnectionsNumber,
				      MaxS2SConnectionsNumberPerNode),
		if
		    NeededConnections > 0 ->
			mnesia:write(#s2s{fromto = FromTo,
					  pid = Pid,
					  key = Key}),
			?INFO_MSG("New s2s connection started ~p", [Pid]),
			Pid;
		    true ->
			choose_connection(From, L)
		end
	end,
    TRes = mnesia:transaction(F),
    case TRes of
	{atomic, Pid} ->
	    ejabberd_s2s_out:start_connection(Pid);
	_ ->
	    ejabberd_s2s_out:stop_connection(Pid)
    end,
    TRes.

max_s2s_connections_number({From, To}) ->
    case acl:match_rule(
	   From, max_s2s_connections, jlib:make_jid("", To, "")) of
	Max when is_integer(Max) -> Max;
	_ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER
    end.

max_s2s_connections_number_per_node({From, To}) ->
    case acl:match_rule(
	   From, max_s2s_connections_per_node, jlib:make_jid("", To, "")) of
	Max when is_integer(Max) -> Max;
	_ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE
    end.

needed_connections_number(Ls, MaxS2SConnectionsNumber,
			  MaxS2SConnectionsNumberPerNode) ->
    LocalLs = [L || L <- Ls, node(L#s2s.pid) == node()],
    lists:min([MaxS2SConnectionsNumber - length(Ls),
	       MaxS2SConnectionsNumberPerNode - length(LocalLs)]).

%%--------------------------------------------------------------------
%% Function: is_service(From, To) -> true | false
%% Description: Return true if the destination must be considered as a
%% service.
%% --------------------------------------------------------------------
is_service(From, To) ->
    LFromDomain = From#jid.lserver,
    case ejabberd_config:get_local_option({route_subdomains, LFromDomain}) of
        s2s -> % bypass RFC 3920 10.3
            false;
        _ ->
            LDstDomain = To#jid.lserver,
            P = fun(Domain) -> is_subdomain(LDstDomain, Domain) end,
            lists:any(P, ?MYHOSTS)
    end.

%%--------------------------------------------------------------------
%% Function: is_subdomain(Domain1, Domain2) -> true | false
%% Description: Return true if Domain1 (a string representing an
%% internet domain name) is a subdomain (or the same domain) of
%% Domain2
%% --------------------------------------------------------------------
is_subdomain(Domain1, Domain2) ->
    lists:suffix(string:tokens(Domain2, "."), string:tokens(Domain1, ".")).

send_element(Pid, El) ->
    Pid ! {send_element, El}.

ctl_process(_Val, ["incoming-s2s-number"]) ->
    N = length(supervisor:which_children(ejabberd_s2s_in_sup)),
    io:format("~p~n", [N]),
    {stop, ?STATUS_SUCCESS};
ctl_process(_Val, ["outgoing-s2s-number"]) ->
    N = length(supervisor:which_children(ejabberd_s2s_out_sup)),
    io:format("~p~n", [N]),
    {stop, ?STATUS_SUCCESS};
ctl_process(Val, _Args) ->
    Val.

update_tables() ->
    case catch mnesia:table_info(s2s, type) of
	bag ->
	    ok;
	{'EXIT', _} ->
	    ok;
	_ ->
	    % XXX TODO convert it ?
	    mnesia:delete_table(s2s)
    end,
    case catch mnesia:table_info(s2s, attributes) of
        [fromto, node, key] ->
            mnesia:transform_table(s2s, ignore, [fromto, pid, key]),
            mnesia:clear_table(s2s);
        [fromto, pid, key] ->
            ok;
        {'EXIT', _} ->
            ok
    end,
    case lists:member(local_s2s, mnesia:system_info(tables)) of
        true ->
            mnesia:delete_table(local_s2s);
        false ->
            ok
    end.

%% Check if host is in blacklist or white list
allow_host(MyServer, S2SHost) ->
    case ejabberd_config:get_local_option({{s2s_host, S2SHost},MyServer}) of
        deny -> false;
        allow -> true;
        _ ->
            case ejabberd_config:get_local_option({s2s_default_policy, MyServer}) of
                deny -> false;
                allow -> true;
                _ -> true %% The default s2s policy is allow
            end
    end.

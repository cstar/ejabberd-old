%%%----------------------------------------------------------------------
%%% File    : ejabberd_router.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Main router
%%% Created : 27 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_router).
-author('alexey@sevcom.net').

-behaviour(gen_server).

%% API
-export([route/3,
	 register_route/1,
	 register_route/2,
	 register_routes/1,
	 unregister_route/1,
	 unregister_routes/1,
	 dirty_get_all_routes/0,
	 dirty_get_all_domains/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(route, {domain, pid, local_hint}).
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

register_route(Domain) ->
    register_route(Domain, undefined).

register_route(Domain, LocalHint) ->
    case jlib:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Pid = self(),
	    case get_component_number(LDomain) of
		undefined ->
		    F = fun() ->
				mnesia:write(#route{domain = LDomain,
						    pid = Pid,
						    local_hint = LocalHint})
			end,
		    mnesia:transaction(F);
		N ->
		    F = fun() ->
				case mnesia:read({route, LDomain}) of
				    [] ->
					mnesia:write(
					  #route{domain = LDomain,
						 pid = Pid,
						 local_hint = 1}),
					lists:foreach(
					  fun(I) ->
						  mnesia:write(
						    #route{domain = LDomain,
							   pid = undefined,
							   local_hint = I})
					  end, lists:seq(2, N));
				    Rs ->
					lists:any(
					  fun(#route{pid = undefined,
						     local_hint = I} = R) ->
						  mnesia:write(
						    #route{domain = LDomain,
							   pid = Pid,
							   local_hint = I}),
						  mnesia:delete_object(R),
						  true;
					     (_) ->
						  false
					  end, Rs)
				end
			end,
		    mnesia:transaction(F)
	    end
    end.

register_routes(Domains) ->
    lists:foreach(fun(Domain) ->
			  register_route(Domain)
		  end, Domains).

unregister_route(Domain) ->
    case jlib:nameprep(Domain) of
	error ->
	    erlang:error({invalid_domain, Domain});
	LDomain ->
	    Pid = self(),
	    case get_component_number(LDomain) of
		undefined ->
		    F = fun() ->
				case mnesia:match_object(
				       #route{domain = LDomain,
					      pid = Pid,
					      _ = '_'}) of
				    [R] ->
					mnesia:delete_object(R);
				    _ ->
					ok
				end
			end,
		    mnesia:transaction(F);
		_ ->
		    F = fun() ->
				case mnesia:match(#route{domain = LDomain,
							 pid = Pid,
							 _ = '_'}) of
				    [R] ->
					I = R#route.local_hint,
					mnesia:write(
					  #route{domain = LDomain,
						 pid = undefined,
						 local_hint = I}),
					mnesia:delete_object(R);
				    _ ->
					ok
				end
			end,
		    mnesia:transaction(F)
	    end
    end.

unregister_routes(Domains) ->
    lists:foreach(fun(Domain) ->
			  unregister_route(Domain)
		  end, Domains).


dirty_get_all_routes() ->
    lists:usort(mnesia:dirty_all_keys(route)) -- ?MYHOSTS.

dirty_get_all_domains() ->
    lists:usort(mnesia:dirty_all_keys(route)).


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
    mnesia:create_table(route,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes,
			  record_info(fields, route)}]),
    mnesia:add_table_copy(route, node(), ram_copies),
    mnesia:subscribe({table, route, simple}),
    lists:foreach(
      fun(Pid) ->
	      erlang:monitor(process, Pid)
      end,
      mnesia:dirty_select(route, [{{route, '_', '$1', '_'}, [], ['$1']}])),
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
handle_info({mnesia_table_event, {write, #route{pid = Pid}, _ActivityId}},
	    State) ->
    erlang:monitor(process, Pid),
    {noreply, State};
handle_info({'DOWN', _Ref, _Type, Pid, _Info}, State) ->
    F = fun() ->
		Es = mnesia:select(
		       route,
		       [{#route{pid = Pid, _ = '_'},
			 [],
			 ['$_']}]),
		lists:foreach(
		  fun(E) ->
			  if
			      is_integer(E#route.local_hint) ->
				  LDomain = E#route.domain,
				  I = E#route.local_hint,
				  mnesia:write(
				    #route{domain = LDomain,
					   pid = undefined,
					   local_hint = I}),
				  mnesia:delete_object(E);
			      true ->
				  mnesia:delete_object(E)
			  end
		  end, Es)
	end,
    mnesia:transaction(F),
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
do_route(OrigFrom, OrigTo, OrigPacket) ->
    ?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n",
	   [OrigFrom, OrigTo, OrigPacket]),
    case ejabberd_hooks:run_fold(filter_packet,
				 {OrigFrom, OrigTo, OrigPacket}, []) of
	{From, To, Packet} ->
	    LDstDomain = To#jid.lserver,
	    case mnesia:dirty_read(route, LDstDomain) of
		[] ->
		    ejabberd_s2s:route(From, To, Packet);
		[R] ->
		    Pid = R#route.pid,
		    if
			node(Pid) == node() ->
			    case R#route.local_hint of
				{apply, Module, Function} ->
				    Module:Function(From, To, Packet);
				_ ->
				    Pid ! {route, From, To, Packet}
			    end;
			is_pid(Pid) ->
			    Pid ! {route, From, To, Packet};
			true ->
			    drop
		    end;
		Rs ->
		    Value = case ejabberd_config:get_local_option(
				   {domain_balancing, LDstDomain}) of
				undefined -> now();
				random -> now();
				source -> jlib:jid_tolower(From);
				destination -> jlib:jid_tolower(To);
				bare_source ->
				    jlib:jid_remove_resource(
				      jlib:jid_tolower(From));
				bare_destination ->
				    jlib:jid_remove_resource(
				      jlib:jid_tolower(To))
			    end,
		    case get_component_number(LDstDomain) of
			undefined ->
			    case [R || R <- Rs, node(R#route.pid) == node()] of
				[] ->
				    R = lists:nth(erlang:phash(Value, length(Rs)), Rs),
				    Pid = R#route.pid,
				    if
					is_pid(Pid) ->
					    Pid ! {route, From, To, Packet};
					true ->
					    drop
				    end;
				LRs ->
				    R = lists:nth(erlang:phash(Value, length(LRs)), LRs),
				    Pid = R#route.pid,
				    case R#route.local_hint of
					{apply, Module, Function} ->
					    Module:Function(From, To, Packet);
					_ ->
					    Pid ! {route, From, To, Packet}
				    end
			    end;
			_ ->
			    SRs = lists:ukeysort(#route.local_hint, Rs),
			    R = lists:nth(erlang:phash(Value, length(SRs)), SRs),
			    Pid = R#route.pid,
			    if
				is_pid(Pid) ->
				    Pid ! {route, From, To, Packet};
				true ->
				    drop
			    end
		    end
	    end;
	drop ->
	    ok
    end.

get_component_number(LDomain) ->
    case ejabberd_config:get_local_option(
	   {domain_balancing_component_number, LDomain}) of
	N when is_integer(N),
	       N > 1 ->
	    N;
	_ ->
	    undefined
    end.

update_tables() ->
    case catch mnesia:table_info(route, attributes) of
	[domain, node, pid] ->
	    mnesia:delete_table(route);
	[domain, pid] ->
	    mnesia:delete_table(route);
	[domain, pid, local_hint] ->
	    ok;
	{'EXIT', _} ->
	    ok
    end,
    case lists:member(local_route, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_route);
	false ->
	    ok
    end.


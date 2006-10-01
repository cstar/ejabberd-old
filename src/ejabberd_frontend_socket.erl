%%%-------------------------------------------------------------------
%%% File    : ejabberd_frontend_socket.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Frontend socket with zlib and TLS support library
%%% Created : 23 Aug 2006 by Alexey Shchepin <alex@alex.sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_frontend_socket).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start/4,
	 start_link/4,
	 %connect/3,
	 starttls/2,
	 starttls/3,
	 compress/1,
	 compress/2,
	 reset_stream/1,
	 send/2,
	 change_shaper/2,
	 get_sockmod/1,
	 get_peer_certificate/1,
	 get_verify_result/1,
	 close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sockmod, socket, receiver}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Module, SockMod, Socket, Opts) ->
    gen_server:start_link(?MODULE,
			  [Module, SockMod, Socket, Opts], []).

start(Module, SockMod, Socket, Opts) ->
    case Module:socket_type() of
	xml_stream ->
	    MaxStanzaSize =
		case lists:keysearch(max_stanza_size, 1, Opts) of
		    {value, {_, Size}} -> Size;
		    _ -> infinity
		end,
	    Receiver = ejabberd_receiver:start(Socket, SockMod, none, MaxStanzaSize),
	    case SockMod:controlling_process(Socket, Receiver) of
		ok ->
		    ok;
		{error, _Reason} ->
		    SockMod:close(Socket)
	    end,
	    gen_server:start(?MODULE,
			     [Module, SockMod, Socket, Opts, Receiver], []);
	raw ->
	    %{ok, Pid} = Module:start({SockMod, Socket}, Opts),
	    %case SockMod:controlling_process(Socket, Pid) of
	    %    ok ->
	    %        ok;
	    %    {error, _Reason} ->
	    %        SockMod:close(Socket)
	    %end
	    todo
    end.

starttls(FsmRef, TLSOpts) ->
    gen_server:call(FsmRef, {starttls, TLSOpts}),
    FsmRef.

starttls(FsmRef, TLSOpts, Data) ->
    gen_server:call(FsmRef, {starttls, TLSOpts, Data}),
    FsmRef.

compress(FsmRef) ->
    gen_server:call(FsmRef, compress),
    FsmRef.

compress(FsmRef, Data) ->
    gen_server:call(FsmRef, {compress, Data}),
    FsmRef.

reset_stream(FsmRef) ->
    gen_server:call(FsmRef, reset_stream).

send(FsmRef, Data) ->
    gen_server:call(FsmRef, {send, Data}).

change_shaper(FsmRef, Shaper) ->
    gen_server:call(FsmRef, {change_shaper, Shaper}).

get_sockmod(FsmRef) ->
    gen_server:call(FsmRef, get_sockmod).

get_peer_certificate(FsmRef) ->
    gen_server:call(FsmRef, get_peer_certificate).

get_verify_result(FsmRef) ->
    gen_server:call(FsmRef, get_verify_result).

close(FsmRef) ->
    gen_server:call(FsmRef, close).


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
init([Module, SockMod, Socket, Opts, Receiver]) ->
    Nodes = mnesia:table_info(schema, disc_copies),
    Node = lists:nth(erlang:phash(now(), length(Nodes)), Nodes),
    {ok, Pid} =
	rpc:call(Node, Module, start, [{?MODULE, self()}, Opts]),
    ejabberd_receiver:become_controller(Receiver, Pid),
    {ok, #state{sockmod = SockMod,
		socket = Socket,
		receiver = Receiver}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({starttls, TLSOpts}, _From, State) ->
    {ok, TLSSocket} = tls:tcp_to_tls(State#state.socket, TLSOpts),
    ejabberd_receiver:starttls(State#state.receiver, TLSSocket),
    Reply = ok,
    {reply, Reply, State#state{socket = TLSSocket, sockmod = tls}};

handle_call({starttls, TLSOpts, Data}, _From, State) ->
    {ok, TLSSocket} = tls:tcp_to_tls(State#state.socket, TLSOpts),
    ejabberd_receiver:starttls(State#state.receiver, TLSSocket),
    catch (State#state.sockmod):send(
	    State#state.socket, Data),
    Reply = ok,
    {reply, Reply, State#state{socket = TLSSocket, sockmod = tls}};

handle_call(compress, _From, State) ->
    {ok, ZlibSocket} = ejabberd_zlib:enable_zlib(
			 State#state.sockmod,
			 State#state.socket),
    ejabberd_receiver:compress(State#state.receiver, ZlibSocket),
    Reply = ok,
    {reply, Reply, State#state{socket = ZlibSocket, sockmod = ejabberd_zlib}};

handle_call({compress, Data}, _From, State) ->
    {ok, ZlibSocket} = ejabberd_zlib:enable_zlib(
			 State#state.sockmod,
			 State#state.socket),
    ejabberd_receiver:compress(State#state.receiver, ZlibSocket),
    catch (State#state.sockmod):send(
	    State#state.socket, Data),
    Reply = ok,
    {reply, Reply, State#state{socket = ZlibSocket, sockmod = ejabberd_zlib}};

handle_call(reset_stream, _From, State) ->
    ejabberd_receiver:reset_stream(State#state.receiver),
    Reply = ok,
    {reply, Reply, State};

handle_call({send, Data}, _From, State) ->
    catch (State#state.sockmod):send(
	    State#state.socket, Data),
    Reply = ok,
    {reply, Reply, State};

handle_call({change_shaper, Shaper}, _From, State) ->
    ejabberd_receiver:change_shaper(State#state.receiver, Shaper),
    Reply = ok,
    {reply, Reply, State};

handle_call(get_sockmod, _From, State) ->
    Reply = State#state.sockmod,
    {reply, Reply, State};

handle_call(get_peer_certificate, _From, State) ->
    Reply = tls:get_peer_certificate(State#state.socket),
    {reply, Reply, State};

handle_call(get_verify_result, _From, State) ->
    Reply = tls:get_verify_result(State#state.socket),
    {reply, Reply, State};

handle_call(close, _From, State) ->
    ejabberd_receiver:close(State#state.receiver),
    Reply = ok,
    {stop, normal, Reply, State};

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

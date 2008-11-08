%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_out.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage outgoing server-to-server connections
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_s2s_out).
-author('alexey@process-one.net').

-behaviour(p1_fsm).

%% External exports
-export([start/3,
	 start_link/3,
	 start_connection/1,
	 terminate_if_waiting_delay/2,
	 stop_connection/1]).

%% p1_fsm callbacks (same as gen_fsm)
-export([init/1,
	 open_socket/2,
	 wait_for_stream/2,
	 wait_for_validation/2,
	 wait_for_features/2,
	 wait_for_auth_result/2,
	 wait_for_starttls_proceed/2,
	 reopen_socket/2,
	 wait_before_retry/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4,
	 test_get_addr_port/1,
	 get_addr_port/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, {socket,
		streamid,
		use_v10,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		db_enabled = true,
		try_auth = true,
		myname, server, queue,
		delay_to_retry = undefined_delay,
		new = false, verify = false,
		timer}).

%%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, p1_fsm:start(ejabberd_s2s_out, [From, Host, Type],
				       ?FSMLIMITS ++ ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_s2s_out_sup,
						 [From, Host, Type])).
-endif.

%% Only change this value if you now what your are doing:
-define(FSMLIMITS,[]).
%% -define(FSMLIMITS, [{max_queue, 2000}]).
-define(FSMTIMEOUT, 30000).

%% Maximum delay to wait before retrying to connect after a failed attempt.
%% Specified in miliseconds. Default value is 5 minutes.
-define(MAX_RETRY_DELAY, 300000).

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"xmlns='jabber:server' "
	"xmlns:db='jabber:server:dialback' "
	"to='~s'~s>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NAMESPACE_ERR,
	xml:element_to_string(?SERR_INVALID_NAMESPACE)).

-define(HOST_UNKNOWN_ERR,
	xml:element_to_string(?SERR_HOST_UNKNOWN)).

-define(INVALID_XML_ERR,
	xml:element_to_string(?SERR_XML_NOT_WELL_FORMED)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(From, Host, Type) ->
    ?SUPERVISOR_START.

start_link(From, Host, Type) ->
    p1_fsm:start_link(ejabberd_s2s_out, [From, Host, Type],
		      ?FSMLIMITS ++ ?FSMOPTS).

start_connection(Pid) ->
    p1_fsm:send_event(Pid, init).

stop_connection(Pid) ->
    p1_fsm:send_event(Pid, stop).

%%%----------------------------------------------------------------------
%%% Callback functions from p1_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([From, Server, Type]) ->
    process_flag(trap_exit, true),
    ?DEBUG("started: ~p", [{From, Server, Type}]),
    TLS = case ejabberd_config:get_local_option(s2s_use_starttls) of
	      undefined ->
		  false;
	      UseStartTLS ->
		  UseStartTLS
	  end,
    UseV10 = TLS,
    TLSOpts = case ejabberd_config:get_local_option(s2s_certfile) of
		  undefined ->
		      [];
		  CertFile ->
		      [{certfile, CertFile}, connect]
	      end,
    {New, Verify} = case Type of
			{new, Key} ->
			    {Key, false};
			{verify, Pid, Key, SID} ->
			    start_connection(self()),
			    {false, {Pid, Key, SID}}
		    end,
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {ok, open_socket, #state{use_v10 = UseV10,
			     tls = TLS,
			     tls_options = TLSOpts,
			     queue = queue:new(),
			     myname = From,
			     server = Server,
			     new = New,
			     verify = Verify,
			     timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
open_socket(init, StateData) ->
    log_s2s_out(StateData#state.new,
		StateData#state.myname,
		StateData#state.server),
    ?DEBUG("open_socket: ~p", [{StateData#state.myname,
				StateData#state.server,
				StateData#state.new,
				StateData#state.verify}]),
    AddrList = case idna:domain_utf8_to_ascii(StateData#state.server) of
		   false -> [];
		   ASCIIAddr ->
		       get_addr_port(ASCIIAddr)
	       end,
    case lists:foldl(fun({Addr, Port}, Acc) ->
			     case Acc of
				 {ok, Socket} ->
				     {ok, Socket};
				 _ ->
				     open_socket1(Addr, Port)
			     end
		     end, {error, badarg}, AddrList) of
	{ok, Socket} ->
	    Version = if
			  StateData#state.use_v10 ->
			      " version='1.0'";
			  true ->
			      ""
		      end,
	    NewStateData = StateData#state{socket = Socket,
					   tls_enabled = false,
					   streamid = new_id()},
	    send_text(NewStateData, io_lib:format(?STREAM_HEADER,
						  [StateData#state.server,
						   Version])),
	    {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
	{error, _Reason} ->
	    ?INFO_MSG("s2s connection: ~s -> ~s (remote server not found)",
		      [StateData#state.myname, StateData#state.server]),
	    wait_before_reconnect(StateData)
	    %%{stop, normal, StateData}
    end;
open_socket(stop, StateData) ->
    ?INFO_MSG("s2s connection: ~s -> ~s (stopped in open socket)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
open_socket(timeout, StateData) ->
    ?INFO_MSG("s2s connection: ~s -> ~s (timeout in open socket)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
open_socket(_, StateData) ->
    {next_state, open_socket, StateData}.

%%----------------------------------------------------------------------
open_socket1(Addr, Port) ->
    ?DEBUG("s2s_out: connecting to ~s:~p~n", [Addr, Port]),
    Res = case catch ejabberd_socket:connect(
		       Addr, Port,
		       [binary, {packet, 0},
			{active, false}]) of
	      {ok, _Socket} = R -> R;
	      {error, Reason1} ->
		  ?DEBUG("s2s_out: connect return ~p~n", [Reason1]),
		  catch ejabberd_socket:connect(
			  Addr, Port,
			  [binary, {packet, 0},
			   {active, false}, inet6]);
	      {'EXIT', Reason1} ->
		  ?DEBUG("s2s_out: connect crashed ~p~n", [Reason1]),
		  catch ejabberd_socket:connect(
			  Addr, Port,
			  [binary, {packet, 0},
			   {active, false}, inet6])
	  end,
    case Res of
	{ok, Socket} ->
	    {ok, Socket};
	{error, Reason} ->
	    ?DEBUG("s2s_out: inet6 connect return ~p~n", [Reason]),
	    {error, Reason};
	{'EXIT', Reason} ->
	    ?DEBUG("s2s_out: inet6 connect crashed ~p~n", [Reason]),
	    {error, Reason}
    end.

%%----------------------------------------------------------------------


wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    case {xml:get_attr_s("xmlns", Attrs),
	  xml:get_attr_s("xmlns:db", Attrs),
	  xml:get_attr_s("version", Attrs) == "1.0"} of
	{"jabber:server", "jabber:server:dialback", false} ->
	    send_db_request(StateData);
	{"jabber:server", "jabber:server:dialback", true} when
	StateData#state.use_v10 ->
	    {next_state, wait_for_features, StateData, ?FSMTIMEOUT};
	{"jabber:server", "", true} when StateData#state.use_v10 ->
	    {next_state, wait_for_features, StateData#state{db_enabled = false}, ?FSMTIMEOUT};
	_ ->
	    send_text(StateData, ?INVALID_NAMESPACE_ERR),
	    ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid namespace)",
		      [StateData#state.myname, StateData#state.server]),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid xml)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

wait_for_stream(timeout, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (timeout in wait_for_stream)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (close in wait_for_stream)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.



wait_for_validation({xmlstreamelement, El}, StateData) ->
    case is_verify_res(El) of
	{result, To, From, Id, Type} ->
	    ?DEBUG("recv result: ~p", [{From, To, Id, Type}]),
	    case Type of
		"valid" ->
		    send_queue(StateData, StateData#state.queue),
		    ?INFO_MSG("Connection established: ~s -> ~s",
			      [StateData#state.myname, StateData#state.server]),
		    ejabberd_hooks:run(s2s_connect_hook,
				       [StateData#state.myname,
					StateData#state.server]),
		    {next_state, stream_established,
		     StateData#state{queue = queue:new()}};
		_ ->
		    %% TODO: bounce packets
		    ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid dialback key)",
			      [StateData#state.myname, StateData#state.server]),
		    {stop, normal, StateData}
	    end;
	{verify, To, From, Id, Type} ->
	    ?DEBUG("recv verify: ~p", [{From, To, Id, Type}]),
	    case StateData#state.verify of
		false ->
		    NextState = wait_for_validation,
		    %% TODO: Should'nt we close the connection here ?
		    {next_state, NextState, StateData,
		     get_timeout_interval(NextState)};
		{Pid, _Key, _SID} ->
		    case Type of
			"valid" ->
			    p1_fsm:send_event(
			      Pid, {valid,
				    StateData#state.server,
				    StateData#state.myname});
			_ ->
			    p1_fsm:send_event(
			      Pid, {invalid,
				    StateData#state.server,
				    StateData#state.myname})
		    end,
		    if
			StateData#state.verify == false ->
			    {stop, normal, StateData};
			true ->
			    NextState = wait_for_validation,
			    {next_state, NextState, StateData,
			     get_timeout_interval(NextState)}
		    end
	    end;
	_ ->
	    {next_state, wait_for_validation, StateData, ?FSMTIMEOUT*3}
    end;

wait_for_validation({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (xmlstreamend)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

wait_for_validation({xmlstreamerror, _}, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (xmlstreamerror)",
	      [StateData#state.myname, StateData#state.server]),
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_validation(timeout, #state{verify = {VPid, VKey, SID}} = StateData)
  when is_pid(VPid) and is_list(VKey) and is_list(SID) ->
    %% This is an auxiliary s2s connection for dialback.
    %% This timeout is normal and doesn't represent a problem.
    ?DEBUG("wait_for_validation: ~s -> ~s (timeout in verify connection)",
	   [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

wait_for_validation(timeout, StateData) ->
    ?INFO_MSG("wait_for_validation: ~s -> ~s (connect timeout)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

wait_for_validation(closed, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (closed)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.


wait_for_features({xmlstreamelement, El}, StateData) ->
    case El of
	{xmlelement, "stream:features", _Attrs, Els} ->
	    {SASLEXT, StartTLS, StartTLSRequired} =
		lists:foldl(
		  fun({xmlelement, "mechanisms", Attrs1, Els1} = _El1,
		      {_SEXT, STLS, STLSReq} = Acc) ->
			  case xml:get_attr_s("xmlns", Attrs1) of
			      ?NS_SASL ->
				  NewSEXT =
				      lists:any(
					fun({xmlelement, "mechanism", _, Els2}) ->
						case xml:get_cdata(Els2) of
						    "EXTERNAL" -> true;
						    _ -> false
						end;
					   (_) -> false
					end, Els1),
				  {NewSEXT, STLS, STLSReq};
			      _ ->
				  Acc
			  end;
		     ({xmlelement, "starttls", Attrs1, _Els1} = El1,
		      {SEXT, _STLS, _STLSReq} = Acc) ->
			  case xml:get_attr_s("xmlns", Attrs1) of
			      ?NS_TLS ->
				  Req = case xml:get_subtag(El1, "required") of
					    {xmlelement, _, _, _} -> true;
					    false -> false
					end,
				  {SEXT, true, Req};
			      _ ->
				  Acc
			  end;
		     (_, Acc) ->
			  Acc
		  end, {false, false, false}, Els),
	    if
		(not SASLEXT) and (not StartTLS) and
		StateData#state.authenticated ->
		    send_queue(StateData, StateData#state.queue),
		    ?INFO_MSG("Connection established: ~s -> ~s",
			      [StateData#state.myname, StateData#state.server]),
		    ejabberd_hooks:run(s2s_connect_hook,
				       [StateData#state.myname,
					StateData#state.server]),
		    {next_state, stream_established,
		     StateData#state{queue = queue:new()}};
		SASLEXT and StateData#state.try_auth and
		(StateData#state.new /= false) ->
		    send_element(StateData,
				 {xmlelement, "auth",
				  [{"xmlns", ?NS_SASL},
				   {"mechanism", "EXTERNAL"}],
				  [{xmlcdata,
				    jlib:encode_base64(
				      StateData#state.myname)}]}),
		    {next_state, wait_for_auth_result,
		     StateData#state{try_auth = false}, ?FSMTIMEOUT};
		StartTLS and StateData#state.tls and
		(not StateData#state.tls_enabled) ->
		    send_element(StateData,
				 {xmlelement, "starttls",
				  [{"xmlns", ?NS_TLS}], []}),
		    {next_state, wait_for_starttls_proceed, StateData,
		     ?FSMTIMEOUT};
		StartTLSRequired and (not StateData#state.tls) ->
		    ?DEBUG("restarted: ~p", [{StateData#state.myname,
					      StateData#state.server}]),
		    ejabberd_socket:close(StateData#state.socket),
		    {next_state, reopen_socket,
		     StateData#state{socket = undefined,
				     use_v10 = false}, ?FSMTIMEOUT};
		StateData#state.db_enabled ->
		    send_db_request(StateData);
		true ->
		    ?DEBUG("restarted: ~p", [{StateData#state.myname,
					      StateData#state.server}]),
						% TODO: clear message queue
		    ejabberd_socket:close(StateData#state.socket),
		    {next_state, reopen_socket, StateData#state{socket = undefined,
								use_v10 = false}, ?FSMTIMEOUT}
	    end;
	_ ->
	    send_text(StateData,
		      xml:element_to_string(?SERR_BAD_FORMAT) ++
		      ?STREAM_TRAILER),
	    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
		      [StateData#state.myname, StateData#state.server]),
	    {stop, normal, StateData}
    end;

wait_for_features({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait_for_features: xmlstreamend", []),
    {stop, normal, StateData};

wait_for_features({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    ?INFO_MSG("wait for features: xmlstreamerror", []),
    {stop, normal, StateData};

wait_for_features(timeout, StateData) ->
    ?INFO_MSG("wait for features: timeout", []),
    {stop, normal, StateData};

wait_for_features(closed, StateData) ->
    ?INFO_MSG("wait for features: closed", []),
    {stop, normal, StateData}.


wait_for_auth_result({xmlstreamelement, El}, StateData) ->
    case El of
	{xmlelement, "success", Attrs, _Els} ->
	    case xml:get_attr_s("xmlns", Attrs) of
		?NS_SASL ->
		    ?DEBUG("auth: ~p", [{StateData#state.myname,
					 StateData#state.server}]),
		    ejabberd_socket:reset_stream(StateData#state.socket),
		    send_text(StateData,
			      io_lib:format(?STREAM_HEADER,
					    [StateData#state.server,
					     " version='1.0'"])),
		    {next_state, wait_for_stream,
		     StateData#state{streamid = new_id(),
				     authenticated = true
				    }, ?FSMTIMEOUT};
		_ ->
		    send_text(StateData,
			      xml:element_to_string(?SERR_BAD_FORMAT) ++
			      ?STREAM_TRAILER),
		    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
			      [StateData#state.myname, StateData#state.server]),
		    {stop, normal, StateData}
	    end;
	{xmlelement, "failure", Attrs, _Els} ->
	    case xml:get_attr_s("xmlns", Attrs) of
		?NS_SASL ->
		    ?DEBUG("restarted: ~p", [{StateData#state.myname,
					      StateData#state.server}]),
		    ejabberd_socket:close(StateData#state.socket),
		    {next_state, reopen_socket,
		     StateData#state{socket = undefined}, ?FSMTIMEOUT};
		_ ->
		    send_text(StateData,
			      xml:element_to_string(?SERR_BAD_FORMAT) ++
			      ?STREAM_TRAILER),
		    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
			      [StateData#state.myname, StateData#state.server]),
		    {stop, normal, StateData}
	    end;
	_ ->
	    send_text(StateData,
		      xml:element_to_string(?SERR_BAD_FORMAT) ++
		      ?STREAM_TRAILER),
	    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
		      [StateData#state.myname, StateData#state.server]),
	    {stop, normal, StateData}
    end;

wait_for_auth_result({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for auth result: xmlstreamend", []),
    {stop, normal, StateData};

wait_for_auth_result({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    ?INFO_MSG("wait for auth result: xmlstreamerror", []),
    {stop, normal, StateData};

wait_for_auth_result(timeout, StateData) ->
    ?INFO_MSG("wait for auth result: timeout", []),
    {stop, normal, StateData};

wait_for_auth_result(closed, StateData) ->
    ?INFO_MSG("wait for auth result: closed", []),
    {stop, normal, StateData}.


wait_for_starttls_proceed({xmlstreamelement, El}, StateData) ->
    case El of
	{xmlelement, "proceed", Attrs, _Els} ->
	    case xml:get_attr_s("xmlns", Attrs) of
		?NS_TLS ->
		    ?DEBUG("starttls: ~p", [{StateData#state.myname,
					     StateData#state.server}]),
		    Socket = StateData#state.socket,
		    TLSOpts = case ejabberd_config:get_local_option(
				     {domain_certfile,
				      StateData#state.server}) of
				  undefined ->
				      StateData#state.tls_options;
				  CertFile ->
				      [{certfile, CertFile} |
				       lists:keydelete(
					 certfile, 1,
					 StateData#state.tls_options)]
			      end,
		    TLSSocket = ejabberd_socket:starttls(Socket, TLSOpts),
		    NewStateData = StateData#state{socket = TLSSocket,
						   streamid = new_id(),
						   tls_enabled = true
						  },
		    send_text(NewStateData,
			      io_lib:format(?STREAM_HEADER,
					    [StateData#state.server,
					     " version='1.0'"])),
		    {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
		_ ->
		    send_text(StateData,
			      xml:element_to_string(?SERR_BAD_FORMAT) ++
			      ?STREAM_TRAILER),
		    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
			      [StateData#state.myname, StateData#state.server]),
		    {stop, normal, StateData}
	    end;
	_ ->
	    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
		      [StateData#state.myname, StateData#state.server]),
	    {stop, normal, StateData}
    end;

wait_for_starttls_proceed({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for starttls proceed: xmlstreamend", []),
    {stop, normal, StateData};

wait_for_starttls_proceed({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    ?INFO_MSG("wait for starttls proceed: xmlstreamerror", []),
    {stop, normal, StateData};

wait_for_starttls_proceed(timeout, StateData) ->
    ?INFO_MSG("wait for starttls proceed: timeout", []),
    {stop, normal, StateData};

wait_for_starttls_proceed(closed, StateData) ->
    ?INFO_MSG("wait for starttls proceed: closed", []),
    {stop, normal, StateData}.


reopen_socket({xmlstreamelement, _El}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamend, _Name}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamerror, _}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket(timeout, StateData) ->
    ?INFO_MSG("reopen socket: timeout", []),
    {stop, normal, StateData};
reopen_socket(closed, StateData) ->
    p1_fsm:send_event(self(), init),
    {next_state, open_socket, StateData, ?FSMTIMEOUT}.

%% This state is use to avoid reconnecting to often to bad sockets
wait_before_retry(_Event, StateData) ->
    {next_state, wait_before_retry, StateData, ?FSMTIMEOUT}.

stream_established({xmlstreamelement, El}, StateData) ->
    ?DEBUG("s2S stream established", []),
    case is_verify_res(El) of
	{verify, VTo, VFrom, VId, VType} ->
	    ?DEBUG("recv verify: ~p", [{VFrom, VTo, VId, VType}]),
	    case StateData#state.verify of
		{VPid, _VKey, _SID} ->
		    case VType of
			"valid" ->
			    p1_fsm:send_event(
			      VPid, {valid,
				     StateData#state.server,
				     StateData#state.myname});
			_ ->
			    p1_fsm:send_event(
			      VPid, {invalid,
				     StateData#state.server,
				     StateData#state.myname})
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {next_state, stream_established, StateData};

stream_established({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (xmlstreamend)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
	      ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    ?INFO_MSG("stream established: ~s -> ~s (xmlstreamerror)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

stream_established(timeout, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (timeout)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};

stream_established(closed, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (closed)",
	      [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.



%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
%%state_name(Event, From, StateData) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, get_timeout_interval(StateName)}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData, get_timeout_interval(StateName)}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {next_state, StateName, StateData#state{timer = Timer},
     get_timeout_interval(StateName)};

handle_info({send_element, El}, StateName, StateData) ->
    case StateName of
	stream_established ->
	    cancel_timer(StateData#state.timer),
	    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
	    send_element(StateData, El),
	    {next_state, StateName, StateData#state{timer = Timer}};
	%% In this state we bounce all message: We are waiting before
	%% trying to reconnect
	wait_before_retry ->
	    bounce_element(El, ?ERR_REMOTE_SERVER_NOT_FOUND),
	    {next_state, StateName, StateData};
	_ ->
	    Q = queue:in(El, StateData#state.queue),
	    {next_state, StateName, StateData#state{queue = Q},
	     get_timeout_interval(StateName)}
    end;

handle_info({timeout, Timer, _}, wait_before_retry,
	    #state{timer = Timer} = StateData) ->
    ?INFO_MSG("Reconnect delay expired: Will now retry to connect to ~s when needed.", [StateData#state.server]),
    {stop, normal, StateData};

handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    ?INFO_MSG("Closing connection with ~s: timeout", [StateData#state.server]),
    {stop, normal, StateData};

handle_info(terminate_if_waiting_before_retry, wait_before_retry, StateData) ->
    {stop, normal, StateData};

handle_info(terminate_if_waiting_before_retry, StateName, StateData) ->
    {next_state, StateName, StateData, get_timeout_interval(StateName)};

handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData, get_timeout_interval(StateName)}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?DEBUG("terminated: ~p", [{Reason, StateName}]),
    case StateData#state.new of
	false ->
	    ok;
	Key ->
	    ejabberd_s2s:remove_connection(
	      {StateData#state.myname, StateData#state.server}, self(), Key)
    end,
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),
    case StateData#state.socket of
	undefined ->
	    ok;
	_Socket ->
	    ejabberd_socket:close(StateData#state.socket)
    end,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(StateData, Text) ->
    ejabberd_socket:send(StateData#state.socket, Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_string(El)).

send_queue(StateData, Q) ->
    case queue:out(Q) of
	{{value, El}, Q1} ->
	    send_element(StateData, El),
	    send_queue(StateData, Q1);
	{empty, _Q1} ->
	    ok
    end.

%% Bounce a single message (xmlelement)
bounce_element(El, Error) ->
    {xmlelement, _Name, Attrs, _SubTags} = El,
    case xml:get_attr_s("type", Attrs) of
	"error" -> ok;
	"result" -> ok;
	_ ->
	    Err = jlib:make_error_reply(El, Error),
	    From = jlib:string_to_jid(xml:get_tag_attr_s("from", El)),
	    To = jlib:string_to_jid(xml:get_tag_attr_s("to", El)),
	    ejabberd_router:route(To, From, Err)
    end.

bounce_queue(Q, Error) ->
    case queue:out(Q) of
	{{value, El}, Q1} ->
	    bounce_element(El, Error),
	    bounce_queue(Q1, Error);
	{empty, _} ->
	    ok
    end.

new_id() ->
    randoms:get_string().

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
	{timeout, Timer, _} ->
	    ok
    after 0 ->
	    ok
    end.

bounce_messages(Error) ->
    receive
	{send_element, El} ->
	    bounce_element(El, Error),
	    bounce_messages(Error)
    after 0 ->
	    ok
    end.


send_db_request(StateData) ->
    Server = StateData#state.server,
    New = case StateData#state.new of
	      false ->
		  case ejabberd_s2s:try_register(
			 {StateData#state.myname, Server}) of
		      {key, Key} ->
			  Key;
		      false ->
			  false
		  end;
	      Key ->
		  Key
	  end,
    case New of
	false ->
	    ok;
	Key1 ->
	    send_element(StateData,
			 {xmlelement,
			  "db:result",
			  [{"from", StateData#state.myname},
			   {"to", Server}],
			  [{xmlcdata, Key1}]})
    end,
    case StateData#state.verify of
	false ->
	    ok;
	{_Pid, Key2, SID} ->
	    send_element(StateData,
			 {xmlelement,
			  "db:verify",
			  [{"from", StateData#state.myname},
			   {"to", StateData#state.server},
			   {"id", SID}],
			  [{xmlcdata, Key2}]})
    end,
    {next_state, wait_for_validation, StateData#state{new = New}, ?FSMTIMEOUT*6}.


is_verify_res({xmlelement, Name, Attrs, _Els}) when Name == "db:result" ->
    {result,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_attr_s("type", Attrs)};
is_verify_res({xmlelement, Name, Attrs, _Els}) when Name == "db:verify" ->
    {verify,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_attr_s("type", Attrs)};
is_verify_res(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SRV support

-include_lib("kernel/include/inet.hrl").

get_addr_port(Server) ->
    Res = case inet_res:getbyname("_xmpp-server._tcp." ++ Server, srv) of
	      {error, _Reason} ->
		  inet_res:getbyname("_jabber._tcp." ++ Server, srv);
	      {ok, _HEnt} = R -> R
	  end,
    case Res of
	{error, Reason} ->
	    ?DEBUG("srv lookup of '~s' failed: ~p~n", [Server, Reason]),
	    [{Server, outgoing_s2s_port()}];
	{ok, HEnt} ->
	    ?DEBUG("srv lookup of '~s': ~p~n",
		   [Server, HEnt#hostent.h_addr_list]),
	    case HEnt#hostent.h_addr_list of
		[] ->
		    [{Server, outgoing_s2s_port()}];
		AddrList ->
		    %% Probabilities are not exactly proportional to weights
		    %% for simplicity (higher weigths are overvalued)
		    {A1, A2, A3} = now(),
		    random:seed(A1, A2, A3),
		    case (catch lists:map(
				  fun({Priority, Weight, Port, Host}) ->
					  N = case Weight of
						  0 -> 0;
						  _ -> (Weight + 1) * random:uniform()
					      end,
					  {Priority * 65536 - N, Host, Port}
				  end, AddrList)) of
			{'EXIT', _Reasn} ->
			    [{Server, outgoing_s2s_port()}];
			SortedList ->
			    List = lists:map(
				     fun({_, Host, Port}) ->
					     {Host, Port}
				     end, lists:keysort(1, SortedList)),
			    ?DEBUG("srv lookup of '~s': ~p~n", [Server, List]),
			    List
		    end
	    end
    end.

test_get_addr_port(Server) ->
    lists:foldl(
      fun(_, Acc) ->
	      [HostPort | _] = get_addr_port(Server),
	      case lists:keysearch(HostPort, 1, Acc) of
		  false ->
		      [{HostPort, 1} | Acc];
		  {value, {_, Num}} ->
		      lists:keyreplace(HostPort, 1, Acc, {HostPort, Num + 1})
	      end
      end, [], lists:seq(1, 100000)).

outgoing_s2s_port() ->
    case ejabberd_config:get_local_option(outgoing_s2s_port) of
	Port when is_integer(Port) ->
	    Port;
	undefined ->
	    5269
    end.

%% Human readable S2S logging: Log only new outgoing connections as INFO
%% Do not log dialback
log_s2s_out(false, _, _) -> ok;
%% Log new outgoing connections:
log_s2s_out(_, Myname, Server) ->
    ?INFO_MSG("Trying to open s2s connection: ~s -> ~s",[Myname, Server]).

%% Calculate timeout depending on which state we are in:
%% Can return integer > 0 | infinity
get_timeout_interval(StateName) ->
    case StateName of
	%% Validation implies dialback: Networking can take longer:
	wait_for_validation ->
	    ?FSMTIMEOUT*6;
	%% When stream is established, we only rely on S2S Timeout timer:
	stream_established ->
	    infinity;
	_ ->
	    ?FSMTIMEOUT
    end.

%% This function is intended to be called at the end of a state
%% function that want to wait for a reconnect delay before stopping.
wait_before_reconnect(StateData) ->
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),
    cancel_timer(StateData#state.timer),
    Delay = case StateData#state.delay_to_retry of
		undefined_delay ->
		    %% The initial delay is random between 1 and 15 seconds
		    %% Return a random integer between 1000 and 15000
		    {_, _, MicroSecs} = now(),
		    (MicroSecs rem 14000) + 1000;
		D1 ->
		    %% Duplicate the delay with each successive failed
		    %% reconnection attempt, but don't exceed the max
		    lists:min([D1 * 2, get_max_retry_delay()])
	    end,
    Timer = erlang:start_timer(Delay, self(), []),
    {next_state, wait_before_retry, StateData#state{timer=Timer,
						    delay_to_retry = Delay,
						    queue = queue:new()}}.

%% @doc Get the maximum allowed delay for retry to reconnect (in miliseconds).
%% The default value is 5 minutes.
%% The option {s2s_max_retry_delay, Seconds} can be used (in seconds).
%% @spec () -> integer()
get_max_retry_delay() ->
    case ejabberd_config:get_local_option(s2s_max_retry_delay) of
	Seconds when is_integer(Seconds) ->
	    Seconds*1000;
	_ ->
	    ?MAX_RETRY_DELAY
    end.

%% Terminate s2s_out connections that are in state wait_before_retry
terminate_if_waiting_delay(From, To) ->
    FromTo = {From, To},
    Pids = ejabberd_s2s:get_connections_pids(FromTo),
    lists:foreach(
      fun(Pid) ->
	      Pid ! terminate_if_waiting_before_retry
      end,
      Pids).

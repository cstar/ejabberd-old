%%%----------------------------------------------------------------------
%%% File    : tls.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to openssl
%%% Created : 24 Jul 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(tls).
-author('alexey@process-one.net').

-behaviour(gen_server).

-export([start/0, start_link/0,
	 tcp_to_tls/2, tls_to_tcp/1,
	 send/2,
	 recv/2, recv/3, recv_data/2,
	 setopts/2,
	 sockname/1, peername/1,
	 controlling_process/2,
	 close/1,
	 get_peer_certificate/1,
	 get_verify_result/1,
	 test/0]).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-include("ejabberd.hrl").

-define(SET_CERTIFICATE_FILE_ACCEPT, 1).
-define(SET_CERTIFICATE_FILE_CONNECT, 2).
-define(SET_ENCRYPTED_INPUT,  3).
-define(SET_DECRYPTED_OUTPUT, 4).
-define(GET_ENCRYPTED_OUTPUT, 5).
-define(GET_DECRYPTED_INPUT,  6).
-define(GET_PEER_CERTIFICATE, 7).
-define(GET_VERIFY_RESULT,    8).
-define(VERIFY_NONE, 16#10000).

-record(tlssock, {tcpsock, tlsport}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv) of
	ok -> ok;
	{error, already_loaded} -> ok
    end,
    Port = open_port({spawn, tls_drv}, [binary]),
    Res = port_control(Port, ?SET_CERTIFICATE_FILE_ACCEPT, "./ssl.pem" ++ [0]),
    case Res of
	<<0>> ->
	    %ets:new(iconv_table, [set, public, named_table]),
	    %ets:insert(iconv_table, {port, Port}),
	    {ok, Port};
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)}
    end.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};

handle_info({'EXIT', _Pid, _Reason}, Port) ->
    {noreply, Port};

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    Port ! {self, close},
    ok.


tcp_to_tls(TCPSocket, Options) ->
    case lists:keysearch(certfile, 1, Options) of
	{value, {certfile, CertFile}} ->
	    case erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv) of
		ok -> ok;
		{error, already_loaded} -> ok
	    end,
	    Port = open_port({spawn, tls_drv}, [binary]),
	    Flags =
		case lists:member(verify_none, Options) of
		    true ->
			?VERIFY_NONE;
		    false ->
			0
		end,
	    Command = case lists:member(connect, Options) of
			  true ->
			      ?SET_CERTIFICATE_FILE_CONNECT;
			  false ->
			      ?SET_CERTIFICATE_FILE_ACCEPT
		      end,
	    case port_control(Port, Command bor Flags, CertFile ++ [0]) of
		<<0>> ->
		    {ok, #tlssock{tcpsock = TCPSocket, tlsport = Port}};
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	false ->
	    {error, no_certfile}
    end.
    
tls_to_tcp(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    port_close(Port),
    TCPSocket.

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#tlssock{tcpsock = TCPSocket} = TLSSock,
     Length, Timeout) ->
    case gen_tcp:recv(TCPSocket, Length, Timeout) of
	{ok, Packet} ->
	    recv_data(TLSSock, Packet);
	{error, _Reason} = Error ->
	    Error
    end.

recv_data(#tlssock{tcpsock = TCPSocket, tlsport = Port}, Packet) ->
    case port_control(Port, ?SET_ENCRYPTED_INPUT, Packet) of
	<<0>> ->
	    case port_control(Port, ?GET_DECRYPTED_INPUT, []) of
		<<0, In/binary>> ->
		    case port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
			<<0, Out/binary>> ->
			    case gen_tcp:send(TCPSocket, Out) of
				ok ->
				    %?PRINT("IN: ~p~n", [{TCPSocket, binary_to_list(In)}]),
				    {ok, In};
				Error ->
				    Error
			    end;
			<<1, Error/binary>> ->
			    {error, binary_to_list(Error)}
		    end;
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)}
    end.

send(#tlssock{tcpsock = TCPSocket, tlsport = Port}, Packet) ->
    case port_control(Port, ?SET_DECRYPTED_OUTPUT, Packet) of
	<<0>> ->
	    %?PRINT("OUT: ~p~n", [{TCPSocket, lists:flatten(Packet)}]),
	    case port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
		<<0, Out/binary>> ->
		    gen_tcp:send(TCPSocket, Out);
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	<<1, Error/binary>> ->
	    {error, binary_to_list(Error)};
	<<2>> -> % Dirty hack
	    receive
		{timeout, _Timer, _} ->
		    {error, timeout}
	    after 100 ->
		    send(#tlssock{tcpsock = TCPSocket, tlsport = Port}, Packet)
	    end
    end.


setopts(#tlssock{tcpsock = TCPSocket}, Opts) ->
    inet:setopts(TCPSocket, Opts).

sockname(#tlssock{tcpsock = TCPSocket}) ->
    inet:sockname(TCPSocket).

peername(#tlssock{tcpsock = TCPSocket}) ->
    inet:peername(TCPSocket).

controlling_process(#tlssock{tcpsock = TCPSocket}, Pid) ->
    gen_tcp:controlling_process(TCPSocket, Pid).

close(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    gen_tcp:close(TCPSocket),
    port_close(Port).

get_peer_certificate(#tlssock{tlsport = Port}) ->
    case port_control(Port, ?GET_PEER_CERTIFICATE, []) of
	<<0, BCert/binary>> ->
	    case catch ssl_pkix:decode_cert(BCert, [pkix]) of
		{ok, Cert} ->
		    {ok, Cert};
		_ ->
		    error
	    end;
	<<1>> ->
	    error
    end.

get_verify_result(#tlssock{tlsport = Port}) ->
    <<Res>> = port_control(Port, ?GET_VERIFY_RESULT, []),
    Res.


test() ->
    case erl_ddll:load_driver(ejabberd:get_so_path(), tls_drv) of
	ok -> ok;
	{error, already_loaded} -> ok
    end,
    Port = open_port({spawn, tls_drv}, [binary]),
    ?PRINT("open_port: ~p~n", [Port]),
    PCRes = port_control(Port, ?SET_CERTIFICATE_FILE_ACCEPT,
			 "./ssl.pem" ++ [0]),
    ?PRINT("port_control: ~p~n", [PCRes]),
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary,
					       {packet, 0}, 
					       {active, true},
					       {reuseaddr, true},
					       {nodelay, true}]),
    ?PRINT("listen: ~p~n", [ListenSocket]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ?PRINT("accept: ~p~n", [Socket]),
    loop(Port, Socket).


loop(Port, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    %?PRINT("read: ~p~n", [Data]),
	    Res = port_control(Port, ?SET_ENCRYPTED_INPUT, Data),
	    ?PRINT("SET_ENCRYPTED_INPUT: ~p~n", [Res]),

	    DIRes = port_control(Port, ?GET_DECRYPTED_INPUT, Data),
	    ?PRINT("GET_DECRYPTED_INPUT: ~p~n", [DIRes]),
	    case DIRes of
		<<0, In/binary>> ->
		    ?PRINT("input: ~s~n", [binary_to_list(In)]);
		<<1, DIError/binary>> ->
		    ?PRINT("GET_DECRYPTED_INPUT error: ~p~n", [binary_to_list(DIError)])
	    end,

	    EORes = port_control(Port, ?GET_ENCRYPTED_OUTPUT, Data),
	    ?PRINT("GET_ENCRYPTED_OUTPUT: ~p~n", [EORes]),
	    case EORes of
		<<0, Out/binary>> ->
		    gen_tcp:send(Socket, Out);
		<<1, EOError/binary>> ->
		    ?PRINT("GET_ENCRYPTED_OUTPUT error: ~p~n", [binary_to_list(EOError)])
	    end,
		    

	    loop(Port, Socket);
	Msg ->
	    ?PRINT("receive: ~p~n", [Msg]),
	    loop(Port, Socket)
    end.



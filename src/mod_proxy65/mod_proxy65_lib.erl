%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_lib.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 parsing library.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_proxy65_lib).
-author('xram@jabber.ru').

-include("mod_proxy65.hrl").

-export([
	 unpack_init_message/1,
	 unpack_auth_request/1,
	 unpack_request/1,
	 make_init_reply/1,
	 make_auth_reply/1,
	 make_reply/0,
	 make_error_reply/1,
	 make_error_reply/2
	]).

unpack_init_message(<<?VERSION_5, N, AuthMethodList:N/binary>>)
  when N > 0, N < 256 ->
    {ok, binary_to_list(AuthMethodList)};

unpack_init_message(_) ->
    error.

unpack_auth_request(<<1, ULen, User:ULen/binary,
		     PLen, Pass:PLen/binary>>) when ULen < 256, PLen < 256 ->
    {binary_to_list(User), binary_to_list(Pass)};

unpack_auth_request(_) ->
    error.

unpack_request(<<?VERSION_5, CMD, RSV,
		?ATYP_DOMAINNAME, 40,
		SHA1:40/binary, 0, 0>>) when CMD == ?CMD_CONNECT;
					     CMD == ?CMD_UDP ->
    Command = if
		  CMD == ?CMD_CONNECT -> connect;
		  CMD == ?CMD_UDP -> udp
	      end,
    #s5_request{cmd = Command, rsv = RSV, sha1 = binary_to_list(SHA1)};

unpack_request(_) ->
    error.

make_init_reply(Method) ->
    [?VERSION_5, Method].

make_auth_reply(true) -> [1, ?SUCCESS];
make_auth_reply(false) -> [1, ?ERR_NOT_ALLOWED].

%% WARNING: According to SOCKS5 RFC, this reply is _incorrect_, but
%% Psi writes junk to the beginning of the file on correct reply.
%% I'm not sure, but there may be an issue with other clients.
%% Needs more testing.
make_reply() ->
    [?VERSION_5, ?SUCCESS, 0, 0, 0, 0].

make_error_reply(Request) ->
    make_error_reply(Request, ?ERR_NOT_ALLOWED).

make_error_reply(#s5_request{rsv = RSV, sha1 = SHA1}, Reason) ->
    [?VERSION_5, Reason, RSV, ?ATYP_DOMAINNAME, length(SHA1), SHA1, 0,0].

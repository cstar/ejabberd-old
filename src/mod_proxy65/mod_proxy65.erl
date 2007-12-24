%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Main supervisor.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2007   Process-one
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

-module(mod_proxy65).
-author('xram@jabber.ru').

-behaviour(gen_mod).
-behaviour(supervisor).

%% gen_mod callbacks.
-export([start/2, stop/1]).

%% supervisor callbacks.
-export([init/1]).

%% API.
-export([start_link/2]).

-define(PROCNAME, ejabberd_mod_proxy65).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {
      Proc, {?MODULE, start_link, [Host, Opts]},
      transient, infinity, supervisor, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:start_link({local, Proc}, ?MODULE, [Host, Opts]).

init([Host, Opts]) ->
    Service =
	{mod_proxy65_service, {mod_proxy65_service, start_link, [Host, Opts]},
	 transient, 5000, worker, [mod_proxy65_service]},
    StreamSupervisor =
	{ejabberd_mod_proxy65_sup,
	 {ejabberd_tmp_sup, start_link,
	  [gen_mod:get_module_proc(Host, ejabberd_mod_proxy65_sup),
	   mod_proxy65_stream]},
      transient, infinity, supervisor, [ejabberd_tmp_sup]},
    StreamManager =
	{mod_proxy65_sm, {mod_proxy65_sm, start_link, [Host, Opts]},
	 transient, 5000, worker, [mod_proxy65_sm]},
    {ok, {{one_for_one, 10, 1},
	  [StreamManager, StreamSupervisor, Service]}}.

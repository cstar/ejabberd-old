%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ODBC connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_odbc_sup).
-author('alexey@process-one.net').

%% API
-export([start_link/1,
	 init/1,
	 get_pids/1,
	 get_random_pid/1
	]).

-include("ejabberd.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_ODBC_START_INTERVAL, 30). % 30 seconds

start_link(Host) ->
    supervisor:start_link({local, gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    PoolSize = case ejabberd_config:get_local_option({odbc_pool_size, Host}) of
		   I when is_integer(I) ->
		       I;
	    undefined ->
		       ?DEFAULT_POOL_SIZE;
		   Other ->
		       ?ERROR_MSG("Wrong odbc_pool_size definition '~p' for host ~p, default to ~p~n",
				  [Other, Host, ?DEFAULT_POOL_SIZE]),
		       ?DEFAULT_POOL_SIZE
	       end,
    StartInterval = case ejabberd_config:get_local_option({odbc_start_interval, Host}) of
			Interval when is_integer(Interval) ->
			    Interval;
			undefined ->
			    ?DEFAULT_ODBC_START_INTERVAL;
			_Other2 ->
			    ?ERROR_MSG("Wrong odbc_start_interval definition '~p' for host ~p"
				       ", defaulting to ~p~n",
				       [_Other2, Host, ?DEFAULT_ODBC_START_INTERVAL]),
			    ?DEFAULT_ODBC_START_INTERVAL
		    end,
    {ok, {{one_for_one, PoolSize+1, StartInterval},
	  lists:map(
	    fun(I) ->
		    {I,
		     {ejabberd_odbc, start_link, [Host, StartInterval*1000]},
		     transient,
		     brutal_kill,
		     worker,
		     [?MODULE]}
	    end, lists:seq(1, PoolSize))}}.

get_pids(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    [Child ||
	{_Id, Child, _Type, _Modules} <- supervisor:which_children(Proc),
	Child /= undefined].

get_random_pid(Host) ->
    Pids = get_pids(Host),
    lists:nth(erlang:phash(now(), length(Pids)), Pids).

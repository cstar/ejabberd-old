%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% The Initial Developer of the Original Code is Process-one.
%%% Portions created by Process-one are Copyright 2006-2008, Process-one
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, Process-one.
%%%
%%%
%%% @copyright 2006-2008 Process-one
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @private
%%% @doc <p>The module <strong>{@module}</strong> defines the PubSub node
%%% tree plugin behaviour. This behaviour is used to check that a PubSub 
%%% node tree plugin respects the current ejabberd PubSub plugin API.</p>

-module(gen_pubsub_nodetree).

-export([behaviour_info/1]).

%% @spec (Query::atom()) -> Callbacks | atom()
%%	Callbacks = [{Function,Arity}]
%%	Function = atom()
%%	Arity = integer()
%% @doc Behaviour definition
behaviour_info(callbacks) ->
	[{init, 3},
	 {terminate, 2},
	 {options, 0},
	 {set_node, 1},
	 {get_node, 2},
	 {get_nodes, 1},
	 {get_subnodes, 3},
	 {get_subnodes_tree, 2},
	 {create_node, 5},
	 {delete_node, 2}
	];
behaviour_info(_Other) ->
	undefined.

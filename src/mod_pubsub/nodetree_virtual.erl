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
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2008, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, ProcessOne.
%%%
%%%
%%% @copyright 2006-2008 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the PubSub node tree plugin that
%%% allow virtual nodes handling.
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_virtual).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-behaviour(gen_pubsub_nodetree).

-export([init/3,
	 terminate/2,
	 options/0,
	 set_node/1,
	 get_node/3,
	 get_node/2,
	 get_nodes/2,
	 get_nodes/1,
	 get_subnodes/3,
	 get_subnodes_tree/2,
	 create_node/5,
	 delete_node/2
	]).

%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Opts) -> any()
%%     Host = mod_pubsub:host()
%%     ServerHost = host()
%%     Opts = list()
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
init(_Host, _ServerHost, _Opts) ->
    ok.
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc <p>Returns the default pubsub node tree options.</p>
options() ->
    [{virtual_tree, true}].

%% @spec (NodeRecord) -> ok | {error, Reason}
%%     NodeRecord = mod_pubsub:pubsub_node()
%% @doc <p>No node record is stored on database. Just do nothing.</p>
set_node(_NodeRecord) ->
    ok.

get_node(Host, Node, _From) ->
    get_node(Host, Node).

%% @spec (Host, Node) -> pubsubNode()
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%% @doc <p>Virtual node tree does not handle a node database. Any node is considered
%% as existing. Node record contains default values.</p>
get_node(Host, Node) ->
    #pubsub_node{nodeid = {Host, Node}}.

get_nodes(Key, _From) ->
    get_nodes(Key).

%% @spec (Key) -> [pubsubNode()]
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle a node database. Any node is considered
%% as existing. Nodes list can not be determined.</p>
get_nodes(_Key) ->
    [].

%% @spec (Host, Node, From) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
get_subnodes(_Host, _Node, _From) ->
    [].

%% @spec (Host, Index) -> [pubsubNode()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%% @doc <p>Virtual node tree does not handle parent/child. Child list is empty.</p>
get_subnodes_tree(_Host, _Node) ->
    [].

%% @spec (Host, Node, Type, Owner, Options) -> ok
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     Type = mod_pubsub:nodeType()
%%     Owner = mod_pubsub:jid()
%%     Options = list()
%% @doc <p>No node record is stored on database. Any valid node
%% is considered as already created.</p>
%% <p>default allowed nodes: /home/host/user/any/node/name</p>
create_node(_Host, Node, _Type, Owner, _Options) ->
    UserName = Owner#jid.luser,
    UserHost = Owner#jid.lserver,
    case Node of
	["home", UserHost, UserName | _] -> {error, ?ERR_CONFLICT};
	_ -> {error, ?ERR_NOT_ALLOWED}
    end.

%% @spec (Host, Node) -> [mod_pubsub:node()]
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%% @doc <p>Virtual node tree does not handle parent/child.
%% node deletion just affects the corresponding node.</p>
delete_node(_Host, Node) ->
    [Node].

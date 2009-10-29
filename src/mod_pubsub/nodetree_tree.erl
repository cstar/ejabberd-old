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
%%% Portions created by ProcessOne are Copyright 2006-2009, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2009, ProcessOne.
%%%
%%%
%%% @copyright 2006-2009 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_tree).
-author('christophe.romain@process-one.net').

-include_lib("stdlib/include/qlc.hrl").

-include("pubsub.hrl").
-include("jlib.hrl").
-include("ejabberd.hrl").
-behaviour(gen_pubsub_nodetree).

-export([init/3,
	 terminate/2,
	 options/0,
	 set_node/1,
	 get_node/3,
	 get_node/2,
	 get_node/1,
	 get_nodes/2,
	 get_nodes/1,
	 get_parentnodes/3,
	 get_parentnodes_tree/3,
	 get_subnodes/3,
	 get_subnodes_tree/3,
	 create_node/6,
	 delete_node/2,
	 make_key/1
	]).

-define(PREFIX, "node:").

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
init(Host, ServerHost, Opts) ->
    Bucket = gen_mod:get_opt(s3_bucket, Opts, ServerHost),
    s3:start(),
    {ok, Buckets} = s3:list_buckets(),
    case lists:member(Bucket, Buckets) of 
        false ->
            s3:create_bucket(Bucket),
            ?INFO_MSG("S3 bucket ~s created", [Bucket]);
        true -> ok
    end,
    ets:insert(gen_mod:get_module_proc(Host, config), {s3_bucket, Bucket}),
    ets:insert(gen_mod:get_module_proc(ServerHost, config), {s3_bucket, Bucket}),
    ok.
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%     Option = mod_pubsub:nodetreeOption()
%% @doc Returns the default pubsub node tree options.
options() ->
    [{virtual_tree, false}].

%% @spec (NodeRecord) -> ok | {error, Reason}
%%     Record = mod_pubsub:pubsub_node()
set_node(#pubsub_node{nodeid = NodeId}=N)->
    Key = make_key(NodeId),
    s3:write_term(get_bucket(NodeId), Key, N),
    ok;
    
set_node(_) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR}.

make_key({{_U, _S, _R}=JID, []})->
    SHost = jlib:jid_to_string(JID),
    ?PREFIX++SHost ++ ":";
make_key({{_U, _S, _R}=JID, Node})->
    SNode = mod_pubsub:node_to_string(Node),
    SHost = jlib:jid_to_string(JID),
    ?PREFIX++SHost ++ ":" ++ SNode;
make_key({Host, []})->
    ?PREFIX++Host ++ ":";
make_key({Host, Node})->
    SNode = mod_pubsub:node_to_string(Node),
    ?PREFIX++Host ++ ":" ++ SNode.
parse_key(?PREFIX++Key) ->
    [Host| R] = string:tokens(Key, ":"),
    {Host, mod_pubsub:string_to_node(string:join(R, ":"))}.
    
    
get_bucket({_U, S, _R})->
    get_bucket(S);
get_bucket({{_U, S, _R}, _Node})->
    get_bucket(S);
get_bucket({Host, _Node})->
    get_bucket(Host);
get_bucket(Host) when is_list(Host)->
    [{s3_bucket, Bucket}] =  ets:lookup(gen_mod:get_module_proc(Host, config), s3_bucket),
    Bucket;
get_bucket(Host)->
    ?ERROR_MSG("Unsupported host format : ~p", [Host]).

get_node(Host, Node, _From) ->
    get_node(Host, Node).

%% @spec (Host, Node) -> pubsubNode() | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
get_node(Host, Node) ->
    Key = make_key({Host, Node}),
    case s3:read_object(get_bucket(Host), Key) of
        {ok, {Conf, _H}}->
            R = binary_to_term(list_to_binary(Conf)),
            ?DEBUG("Got node : ~p", [R]),
            R;
        _ -> 
            {error, ?ERR_ITEM_NOT_FOUND}
    end.  
get_node({Host, Node}) ->
  get_node(Host, Node).

get_nodes(Host, _From) ->
    get_nodes(Host).

%% @spec (Host) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host() | mod_pubsub:jid()
get_nodes(Host) ->
    K=make_key({Host, []}),
    Nodes =  s3:get_objects(get_bucket(Host), [{prefix, K}]),
    lists:map(fun({_K, Bin, _H})-> binary_to_term(list_to_binary(Bin)) end, Nodes).

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | mod_pubsub:jid()
%%     Node   = mod_pubsub:pubsubNode()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return empty list.</p>
get_parentnodes(_Host, _Node, _From) ->
    [].

%% @spec (Host, Node, From) -> [{Depth, Record}] | {error, Reason}
%%     Host   = mod_pubsub:host() | mod_pubsub:jid()
%%     Node   = mod_pubsub:pubsubNode()
%%     From   = mod_pubsub:jid()
%%     Depth  = integer()
%%     Record = pubsubNode()
%% @doc <p>Default node tree does not handle parents, return a list
%% containing just this node.</p>
get_parentnodes_tree(Host, Node, From) ->
    case get_node(Host, Node, From) of
	N when is_record(N, pubsub_node) -> [{0, [N]}];
	_Error -> []
    end.

%% @spec (Host, Node, From) -> [pubsubNode()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
%%     From = mod_pubsub:jid()
get_subnodes(Host, Node, _From) ->
    Key=make_key({Host, Node}),
    K = case lists:reverse(Key) of
        [$/|_R] -> Key;
        _ -> Key ++ "/"
    end,
    Nodes = s3:get_objects(get_bucket(Host), [{prefix, K}, {delimiter, "/"}]),
    lists:map(fun({_K, Bin, _H})-> binary_to_term(list_to_binary(Bin)) end, Nodes).


%% @spec (Host, Index) -> [pubsubNodeIdx()] | {error, Reason}
%%     Host = mod_pubsub:host()
%%     Node = mod_pubsub:pubsubNode()
get_subnodes_tree(Host, Node,_From) ->
    get_subnodes_tree(Host, Node).

get_subnodes_tree(Host, Node)->
    Key=make_key({Host, Node}),
    {ok, Items} =  s3:list_objects(get_bucket(Host), [{prefix, Key}]),
    lists:foldl(fun({object_info,{"Key", K }, _, _,_}, Acc)-> 
            case parse_key(K) of
                {_H, Node}->Acc;
                {_H, N}->[N|Acc]
            end
         end, [], Items).

%% @spec (Key, Node, Type, Owner, Options) -> ok | {error, Reason}
%%     Key = mod_pubsub:host() | mod_pubsub:jid()
%%     Node = mod_pubsub:pubsubNode()
%%     NodeType = mod_pubsub:nodeType()
%%     Owner = mod_pubsub:jid()
%%     Options = list()
create_node(Key, Node, Type, Owner, Options, Parents) ->
    BJID = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    case get_node(Key, Node) of
	{error, ?ERR_ITEM_NOT_FOUND} ->
	    {ParentNode, ParentExists} =
		case Key of
		    {_U, _S, _R} ->
			%% This is special case for PEP handling
			%% PEP does not uses hierarchy
			{[], true};
		    _ ->
			case Parents of
			[] -> true;
			[Parent|_] ->
			    case get_node(Key, Parent) of
				{error, ?ERR_ITEM_NOT_FOUND} -> false;
				#pubsub_node{owners = Owners} -> lists:member(BJID, Owners);
				_ ->  false
			    end
			end
		end,
	    case ParentExists of
		true ->
		    %% Service requires registration
		    %%{error, ?ERR_REGISTRATION_REQUIRED};
		    %NodeId = pubsub_index:new(node),
		    set_node(#pubsub_node{nodeid = {Key, Node},
					      parents = Parents,
					      id = {Key, Node},
					      type = Type,
					      owners = [BJID],
					      options = Options}),
			{ok, {Key, Node}};
		false ->
		    %% Requesting entity is prohibited from creating nodes
		    {error, ?ERR_FORBIDDEN}
	    end;
	_ ->
	    %% NodeID already exists
	    {error, ?ERR_CONFLICT}
    end.

%% @spec (Key, Node) -> [mod_pubsub:node()]
%%     Key = mod_pubsub:host() | mod_pubsub:jid()
%%     Node = mod_pubsub:pubsubNode()
delete_node(Key, Node) ->
    Removed = get_subnodes_tree(Key, Node),
    lists:foreach(fun(N) ->
            K = make_key({Key, N}),
		    s3:delete_object(get_bucket(Key), K)
		  end, Removed),
    Removed.

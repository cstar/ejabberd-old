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
%%% @copyright 2006-2008 Process-one
%%% @author Christophe romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(node_zoo).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2,
	 options/0, features/0,
	 create_node_permission/6,
	 create_node/3,
	 delete_node/2,
	 purge_node/3,
	 subscribe_node/8,
	 unsubscribe_node/5,
	 publish_item/7,
	 delete_item/4,
	 remove_extra_items/4,
	 get_entity_affiliations/2,
	 get_node_affiliations/2,
	 get_affiliation/3,
	 set_affiliation/4,
	 get_entity_subscriptions/2,
	 get_node_subscriptions/2,
	 get_subscription/3,
	 set_subscription/4,
	 get_states/2,
	 get_state/3,
	 set_state/1,
	 get_items/7,
	 get_items/2,
	 get_item/8,
	 get_item/3,
	 set_item/1
	]).


init(Host, ServerHost, Opts) ->
    node_default:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_default:terminate(Host, ServerHost).

options() ->
    [{node_type, zoo},
     {deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, true},
     {persist_items, true},
     {max_items, ?MAXITEMS div 2},
     {subscribe, true},
     {access_model, open},
     {roster_groups_allowed, []},
     {publish_model, publishers},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, never},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

features() ->
    node_default:features().

%% use same code as node_default, but do not limite node to
%% the home/localhost/user/... hierarchy
%% any node is allowed
create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    LOwner = jlib:jid_tolower(Owner),
    {User, Server, _Resource} = LOwner, 
    Allowed = case acl:match_rule(ServerHost, Access, LOwner) of
		allow ->
		    true;
		_ ->    
		    case Owner of
		    {jid, "", _, "", "", _, ""} -> true;
		    _ -> false
		    end     
		end,    
    {result, Allowed}.

create_node(Host, Node, Owner) ->
    node_default:create_node(Host, Node, Owner).

delete_node(Host, Removed) ->
    node_default:delete_node(Host, Removed).

subscribe_node(Host, Node, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup) ->
    node_default:subscribe_node(Host, Node, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup).

unsubscribe_node(Host, Node, Sender, Subscriber, SubID) ->
    node_default:unsubscribe_node(Host, Node, Sender, Subscriber, SubID).

publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload) ->
    node_default:publish_item(Host, Node, Publisher, Model, MaxItems, ItemId, Payload).

remove_extra_items(Host, Node, MaxItems, ItemIds) ->
    node_default:remove_extra_items(Host, Node, MaxItems, ItemIds).

delete_item(Host, Node, JID, ItemId) ->
    node_default:delete_item(Host, Node, JID, ItemId).

purge_node(Host, Node, Owner) ->
    node_default:purge_node(Host, Node, Owner).

get_entity_affiliations(Host, Owner) ->
    node_default:get_entity_affiliations(Host, Owner).

get_node_affiliations(Host, Node) ->
    node_default:get_node_affiliations(Host, Node).

get_affiliation(Host, Node, Owner) ->
    node_default:get_affiliation(Host, Node, Owner).

set_affiliation(Host, Node, Owner, Affiliation) ->
    node_default:set_affiliation(Host, Node, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_default:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(Host, Node) ->
    node_default:get_node_subscriptions(Host, Node).

get_subscription(Host, Node, Owner) ->
    node_default:get_subscription(Host, Node, Owner).

set_subscription(Host, Node, Owner, Subscription) ->
    node_default:set_subscription(Host, Node, Owner, Subscription).

get_states(Host, Node) ->
    node_default:get_states(Host, Node).

get_state(Host, Node, JID) ->
    node_default:get_state(Host, Node, JID).

set_state(State) ->
    node_default:set_state(State).

get_items(Host, Node) ->
    node_default:get_items(Host, Node).

get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_default:get_items(Host, Node, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

get_item(Host, Node, ItemId) ->
    node_default:get_item(Host, Node, ItemId).
       
get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_default:get_item(Host, Node, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_default:set_item(Item).

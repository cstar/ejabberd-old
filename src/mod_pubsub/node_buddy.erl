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
%%% @author Christophe romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(node_buddy).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").

-behaviour(gen_pubsub_node).

%% Note on function definition
%%   included is all defined plugin function
%%   it's possible not to define some function at all
%%   in that case, warning will be generated at compilation
%%   and function call will fail,
%%   then mod_pubsub will call function from node_default
%%   (this makes code cleaner, but execution a little bit longer)

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
	 get_items/2,
	 get_item/3,
	 set_item/1
	]).


init(Host, ServerHost, Opts) ->
    node_default:init(Host, ServerHost, Opts).

terminate(Host, ServerHost) ->
    node_default:terminate(Host, ServerHost).

options() ->
    [{node_type, buddy},
     {deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, true},
     {persist_items, true},
     {max_items, ?MAXITEMS div 2},
     {subscribe, true},
     {access_model, presence},
     {roster_groups_allowed, []},
     {publish_model, publishers},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, never},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

features() ->
    ["create-nodes",
     "delete-nodes",
     "instant-nodes",
     "item-ids",
     "outcast-affiliation",
     "persistent-items",
     "publish",
     "purge-nodes",
     "retract-items",
     "retrieve-affiliations",
     "retrieve-items",
     "retrieve-subscriptions",
     "subscribe",
     "subscription-notifications"
    ].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_default:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

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

get_item(Host, Node, ItemId) ->
    node_default:get_items(Host, Node, ItemId).

set_item(Item) ->
    node_default:set_item(Item).

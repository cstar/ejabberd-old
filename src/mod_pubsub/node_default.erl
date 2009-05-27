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

%%% @todo The item table should be handled by the plugin, but plugin that do
%%% not want to manage it should be able to use the default behaviour.
%%% @todo Plugin modules should be able to register to receive presence update
%%% send to pubsub.

%%% @doc The module <strong>{@module}</strong> is the default PubSub plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node
%%% types.</p>
%%% <p>PubSub plugin nodes are using the {@link gen_node} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% useable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(node_default).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").
-include("jlib.hrl").
-include("ejabberd.hrl").
-behaviour(gen_pubsub_node).

%% API definition
-export([init/3, terminate/2,
	 options/0, features/0,
	 create_node_permission/6,
	 create_node/2,
	 delete_node/1,
	 purge_node/2,
	 subscribe_node/7,
	 unsubscribe_node/4,
	 publish_item/6,
	 delete_item/4,
	 remove_extra_items/3,
	 get_entity_affiliations/2,
	 get_node_affiliations/1,
	 get_affiliation/2,
	 set_affiliation/3,
	 get_entity_subscriptions/2,
	 get_node_subscriptions/1,
	 get_subscription/2,
	 set_subscription/3,
	 get_states/1,
	 get_state/2,
	 set_state/1,
	 get_items/6,
	 get_items/2,
	 get_item/7,
	 get_item/2,
	 set_item/1,
	 get_item_name/3
	]).
-define(DOMAIN, "pubsub").
-define(PREFIX, "item:").

%% ================
%% API definition
%% ================

%% @spec (Host, ServerHost, Opts) -> any()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = mod_pubsub:host()
%%	 Opts = list()
%% @doc <p>Called during pubsub modules initialisation. Any pubsub plugin must
%% implement this function. It can return anything.</p>
%% <p>This function is mainly used to trigger the setup task necessary for the
%% plugin. It can be used for example by the developer to create the specific
%% module database schema if it does not exists yet.</p>
init(Host, ServerHost, Opts) ->
    erlsdb:start(),
    Bucket = gen_mod:get_opt(s3_tree_bucket, Opts, ServerHost),
    s3:start(),
    {ok, Buckets} = s3:list_buckets(),
    case lists:member(Bucket, Buckets) of 
        false ->
            s3:create_bucket(Bucket),
            ?INFO_MSG("S3 bucket ~s created", [Bucket]);
        true -> ok
    end,
    ets:insert(gen_mod:get_module_proc(Host, pubsub_state), {s3_bucket, Bucket}),
    ets:insert(gen_mod:get_module_proc(ServerHost, pubsub_state), {s3_bucket, Bucket}),
    {ok, Domains, _Token}  = erlsdb:list_domains(),
    case lists:member(?DOMAIN, Domains) of 
        false ->
            erlsdb:create_domain(?DOMAIN),
            ?INFO_MSG("SimpleDB domain ~s created", [?DOMAIN]);
        true -> ok
    end,
    ok.

%% @spec (Host, ServerHost) -> any()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = host()
%% @doc <p>Called during pubsub modules termination. Any pubsub plugin must
%% implement this function. It can return anything.</p>
terminate(_Host, _ServerHost) ->
    ok.

%% @spec () -> [Option]
%%	 Option = mod_pubsub:nodeOption()
%% @doc Returns the default pubsub node options.
%% <p>Example of function return value:</p>
%%	```
%%	 [{deliver_payloads, true},
%%	  {notify_config, false},
%%	  {notify_delete, false},
%%	  {notify_retract, true},
%%	  {persist_items, true},
%%	  {max_items, 10},
%%	  {subscribe, true},
%%	  {access_model, open},
%%	  {publish_model, publishers},
%%	  {max_payload_size, 100000},
%%	  {send_last_published_item, never},
%%	  {presence_based_delivery, false}]'''
options() ->
    [{node_type, default},
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
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

%% @spec () -> []
%% @doc Returns the node features
features() ->
    ["create-nodes",
     "auto-create",
     "delete-nodes",
     "delete-items",
     "instant-nodes",
     "manage-subscriptions",
     "modify-affiliations",
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

%% @spec (Host, ServerHost, Node, ParentNode, Owner, Access) -> bool()
%%	 Host = mod_pubsub:host()
%%	 ServerHost = mod_pubsub:host()
%%	 Node = mod_pubsub:pubsubNode()
%%	 ParentNode = mod_pubsub:pubsubNode()
%%	 Owner = mod_pubsub:jid()
%%	 Access = all | atom()
%% @doc Checks if the current user has the permission to create the requested node
%% <p>In {@link node_default}, the permission is decided by the place in the
%% hierarchy where the user is creating the node. The access parameter is also
%% checked in the default module. This parameter depends on the value of the
%% <tt>access_createnode</tt> ACL value in ejabberd config file.</p>
%% <p>This function also check that node can be created a a children of its
%% parent node</p>
%% <p>PubSub plugins can redefine the PubSub node creation rights as they
%% which. They can simply delegate this check to the {@link node_default}
%% module by implementing this function like this:
%% ```check_create_user_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
%%	   node_default:check_create_user_permission(Host, ServerHost, Node, ParentNode, Owner, Access).'''</p>
create_node_permission(Host, ServerHost, Node, _ParentNode, Owner, Access) ->
    LOwner = jlib:jid_tolower(Owner),
    {User, Server, _Resource} = LOwner,
    Allowed = case LOwner of
	{"", Host, ""} ->
	    true; % pubsub service always allowed
	_ ->
	    case acl:match_rule(ServerHost, Access, LOwner) of
		allow ->
		    case Node of
			["home", Server, User | _] -> true;
			_ -> false
		    end;
		_ ->
		    false
	    end
    end,
    {result, Allowed}.

%% @spec (NodeId, Owner) ->
%%		  {result, Result} | exit
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Owner = mod_pubsub:jid()
%% @doc <p></p>
create_node(NodeId, Owner) ->
    OwnerKey = jlib:jid_tolower(jlib:jid_remove_resource(Owner)),
    ?DEBUG("create node : ~p",[#pubsub_state{stateid = {OwnerKey, NodeId}, affiliation = owner}]),
    set_state(#pubsub_state{stateid = {OwnerKey, NodeId}, affiliation = owner}),
    {result, {default, broadcast}}.

%% @spec (Removed) -> ok
%%	 Removed = [mod_pubsub:pubsubNode()]
%% @doc <p>purge items of deleted nodes after effective deletion.</p>
delete_node(Removed) ->
    Tr = fun(#pubsub_state{stateid = {J, _}, subscription = S}) ->
		 {J, S}
	 end,
    Reply = lists:map(
	fun(#pubsub_node{id = NodeId} = PubsubNode) ->
	    {result, States} = get_states(NodeId),
	    lists:foreach(
		fun(#pubsub_state{stateid = {LJID, _}, items = Items}) ->
		    del_items(NodeId, Items),
		    del_state(NodeId, LJID)
	    end, States),
	    {PubsubNode, lists:map(Tr, States)}
	end, Removed),
    {result, {default, broadcast, Reply}}.

%% @spec (NodeId, Sender, Subscriber, AccessModel, SendLast, PresenceSubscription, RosterGroup) ->
%%		 {error, Reason} | {result, Result}
%% @doc <p>Accepts or rejects subcription requests on a PubSub node.</p>
%% <p>The mechanism works as follow:
%% <ul>
%% <li>The main PubSub module prepares the subscription and passes the
%% result of the preparation as a record.</li>
%% <li>This function gets the prepared record and several other parameters and
%% can decide to:<ul>
%%  <li>reject the subscription;</li>
%%  <li>allow it as is, letting the main module perform the database
%%  persistance;</li>
%%  <li>allow it, modifying the record. The main module will store the
%%  modified record;</li>
%%  <li>allow it, but perform the needed persistance operations.</li></ul>
%% </li></ul></p>
%% <p>The selected behaviour depends on the return parameter:
%%  <ul>
%%   <li><tt>{error, Reason}</tt>: an IQ error result will be returned. No
%%   subscription will actually be performed.</li>
%%   <li><tt>true</tt>: Subscribe operation is allowed, based on the
%%   unmodified record passed in parameter <tt>SubscribeResult</tt>. If this
%%   parameter contains an error, no subscription will be performed.</li>
%%   <li><tt>{true, PubsubState}</tt>: Subscribe operation is allowed, but
%%   the {@link mod_pubsub:pubsubState()} record returned replaces the value
%%   passed in parameter <tt>SubscribeResult</tt>.</li>
%%   <li><tt>{true, done}</tt>: Subscribe operation is allowed, but the
%%   {@link mod_pubsub:pubsubState()} will be considered as already stored and
%%   no further persistance operation will be performed. This case is used,
%%   when the plugin module is doing the persistance by itself or when it want
%%   to completly disable persistance.</li></ul>
%% </p>
%% <p>In the default plugin module, the record is unchanged.</p>
subscribe_node(NodeId, Sender, Subscriber, AccessModel,
	       SendLast, PresenceSubscription, RosterGroup) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized = (jlib:jid_tolower(jlib:jid_remove_resource(Sender)) == GenKey),
    GenState = get_state(NodeId, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(NodeId, SubKey)
	end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = SubState#pubsub_state.subscription,
    Whitelisted = lists:member(Affiliation, [member, publisher, owner]),
    if
	not Authorized ->
	    %% JIDs do not match
	    {error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "invalid-jid")};
	Affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, ?ERR_FORBIDDEN};
	Subscription == pending ->
	    %% Requesting entity has pending subscription
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "pending-subscription")};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-in-roster-group")};
	(AccessModel == whitelist) and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {error, ?ERR_EXTENDED(?ERR_NOT_ALLOWED, "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	%%ForbiddenAnonymous ->
	%%	% Requesting entity is anonymous
	%%	{error, ?ERR_FORBIDDEN};
	true ->
	    NewSubscription =
		if
		    AccessModel == authorize ->
			pending;
		    %%NeedConfiguration ->
		    %%	unconfigured
		    true ->
			subscribed
		end,
	    set_state(SubState#pubsub_state{subscription = NewSubscription}),
	    case NewSubscription of
		subscribed ->
		    case SendLast of
			never -> {result, {default, NewSubscription}};
			_ -> {result, {default, NewSubscription, send_last}}
		    end;
		_ ->
		    {result, {default, NewSubscription}}
	    end
    end.

%% @spec (NodeId, Sender, Subscriber, SubID) ->
%%			{error, Reason} | {result, []}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Sender = mod_pubsub:jid()
%%	 Subscriber = mod_pubsub:jid()
%%	 SubID = string()
%%	 Reason = mod_pubsub:stanzaError()
%% @doc <p>Unsubscribe the <tt>Subscriber</tt> from the <tt>Node</tt>.</p>
unsubscribe_node(NodeId, Sender, Subscriber, _SubId) ->
    SubKey = jlib:jid_tolower(Subscriber),
    GenKey = jlib:jid_remove_resource(SubKey),
    Authorized = (jlib:jid_tolower(jlib:jid_remove_resource(Sender)) == GenKey),
    GenState = get_state(NodeId, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(NodeId, SubKey)
	end,
    if
	%% Requesting entity is prohibited from unsubscribing entity
	not Authorized ->
	    {error, ?ERR_FORBIDDEN};
	%% Entity did not specify SubID
	%%SubID == "", ?? ->
	%%	{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%% Invalid subscription identifier
	%%InvalidSubID ->
	%%	{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	%% Requesting entity is not a subscriber
	SubState#pubsub_state.subscription == none ->
	    {error, ?ERR_EXTENDED(?ERR_UNEXPECTED_REQUEST, "not-subscribed")};
	%% Was just subscriber, remove the record
	SubState#pubsub_state.affiliation == none ->
	    del_state(NodeId, SubKey),
	    {result, default};
	true ->
	    set_state(SubState#pubsub_state{subscription = none}),
	    {result, default}
    end.

%% @spec (NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
%%		 {true, PubsubItem} | {result, Reply}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Publisher = mod_pubsub:jid()
%%	 PublishModel = atom()
%%	 MaxItems = integer()
%%	 ItemId = string()
%%	 Payload = term()
%% @doc <p>Publishes the item passed as parameter.</p>
%% <p>The mechanism works as follow:
%% <ul>
%% <li>The main PubSub module prepares the item to publish and passes the
%% result of the preparation as a {@link mod_pubsub:pubsubItem()} record.</li>
%% <li>This function gets the prepared record and several other parameters and can decide to:<ul>
%%  <li>reject the publication;</li>
%%  <li>allow the publication as is, letting the main module perform the database persistance;</li>
%%  <li>allow the publication, modifying the record. The main module will store the modified record;</li>
%%  <li>allow it, but perform the needed persistance operations.</li></ul>
%% </li></ul></p>
%% <p>The selected behaviour depends on the return parameter:
%%  <ul>
%%   <li><tt>{error, Reason}</tt>: an iq error result will be return. No
%%   publication is actually performed.</li>
%%   <li><tt>true</tt>: Publication operation is allowed, based on the
%%   unmodified record passed in parameter <tt>Item</tt>. If the <tt>Item</tt>
%%   parameter contains an error, no subscription will actually be
%%   performed.</li>
%%   <li><tt>{true, Item}</tt>: Publication operation is allowed, but the
%%   {@link mod_pubsub:pubsubItem()} record returned replaces the value passed
%%   in parameter <tt>Item</tt>. The persistance will be performed by the main
%%   module.</li>
%%   <li><tt>{true, done}</tt>: Publication operation is allowed, but the
%%   {@link mod_pubsub:pubsubItem()} will be considered as already stored and
%%   no further persistance operation will be performed. This case is used,
%%   when the plugin module is doing the persistance by itself or when it want
%%   to completly disable persistance.</li></ul>
%% </p>
%% <p>In the default plugin module, the record is unchanged.</p>
publish_item(NodeId, Publisher, PublishModel, MaxItems, ItemId, Payload) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    SubState = case SubKey of
	GenKey -> GenState;
	_ -> get_state(NodeId, SubKey)
	end,
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = SubState#pubsub_state.subscription,
    if
	not ((PublishModel == open)
	     or ((PublishModel == publishers)
		 and ((Affiliation == owner) or (Affiliation == publisher)))
	     or ((PublishModel == subscribers)
		 and (Subscription == subscribed))) ->
	    %% Entity does not have sufficient privileges to publish to node
	    {error, ?ERR_FORBIDDEN};
	true ->
	    %% TODO: check creation, presence, roster
	    if MaxItems > 0 ->
		PubId = {now(), SubKey},
		Item = case get_item(NodeId, ItemId) of
		       {result, OldItem} ->
			   OldItem#pubsub_item{modification = PubId,
					       payload = Payload};
		       _ ->
			   #pubsub_item{itemid = {ItemId, NodeId},
					creation = {now(), GenKey},
					modification = PubId,
					payload = Payload}
		   end,
		Items = [ItemId | GenState#pubsub_state.items--[ItemId]],
		{result, {NI, OI}} = remove_extra_items(NodeId, MaxItems, Items),
		set_item(Item),
		set_state(GenState#pubsub_state{items = NI}),
		{result, {default, broadcast, OI}};
	       true ->
		{result, {default, broadcast, []}}
	    end
    end.

%% @spec (NodeId, MaxItems, ItemIds) -> {NewItemIds,OldItemIds}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 MaxItems = integer() | unlimited
%%	 ItemIds = [ItemId::string()]
%%	 NewItemIds = [ItemId::string()]
%% @doc <p>This function is used to remove extra items, most notably when the
%% maximum number of items has been reached.</p>
%% <p>This function is used internally by the core PubSub module, as no
%% permission check is performed.</p>
%% <p>In the default plugin module, the oldest items are removed, but other
%% rules can be used.</p>
%% <p>If another PubSub plugin wants to delegate the item removal (and if the
%% plugin is using the default pubsub storage), it can implements this function like this:
%% ```remove_extra_items(NodeId, MaxItems, ItemIds) ->
%%	   node_default:remove_extra_items(NodeId, MaxItems, ItemIds).'''</p>
remove_extra_items(_NodeId, unlimited, ItemIds) ->
    {result, {ItemIds, []}};
remove_extra_items(NodeId, MaxItems, ItemIds) ->
    NewItems = lists:sublist(ItemIds, MaxItems),
    OldItems = lists:nthtail(length(NewItems), ItemIds),
    %% Remove extra items:
    del_items(NodeId, OldItems),
    %% Return the new items list:
    {result, {NewItems, OldItems}}.

%% @spec (NodeId, Publisher, PublishModel, ItemId) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, []}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Publisher = mod_pubsub:jid()
%%	 PublishModel = atom()
%%	 ItemId = string()
%% @doc <p>Triggers item deletion.</p>
%% <p>Default plugin: The user performing the deletion must be the node owner
%% or a publisher, or PublishModel being open.</p>
delete_item(NodeId, Publisher, PublishModel, ItemId) ->
    SubKey = jlib:jid_tolower(Publisher),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    #pubsub_state{affiliation = Affiliation, items = Items} = GenState,
    Allowed = (Affiliation == publisher) orelse (Affiliation == owner)
	orelse (PublishModel == open)
	orelse case get_item(NodeId, ItemId) of
		   {result, #pubsub_item{creation = {_, GenKey}}} -> true;
		   _ -> false
	       end,
    if
	not Allowed ->
	    %% Requesting entity does not have sufficient privileges
	    {error, ?ERR_FORBIDDEN};
	true ->
	    case lists:member(ItemId, Items) of
		true ->
		    del_item(NodeId, ItemId),
		    NewItems = lists:delete(ItemId, Items),
		    set_state(GenState#pubsub_state{items = NewItems}),
		    {result, {default, broadcast}};
		false ->
		    %% Non-existent node or item
		    {error, ?ERR_ITEM_NOT_FOUND}
	    end
    end.

%% @spec (NodeId, Owner) ->
%%		  {error, Reason::stanzaError()} |
%%		  {result, {default, broadcast}}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Owner = mod_pubsub:jid()
purge_node(NodeId, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    case GenState of
	#pubsub_state{items = Items, affiliation = owner} ->
	    del_items(NodeId, Items),
	    set_state(GenState#pubsub_state{items = []}),
	    {result, {default, broadcast}};
	_ ->
	    %% Entity is not owner
	    {error, ?ERR_FORBIDDEN}
    end.

%% @spec (Host, JID) -> [{Node,Affiliation}]
%%	 Host = host()
%%	 JID = mod_pubsub:jid()
%% @doc <p>Return the current affiliations for the given user</p>
%% <p>The default module reads affiliations in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_affiliations(Host, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    SGenKey = jlib:jid_to_string(GenKey),
    SHost = host_to_string(Host),
    ?DEBUG("select * from pubsub where  host='"++SHost ++"' and jid='" ++ SGenKey ++ "'", []),
    {ok, Items}=erlsdb:s_all("select * from pubsub where  host='"++SHost ++"' and jid='" ++ SGenKey ++ "%'"),
    NodeTree = case ets:lookup(gen_mod:get_module_proc(Host, pubsub_state), nodetree) of
	    [{nodetree, NT}] -> NT;
	    _ -> nodetree_default
	end,
    Affs = lists:foldl(fun(S,Acc)->
        #pubsub_state{stateid = {_, {_, N}}, affiliation = A} = sdb_to_record(S),
        #pubsub_node{nodeid = {H, _}} = Node = NodeTree:get_node(Host, N), %%ECE Can't do any better. Need to change mod_pubsub otherwise.
        case H of
		    Host -> [{Node, A}|Acc];
		    _ -> Acc
		end
    end, [],  Items),
    {result, Affs}.

get_node_affiliations(NodeId) ->
    {result, States} = get_states(NodeId),
    Tr = fun(#pubsub_state{stateid = {J, _}, affiliation = A}) ->
		 {J, A}
	 end,
    {result, lists:map(Tr, States)}.

get_affiliation(NodeId, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    {result, GenState#pubsub_state.affiliation}.

set_affiliation(NodeId, Owner, Affiliation) ->
    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    case {Affiliation, GenState#pubsub_state.subscription} of
	{none, none} ->
	    del_state(NodeId, GenKey);
	_ ->
	    set_state(GenState#pubsub_state{affiliation = Affiliation})
    end.

%% @spec (Host, Owner) -> [{Node,Subscription}]
%%	 Host = host()
%%	 Owner = mod_pubsub:jid()
%% @doc <p>Return the current subscriptions for the given user</p>
%% <p>The default module reads subscriptions in the main Mnesia
%% <tt>pubsub_state</tt> table. If a plugin stores its data in the same
%% table, it should return an empty list, as the affiliation will be read by
%% the default PubSub module. Otherwise, it should return its own affiliation,
%% that will be added to the affiliation stored in the main
%% <tt>pubsub_state</tt> table.</p>
get_entity_subscriptions(Host, Owner) ->

    SubKey = jlib:jid_tolower(Owner),
    GenKey = jlib:jid_to_string(jlib:jid_remove_resource(SubKey)),
    SHost = host_to_string(Host),
    {ok, Items}=erlsdb:s_all("select * from pubsub where host='"++ SHost ++"' and jid like '"++ GenKey ++"%'"),
	NodeTree = case ets:lookup(gen_mod:get_module_proc(Host, pubsub_state), nodetree) of
	    [{nodetree, NT}] -> NT;
	    _ -> nodetree_default
	end,
	Subs = lists:map(fun(Subs)->
	    #pubsub_state{stateid = {J, {_, N}}, subscription = S} = sdb_to_record(Subs),
	    Node = NodeTree:get_node(Host, N), %%ECE Can't do any better. Need to change mod_pubsub otherwise.
	    {Node, S, J}
	   end, Items),
    {result, Subs}.


get_node_subscriptions(NodeId) ->
    {result, States} = get_states(NodeId),
    Tr = fun(#pubsub_state{stateid = {J, _}, subscription = S}) ->
		 {J, S}
	 end,
    {result, lists:map(Tr, States)}.

get_subscription(NodeId, Owner) ->
    SubKey = jlib:jid_tolower(Owner),
    SubState = get_state(NodeId, SubKey),
    {result, SubState#pubsub_state.subscription}.

set_subscription(NodeId, Owner, Subscription) ->
    SubKey = jlib:jid_tolower(Owner),
    SubState = get_state(NodeId, SubKey),
    case {Subscription, SubState#pubsub_state.affiliation} of
	{none, none} ->
	    del_state(NodeId, SubKey);
	_ ->
	    set_state(SubState#pubsub_state{subscription = Subscription})
    end.

sdb_to_record({_Key, Attrs})->
    str(Attrs, #pubsub_state{stateid={nil, {nil, nil}}});
sdb_to_record(Item)->
    str(Item, #pubsub_state{stateid={nil, {nil,nil}}}).
    
str([],#pubsub_state{}=State) -> 
    State;
str([{"host", V}|Rest], #pubsub_state{stateid={Jid, {_, Node}}}=N)->
    Host = string_to_host(V),
    str(Rest, N#pubsub_state{stateid={Jid, {Host, Node}}});
str([{"node", V}|Rest], #pubsub_state{stateid={Jid, {Host, _}}}=N)->
    Node = mod_pubsub:string_to_node(V),
    str(Rest, N#pubsub_state{stateid={Jid, {Host, Node}}});
str([{"jid", V}|Rest], #pubsub_state{stateid={_, {Host, Node}}}=N)->
    Jid = jlib:jid_tolower(jlib:string_to_jid(V)),
    str(Rest, N#pubsub_state{stateid={Jid, {Host, Node}}});    
str([{"affiliation", V}|Rest], N)->
    str(Rest, N#pubsub_state{affiliation=l2a(V)});
str([{"subscription", V}|Rest], N)->
    str(Rest, N#pubsub_state{subscription=l2a(V)});
str([{_, _ }|Rest], S)->str(Rest, S).

%% @spec (NodeId) -> [States] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%% @doc Returns the list of stored states for a given node.
%% <p>For the default PubSub module, states are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_state table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the states where they wants (for example in a
%% relational database).</p>
%% <p>If a PubSub plugin wants to delegate the states storage to the default node,
%% they can implement this function like this:
%% ```get_states(NodeId) ->
%%	   node_default:get_states(NodeId).'''</p>
get_states({Host, Node}) ->
    SHost = host_to_string(Host),
    SNode = mod_pubsub:node_to_string(Node),
    {ok, R} = erlsdb:s_all("select * from pubsub where host='"++ SHost ++ "'  and node = '" ++ SNode ++ "'"),
    States = lists:map(fun(S)->
        sdb_to_record(S)
    end,R),
    {result, States}.

%% @spec (NodeId, JID) -> [State] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 JID = mod_pubsub:jid()
%%	 State = mod_pubsub:pubsubItems()
%% @doc <p>Returns a state (one state list), given its reference.</p>
get_state({Host, Node}, JID) ->
    {_SJID, _SHost, _SNode, Key} = make_key({JID, {Host, Node}}),
    {ok, Attrs} = erlsdb:get_attributes(?DOMAIN, Key), 
    S = sdb_to_record(Attrs),
    State = S#pubsub_state{stateid = {JID, {Host, Node}}},
    %?DEBUG("state fetched from SDB for JID : ~s / Host: ~s / Node : ~s :~n~p", [SJID, SHost, SNode, State]),
    State.

make_key({JID, {Host, Node}})->
    SNode = mod_pubsub:node_to_string(Node),
    SHost = host_to_string(Host),
    SJID = jlib:jid_to_string(JID),
    {SJID, SHost, SNode, SHost ++ ":" ++ SNode ++ ":" ++ SJID}.

%% @spec (State) -> ok | {error, Reason::stanzaError()}
%%	 State = mod_pubsub:pubsubStates()
%% @doc <p>Write a state into database.</p>
set_state(#pubsub_state{stateid = StateId,
                      affiliation = Aff,
                      subscription = Subs})->
    {SJID, SHost, SNode, Key} = make_key(StateId),
    ?DEBUG("erlsdb:replace_attributes(~p, ~p, ~p).",[ ?DOMAIN, Key, [{"host", SHost}, 
                                      {"node", SNode},
                                      {"jid", SJID},
                                      {"affiliation", a2l(Aff)},
                                      {"subscription", a2l(Subs)}]]),
    erlsdb:replace_attributes(?DOMAIN, Key, [{"host", SHost}, 
                                         {"node", SNode},
                                         {"jid", SJID},
                                         {"affiliation", a2l(Aff)},
                                         {"subscription", a2l(Subs)}]),

    ok;
set_state(_) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @spec (NodeId, JID) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%   JID = mod_pubsub:Jid()
%% @doc <p>Delete a state from database.</p>
del_state(NodeId, JID) ->
    {_SJID, _SHost, _SNode, Key} = make_key({JID, NodeId}),
    erlsdb:delete_item(?DOMAIN,Key),
    ok.

%% @spec (NodeId, From) -> [Items] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 Items = mod_pubsub:pubsubItems()
%% @doc Returns the list of stored items for a given node.
%% <p>For the default PubSub module, items are stored in Mnesia database.</p>
%% <p>We can consider that the pubsub_item table have been created by the main
%% mod_pubsub module.</p>
%% <p>PubSub plugins can store the items where they wants (for example in a
%% relational database), or they can even decide not to persist any items.</p>
%% <p>If a PubSub plugin wants to delegate the item storage to the default node,
%% they can implement this function like this:
%% ```get_items(NodeId, From) ->
%%	   node_default:get_items(NodeId, From).'''</p>
get_items({Host, Node}, _From) ->
    Items = s3:get_objects(get_bucket(Host), [{prefix,build_key(Host, Node,"")}] ),
    Items2 = lists:map(fun({_K, Conf, _H})->
        binary_to_term(list_to_binary(Conf))
    end, Items),
    {result, lists:reverse(lists:keysort(#pubsub_item.modification, Items2))}.

get_items(NodeId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = GenState#pubsub_state.subscription,
    Whitelisted = can_fetch_item(Affiliation, Subscription),
    if
	%%SubID == "", ?? ->
	    %% Entity has multiple subscriptions to the node but does not specify a subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubID ->
	    %% Entity is subscribed but specifies an invalid subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	GenState#pubsub_state.affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, ?ERR_FORBIDDEN};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-in-roster-group")};
	(AccessModel == whitelist) and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {error, ?ERR_EXTENDED(?ERR_NOT_ALLOWED, "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_items(NodeId, JID)
    end.

build_key(Host, Node,ItemId)->
    SHost = host_to_string(Host),
    SNode = mod_pubsub:node_to_string(Node),
    ?PREFIX++":"++SHost++":"++SNode++":"++ItemId.
build_key(#pubsub_item{itemid={ItemId, {Host, Node}}})->
    build_key(Host, Node,ItemId).


%% @spec (NodeId, ItemId) -> [Item] | []
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 ItemId = string()
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Returns an item (one item list), given its reference.</p>
get_item({Host, Node}, ItemId) ->
    case s3:read_object(get_bucket(Host), build_key(Host, Node,ItemId)) of
        {ok, {Conf, _H}}->
            {result, binary_to_term(list_to_binary(Conf))};
        _ -> 
            {error, ?ERR_ITEM_NOT_FOUND}
    end.
get_item(NodeId, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, _SubId) ->
    SubKey = jlib:jid_tolower(JID),
    GenKey = jlib:jid_remove_resource(SubKey),
    GenState = get_state(NodeId, GenKey),
    Affiliation = GenState#pubsub_state.affiliation,
    Subscription = GenState#pubsub_state.subscription,
    Whitelisted = can_fetch_item(Affiliation, Subscription),
    if
	%%SubID == "", ?? ->
	    %% Entity has multiple subscriptions to the node but does not specify a subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_BAD_REQUEST, "subid-required")};
	%%InvalidSubID ->
	    %% Entity is subscribed but specifies an invalid subscription ID
	    %{error, ?ERR_EXTENDED(?ERR_NOT_ACCEPTABLE, "invalid-subid")};
	GenState#pubsub_state.affiliation == outcast ->
	    %% Requesting entity is blocked
	    {error, ?ERR_FORBIDDEN};
	(AccessModel == presence) and (not PresenceSubscription) ->
	    %% Entity is not authorized to create a subscription (presence subscription required)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "presence-subscription-required")};
	(AccessModel == roster) and (not RosterGroup) ->
	    %% Entity is not authorized to create a subscription (not in roster group)
	    {error, ?ERR_EXTENDED(?ERR_NOT_AUTHORIZED, "not-in-roster-group")};
	(AccessModel == whitelist) and (not Whitelisted) ->
	    %% Node has whitelist access model and entity lacks required affiliation
	    {error, ?ERR_EXTENDED(?ERR_NOT_ALLOWED, "closed-node")};
	(AccessModel == authorize) -> % TODO: to be done
	    %% Node has authorize access model
	    {error, ?ERR_FORBIDDEN};
	%%MustPay ->
	%%	% Payment is required for a subscription
	%%	{error, ?ERR_PAYMENT_REQUIRED};
	true ->
	    get_item(NodeId, ItemId)
    end.

%% @spec (Item) -> ok | {error, Reason::stanzaError()}
%%	 Item = mod_pubsub:pubsubItems()
%% @doc <p>Write an item into database.</p>
set_item(#pubsub_item{itemid={_,NodeId}}=Item) when is_record(Item, pubsub_item) ->
    spawn(fun()->
        s3:write_object(get_bucket(NodeId), build_key(Item), term_to_binary(Item), "application/erlang")
    end),
    ok;
set_item(_) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR}.

%% @spec (NodeId, ItemId) -> ok | {error, Reason::stanzaError()}
%%	 NodeId = mod_pubsub:pubsubNodeId()
%%	 ItemId = string()
%% @doc <p>Delete an item from database.</p>
del_item({Host, Node}=NodeId, ItemId) ->
    spawn(fun()->
        s3:delete_object(get_bucket(NodeId), build_key(Host, Node, ItemId))
    end),
    ok.
del_items(NodeId, ItemIds) ->
    lists:foreach(fun(ItemId) ->
	del_item(NodeId, ItemId)
    end, ItemIds).

%% @doc <p>Return the name of the node if known: Default is to return
%% node id.</p>
get_item_name(_Host, _Node, Id) ->
    Id.

%% @spec (Affiliation, Subscription) -> true | false
%%       Affiliation = owner | member | publisher | outcast | none
%%       Subscription = subscribed | none
%% @doc Determines if the combination of Affiliation and Subscribed
%% are allowed to get items from a node.
can_fetch_item(owner,        _)             -> true;
can_fetch_item(member,       _)             -> true;
can_fetch_item(publisher,    _)             -> true;
can_fetch_item(outcast,      _)             -> false;
can_fetch_item(none,         subscribed)    -> true;
can_fetch_item(none,         none)          -> false;
can_fetch_item(_Affiliation, _Subscription) -> false.

%% @spec (NodeId) -> Node
%% @doc retreive pubsubNode() representation giving a NodeId
%get_nodename(NodeId) ->
%    case mnesia:index_read(pubsub_node, NodeId, #pubsub_node.id) of
%	[#pubsub_node{nodeid = {_, Node}}] -> Node;
%	_ -> []
%    end.
    
a2l(Atom) when is_atom(Atom)->atom_to_list(Atom).
l2a("member")->member;
l2a("owner")->owner;
l2a("publisher")->publisher;
l2a("outcast")->outcast;
l2a("subscribed")->subscribed;
l2a("none")->none.

host_to_string({_, _, _}=Host)->
    jlib:jid_to_string(Host);
host_to_string(Host) when is_list(Host)-> Host.
string_to_host(Host) when is_list(Host)->
   case jlib:jid_tolower(jlib:string_to_jid(Host)) of
        {[], PubSub, []} -> PubSub;
        JID -> JID
    end.
    
get_bucket({_U, S, _R})->
    get_bucket(S);
get_bucket({{_U, S, _R}, _Node})->
    get_bucket(S);
get_bucket({Host, _Node})->
    get_bucket(Host);
get_bucket(Host) when is_list(Host)->
    [{s3_bucket, Bucket}] =  ets:lookup(gen_mod:get_module_proc(Host, pubsub_state), s3_bucket),
    Bucket;
get_bucket(Host)->
    ?ERROR_MSG("Unsupported host format : ~p", [Host]).



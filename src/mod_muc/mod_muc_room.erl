%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_muc_room).
-author('alexey@process-one.net').

-behaviour(gen_fsm).


%% External exports
-export([start_link/9,
	 start_link/7,
	 start/9,
	 start/7,
	 route/4]).

%% gen_fsm callbacks
-export([init/1,
	 normal_state/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_muc_room.hrl").

-define(MAX_USERS_DEFAULT_LIST,
	[5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, 
	gen_fsm:start(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				RoomShaper, Creator, Nick, DefRoomOpts],
		      ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, 
	Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
	supervisor:start_child(
	  Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		       Creator, Nick, DefRoomOpts])).
-endif.


-define(ERR(Packet,Type, Lang, ErrText),
    exmpp_stanza:error(Packet#xmlel.ns,
                                   Type,
                                   {Lang, translate:translate(Lang, ErrText)})).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts) ->
    ?SUPERVISOR_START.

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(
      Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		   Opts]).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
	   Creator, Nick, DefRoomOpts) ->
    gen_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Creator, Nick, DefRoomOpts],
		       ?FSMOPTS).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    gen_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Opts],
		       ?FSMOPTS).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Creator, _Nick, DefRoomOpts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_affiliation(Creator, owner,
			    #state{host = Host,
				   server_host = ServerHost,
				   access = Access,
				   room = Room,
				   history = lqueue_new(HistorySize),
				   jid = exmpp_jid:make_jid(Room, Host),
				   just_created = true,
				   room_shaper = Shaper}),
    State1 = set_opts(DefRoomOpts, State),
    ?INFO_MSG("Created MUC room ~s@~s by ~s", 
	      [Room, Host, exmpp_jid:jid_to_list(Creator)]),
    {ok, normal_state, State1};
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_opts(Opts, #state{host = Host,
				  server_host = ServerHost,
				  access = Access,
				  room = Room,
				  history = lqueue_new(HistorySize),
				  jid = exmpp_jid:make_jid(Room, Host),
				  room_shaper = Shaper}),
    {ok, normal_state, State}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
normal_state({route, From, undefined,
	      #xmlel{name = 'message'} = Packet},
	     StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    case exmpp_message:get_type(Packet) of
		groupchat ->
		    Activity = get_user_activity(From, StateData),
		    Now = now_to_usec(now()),
		    MinMessageInterval =
			trunc(gen_mod:get_module_opt(
				StateData#state.server_host,
				mod_muc, min_message_interval, 0) * 1000000),
		    Size = lists:flatlength(exmpp_xml:document_to_list(Packet)),
		    {MessageShaper, MessageShaperInterval} =
			shaper:update(Activity#activity.message_shaper, Size),
		    if
			Activity#activity.message /= undefined ->
			    ErrText = "Traffic rate limit is exceeded",
			    Err = exmpp_stanza:error(Packet#xmlel.ns,
			      'resource-constraint',
			      {Lang, translate:translate(Lang, ErrText)}),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, exmpp_stanza:reply_with_error(Packet, Err)),
			    {next_state, normal_state, StateData};
			Now >= Activity#activity.message_time + MinMessageInterval,
			MessageShaperInterval == 0 ->
			    {RoomShaper, RoomShaperInterval} =
				shaper:update(StateData#state.room_shaper, Size),
			    RoomQueueEmpty = queue:is_empty(
					       StateData#state.room_queue),
			    if
				RoomShaperInterval == 0,
				RoomQueueEmpty ->
				    NewActivity = Activity#activity{
						    message_time = Now,
						    message_shaper = MessageShaper},
				    StateData1 =
					store_user_activity(
					  From, NewActivity, StateData),
				    StateData2 =
					StateData1#state{
					  room_shaper = RoomShaper},
				    process_groupchat_message(From, Packet, StateData2);
				true ->
				    StateData1 =
					if
					    RoomQueueEmpty ->
						erlang:send_after(
						  RoomShaperInterval, self(),
						  process_room_queue),
						StateData#state{
						  room_shaper = RoomShaper};
					    true ->
						StateData
					end,
				    NewActivity = Activity#activity{
						    message_time = Now,
						    message_shaper = MessageShaper,
						    message = Packet},
				    RoomQueue = queue:in(
						  {message, From},
						  StateData#state.room_queue),
				    StateData2 =
					store_user_activity(
					  From, NewActivity, StateData1),
				    StateData3 =
					StateData2#state{
					  room_queue = RoomQueue},
				    {next_state, normal_state, StateData3}
			    end;
			true ->
			    MessageInterval =
				(Activity#activity.message_time +
				 MinMessageInterval - Now) div 1000,
			    Interval = lists:max([MessageInterval,
						  MessageShaperInterval]),
			    erlang:send_after(
			      Interval, self(), {process_user_message, From}),
			    NewActivity = Activity#activity{
					    message = Packet,
					    message_shaper = MessageShaper},
			    StateData1 =
				store_user_activity(
				  From, NewActivity, StateData),
			    {next_state, normal_state, StateData1}
		    end;
		error ->
		    case is_user_online(From, StateData) of
			true ->
			    ErrorText = "This participant is kicked from the room because "
				"he sent an error message",
			    NewState = expulse_participant(Packet, From, StateData, 
					 translate:translate(Lang, ErrorText)),
			    {next_state, normal_state, NewState};
			_ ->
			    {next_state, normal_state, StateData}
		    end;
		chat ->
		    ErrText = "It is not allowed to send private messages to the conference",
		    Err = exmpp_stanza:error(Packet#xmlel.ns,
		      'not-acceptable',
		      {Lang, translate:translate(Lang, ErrText)}),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, exmpp_stanza:reply_with_error(Packet, Err)),
		    {next_state, normal_state, StateData};
        %%TODO: currently exmpp_message:get_type/1 never returns 'undefined'
		Type when (Type == 'normal') orelse (Type == 'undefined') ->
		    case catch check_invitation(From, 
			  exmpp_xml:get_child_elements(Packet), 
			  Lang, 
			  StateData) of
			{error, Error} ->
			    Err = exmpp_stanza:reply_with_error(Packet, Error),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, Err),
			    {next_state, normal_state, StateData};
			IJID ->
			    Config = StateData#state.config,
			    case Config#config.members_only of
				true ->
				    case get_affiliation(IJID, StateData) of
					none ->
					    NSD = set_affiliation(
						    IJID,
						    member,
						    StateData),
					    case (NSD#state.config)#config.persistent of
						true ->
						    mod_muc:store_room(
						      NSD#state.host,
						      NSD#state.room,
						      make_opts(NSD));
						_ ->
						    ok
					    end,
					    {next_state, normal_state, NSD};
					_ ->
					    {next_state, normal_state,
					     StateData}
				    end;
				false ->
				    {next_state, normal_state, StateData}
			    end
		    end;
		_ ->
		    ErrText = "Improper message type",
		    Err = exmpp_stanza:error(Packet#xmlel.ns,
		      'not-acceptable',
		      {Lang, translate:translate(Lang, ErrText)}),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, exmpp_stanza:reply_with_error(Packet, Err)),
		    {next_state, normal_state, StateData}
	    end;
	_ ->
	    case exmpp_stanza:is_stanza_error(Packet) of
		true ->
		    ok;
		false ->
		    handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, undefined,
	      #xmlel{name = 'iq'} = Packet},
	     StateData) ->
    case exmpp_iq:xmlel_to_iq(Packet) of
	#iq{type = Type, ns = XMLNS, lang = Lang, payload = SubEl} = IQ when
	      (XMLNS == ?NS_MUC_ADMIN) or
	      (XMLNS == ?NS_MUC_OWNER) or
	      (XMLNS == ?NS_DISCO_INFO) or
	      (XMLNS == ?NS_DISCO_ITEMS) ->
	    Res1 = case XMLNS of
		       ?NS_MUC_ADMIN ->
			   process_iq_admin(From, Type, Lang, SubEl, StateData);
		       ?NS_MUC_OWNER ->
			   process_iq_owner(From, Type, Lang, SubEl, StateData);
		       ?NS_DISCO_INFO ->
			   process_iq_disco_info(From, Type, Lang, StateData);
		       ?NS_DISCO_ITEMS ->
			   process_iq_disco_items(From, Type, Lang, StateData)
		   end,
	    {IQRes, NewStateData} =
		case Res1 of
		    {result, [], SD} ->
			{exmpp_iq:result(IQ), SD};
		    {result, Res, SD} ->
			{exmpp_iq:result(IQ,#xmlel{ns = XMLNS, 
			      name = 'query', 
			      children = Res}), SD};
		    {error, Error} ->
			{exmpp_iq:error(IQ, Error), StateData}
		end,
	    ejabberd_router:route(StateData#state.jid,
				  From,
				  exmpp_iq:iq_to_xmlel(IQRes)),
	    case NewStateData of
		stop ->
		    {stop, normal, StateData};
		_ ->
		    {next_state, normal_state, NewStateData}
	    end;
	reply ->
	    {next_state, normal_state, StateData};
	_ ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'feature-not-implemented'),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, Nick,
	      #xmlel{name = 'presence'} = Packet},
	     StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = now_to_usec(now()),
    MinPresenceInterval =
	trunc(gen_mod:get_module_opt(
		StateData#state.server_host,
		mod_muc, min_presence_interval, 0) * 1000000),
    if
	(Now >= Activity#activity.presence_time + MinPresenceInterval) and
	(Activity#activity.presence == undefined) ->
	    NewActivity = Activity#activity{presence_time = Now},
	    StateData1 = store_user_activity(From, NewActivity, StateData),
	    process_presence(From, Nick, Packet, StateData1);
	true ->
	    if
		Activity#activity.presence == undefined ->
		    Interval = (Activity#activity.presence_time +
				MinPresenceInterval - Now) div 1000,
		    erlang:send_after(
		      Interval, self(), {process_user_presence, From});
		true ->
		    ok
	    end,
	    NewActivity = Activity#activity{presence = {Nick, Packet}},
	    StateData1 = store_user_activity(From, NewActivity, StateData),
	    {next_state, normal_state, StateData1}
    end;

normal_state({route, From, ToNick,
	      #xmlel{name = 'message'} = Packet},
	     StateData) ->
    Type = exmpp_message:get_type(Packet),
    Lang = exmpp_stanza:get_lang(Packet),
    case decide_fate_message(Type, Packet, From, StateData) of
	{expulse_sender, Reason} ->
	    ?DEBUG(Reason, []),
	    ErrorText = "This participant is kicked from the room because "
		"he sent an error message to another participant",
	    NewState = expulse_participant(Packet, From, StateData, 
					   translate:translate(Lang, ErrorText)),
	    {next_state, normal_state, NewState};
	forget_message ->
	    {next_state, normal_state, StateData};
	continue_delivery ->
	    case {(StateData#state.config)#config.allow_private_messages,
		is_user_online(From, StateData)} of
		{true, true} ->
		    case Type of
			"groupchat" ->
			    ErrText = "It is not allowed to send private "
				"messages of type \"groupchat\"",
			    Err = exmpp_stanza:reply_with_error(Packet,
			      exmpp_stanza:error(Packet#xmlel.ns, 'bad-request',
				{Lang, translate:translate(Lang, ErrText)})),
			    ejabberd_router:route(
			      jid_replace_resource(
				StateData#state.jid,
				ToNick),
			      From, Err);
			_ ->
			    case find_jid_by_nick(ToNick, StateData) of
				false ->
				    ErrText = "Recipient is not in the conference room",
				    Err = exmpp_stanza:reply_with_error(Packet,
				      exmpp_stanza:error(Packet#xmlel.ns, 'item-not-found',
					{Lang, translate:translate(Lang, ErrText)})),
				    ejabberd_router:route(
				      jid_replace_resource(
					StateData#state.jid,
					ToNick),
				      From, Err);
				ToJID ->
				    {ok, #user{nick = FromNick}} =
					?DICT:find(jlib:short_prepd_jid(From),
						   StateData#state.users),
				    ejabberd_router:route(
				      jid_replace_resource(
					StateData#state.jid,
					FromNick),
				      ToJID, Packet)
			    end
		    end;
		{true, false} ->
		    ErrText = "Only occupants are allowed to send messages to the conference",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(
			StateData#state.jid,
			ToNick),
		      From, Err);
		{false, _} ->
		    ErrText = "It is not allowed to send private messages",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(
			StateData#state.jid,
			ToNick),
		      From, Err)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, ToNick,
	      #xmlel{name = 'iq'} = Packet},
	     StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
    case {(StateData#state.config)#config.allow_query_users,
	  is_user_online(From, StateData)} of
	{true, true} ->
	    case find_jid_by_nick(ToNick, StateData) of
		false ->
		    case exmpp_iq:get_type(Packet) of
			result ->
			    ok;
			error ->
			    ok;
			_ ->
			    ErrText = "Recipient is not in the conference room",
			    Err = exmpp_stanza:reply_with_error(Packet,
			      exmpp_stanza:error(Packet#xmlel.ns, 'item-not-found',
				{Lang, translate:translate(Lang, ErrText)})),
			    ejabberd_router:route(
			      jid_replace_resource(
				StateData#state.jid, ToNick),
			      From, Err)
		    end;
		ToJID ->
		    {ok, #user{nick = FromNick}} =
			?DICT:find(jlib:short_prepd_jid(From),
				   StateData#state.users),
		    ejabberd_router:route(
		      jid_replace_resource(StateData#state.jid, FromNick),
		      ToJID, Packet)
	    end;
	{_, false} ->
	    case exmpp_iq:get_type(Packet) of
		result ->
		    ok;
		error ->
		    ok;
		_ ->
		    ErrText = "Only occupants are allowed to send queries to the conference",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(StateData#state.jid, ToNick),
		      From, Err)
	    end;
	_ ->
	    case exmpp_iq:get_type(Packet) of
		result ->
		    ok;
		error ->
		    ok;
		_ ->
		    ErrText = "Queries to the conference members are not allowed in this room",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'not-allowed',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      jid_replace_resource(StateData#state.jid, ToNick),
		      From, Err)
	    end
    end,
    {next_state, normal_state, StateData};

normal_state(_Event, StateData) ->
    {next_state, normal_state, StateData}.



%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event({service_message, Msg}, _StateName, StateData) ->
    MessagePkt = #xmlel{name = 'message',
      attrs = [#xmlattr{name = 'type', value = "groupchat"}],
      children = [#xmlel{name = 'body',
	  children = [#xmlcdata{cdata = Msg}]}]},
    lists:foreach(
      fun({_LJID, Info}) ->
	      ejabberd_router:route(
		StateData#state.jid,
		Info#user.jid,
		MessagePkt)
      end,
      ?DICT:to_list(StateData#state.users)),
    NSD = add_message_to_history("",
				 MessagePkt,
				 StateData),
    {next_state, normal_state, NSD};

handle_event({destroy, Reason}, _StateName, StateData) ->
    {result, [], stop} =
    destroy_room(
      #xmlel{ns = ?NS_MUC_OWNER, name = 'destroy',
	children = case Reason of
	    none -> [];
	    _Else -> [#xmlel{name = 'reason',
		    children = [#xmlcdata{cdata = Reason}]}]
	end}, StateData),

    ?INFO_MSG("Destroyed MUC room ~s with reason: ~p", 
	      [exmpp_jid:jid_to_list(StateData#state.jid), Reason]),
    {stop, normal, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s", 
	      [exmpp_jid:jid_to_list(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);

handle_event({set_affiliations, Affiliations}, StateName, StateData) ->
    {next_state, StateName, StateData#state{affiliations = Affiliations}};

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event({get_disco_item, JID, Lang}, _From, StateName, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FRole = get_role(JID, StateData),
    Tail =
	case ((StateData#state.config)#config.public_list == true) orelse
	    (FRole /= none) orelse
	    (FAffiliation == admin) orelse
	    (FAffiliation == owner) of
	    true ->
		Desc = case (StateData#state.config)#config.public of
			   true ->
			       "";
			   _ ->
			       translate:translate(Lang, "private, ")
		       end,
		Len = ?DICT:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
				 StateData#state.users),
		" (" ++ Desc ++ integer_to_list(Len) ++ ")";
	    _ ->
		" (n/a)"
	end,
	Reply = case ((StateData#state.config)#config.public == true) orelse
		(FRole /= none) orelse
		(FAffiliation == admin) orelse
		(FAffiliation == owner) of
		true ->
		    {item, get_title(StateData) ++ Tail};
		_ ->
		    false
	    end,
    {reply, Reply, StateName, StateData};
handle_sync_event(get_config, _From, StateName, StateData) ->
    {reply, {ok, StateData#state.config}, StateName, StateData};
handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event({change_config, Config}, _From, StateName, StateData) ->
    {result, [], NSD} = change_config(Config, StateData),
    {reply, {ok, NSD#state.config}, StateName, NSD};
handle_sync_event({change_state, NewStateData}, _From, StateName, _StateData) ->
    {reply, {ok, NewStateData}, StateName, NewStateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({presence, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
	RoomQueueEmpty ->
	    StateData2 = prepare_room_queue(StateData1),
	    {next_state, normal_state, StateData2};
	true ->
	    {next_state, normal_state, StateData1}
    end;
handle_info({process_user_message, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({message, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
	RoomQueueEmpty ->
	    StateData2 = prepare_room_queue(StateData1),
	    {next_state, normal_state, StateData2};
	true ->
	    {next_state, normal_state, StateData1}
    end;
handle_info(process_room_queue, normal_state = StateName, StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    NewActivity = Activity#activity{message = undefined},
	    StateData1 =
		store_user_activity(
		  From, NewActivity, StateData),
	    StateData2 =
		StateData1#state{
		  room_queue = RoomQueue},
	    StateData3 = prepare_room_queue(StateData2),
	    process_groupchat_message(From, Packet, StateData3);
	{{value, {presence, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {Nick, Packet} = Activity#activity.presence,
	    NewActivity = Activity#activity{presence = undefined},
	    StateData1 =
		store_user_activity(
		  From, NewActivity, StateData),
	    StateData2 =
		StateData1#state{
		  room_queue = RoomQueue},
	    StateData3 = prepare_room_queue(StateData2),
	    process_presence(From, Nick, Packet, StateData3);
	{empty, _} ->
	    {next_state, StateName, StateData}
    end;
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    ?DICT:fold(
       fun(J, _, _) ->
	       tab_remove_online_user(J, StateData)
       end, [], StateData#state.users),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
			   StateData#state.server_host),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

route(Pid, From, ToNick, Packet) ->
    gen_fsm:send_event(Pid, {route, From, ToNick, Packet}).

process_groupchat_message(From, #xmlel{name = 'message'} = Packet,
			  StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    {FromNick, Role} = get_participant_data(From, StateData),
	    if
		(Role == moderator) or (Role == participant) 
		or ((StateData#state.config)#config.moderated == false) ->
		    {NewStateData1, IsAllowed} =
			case check_subject(Packet) of
			    false ->
				{StateData, true};
			    Subject ->
				case can_change_subject(Role,
							StateData) of
				    true ->
					NSD =
					    StateData#state{
					      subject = Subject,
					      subject_author =
					      FromNick},
					case (NSD#state.config)#config.persistent of
					    true ->
						mod_muc:store_room(
						  NSD#state.host,
						  NSD#state.room,
						  make_opts(NSD));
					    _ ->
						ok
					end,
					{NSD, true};
				    _ ->
					{StateData, false}
				end
			end,
		    case IsAllowed of
			true ->
			    lists:foreach(
			      fun({_LJID, Info}) ->
				      ejabberd_router:route(
					jid_replace_resource(
					  StateData#state.jid,
					  FromNick),
					Info#user.jid,
					Packet)
			      end,
			      ?DICT:to_list(StateData#state.users)),
			    NewStateData2 =
				add_message_to_history(FromNick,
						       Packet,
						       NewStateData1),
			    {next_state, normal_state, NewStateData2};
			_ ->
			    Err =
				case (StateData#state.config)#config.allow_change_subj of
				    true ->
					exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
					    {Lang, translate:translate(Lang,
						"Only moderators and participants "
						"are allowed to change subject in this room")}));
				    _ ->
					exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
					    {Lang, translate:translate(Lang,
						"Only moderators "
						"are allowed to change subject in this room")}))
				end,
			    ejabberd_router:route(
			      StateData#state.jid,
			      From,
			      Err),
			    {next_state, normal_state, StateData}
		    end;
		true ->
		    ErrText = "Visitors are not allowed to send messages to all occupants",
		    Err = exmpp_stanza:reply_with_error(Packet,
		      exmpp_stanza:error(Packet#xmlel.ns, 'forbidden',
			{Lang, translate:translate(Lang, ErrText)})),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData}
	    end;
	false ->
	    ErrText = "Only occupants are allowed to send messages to the conference",
	    Err = exmpp_stanza:reply_with_error(Packet,
	      exmpp_stanza:error(Packet#xmlel.ns, 'not-acceptable',
		{Lang, translate:translate(Lang, ErrText)})),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end.


%% @doc Check if this non participant can send message to room.
%%
%% XEP-0045 v1.23:
%% 7.9 Sending a Message to All Occupants
%% an implementation MAY allow users with certain privileges
%% (e.g., a room owner, room admin, or service-level admin)
%% to send messages to the room even if those users are not occupants.
%%
%% Check the mod_muc option access_message_nonparticipant and wether this JID
%% is allowed or denied
is_user_allowed_message_nonparticipant(JID, StateData) ->
    case get_service_affiliation(JID, StateData) of
	owner ->
	    true;
	_ -> false
    end.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
get_participant_data(From, StateData) ->
    case ?DICT:find(jlib:short_prepd_jid(From), StateData#state.users) of
	{ok, #user{nick = FromNick, role = Role}} ->
	    {FromNick, Role};
	error ->
	    {"", moderator}
    end.


process_presence(From, Nick, #xmlel{name = 'presence'} = Packet,
		 StateData) ->
    Type = exmpp_presence:get_type(Packet),
    Lang = exmpp_stanza:get_lang(Packet),
    StateData1 =
	case Type of
	    unavailable ->
		case is_user_online(From, StateData) of
		    true ->
			NewState =
			    add_user_presence_un(From, Packet, StateData),
			send_new_presence(From, NewState),
			Reason = case exmpp_xml:get_element(Packet, 'status') of
				undefined -> <<>>;
				Status_el -> exmpp_xml:get_cdata(Status_el)
			end,
			remove_online_user(From, NewState, Reason);
		    _ ->
			StateData
		end;
	    error ->
		case is_user_online(From, StateData) of
		    true ->
			ErrorText = "This participant is kicked from the room because "
			    "he sent an error presence",
			expulse_participant(Packet, From, StateData,
					    translate:translate(Lang, ErrorText));
		    _ ->
			StateData
		end;
	    'available' ->
		case is_user_online(From, StateData) of
		    true ->
			case is_nick_change(From, Nick, StateData) of
			    true ->
				case {is_nick_exists(Nick, StateData),
				      mod_muc:can_use_nick(
					StateData#state.host, From, Nick),
				      {(StateData#state.config)#config.allow_visitor_nickchange,
					is_visitor(From, StateData)}} of
				    {_, _, {false, true}} ->
					ErrText = "Visitors are not allowed to change their nicknames in this room",
					Err = exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'not-allowed',
					    {Lang, translate:translate(Lang, ErrText)})),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					StateData;
				    {true, _, _} ->
					Lang = exmpp_stanza:get_lang(Packet),
					ErrText = "Nickname is already in use by another occupant",
					Err = exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'conflict',
					    {Lang, translate:translate(Lang, ErrText)})),
					ejabberd_router:route(
					  jid_replace_resource(
					    StateData#state.jid,
					    Nick), % TODO: s/Nick/""/
					  From, Err),
					StateData;
				    {_, false, _} ->
					ErrText = "Nickname is registered by another person",
					Err = exmpp_stanza:reply_with_error(Packet,
					  exmpp_stanza:error(Packet#xmlel.ns, 'conflict',
					    {Lang, translate:translate(Lang, ErrText)})),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					StateData;
				    _ ->
					change_nick(From, Nick, StateData)
				end;
			    _NotNickChange ->
                                Stanza = case {(StateData#state.config)#config.allow_visitor_status,
                                               is_visitor(From, StateData)} of
                                             {false, true} ->
                                                 strip_status(Packet);
                                             _Allowed ->
                                                 Packet
                                         end,
                                NewState = add_user_presence(From, Stanza, StateData),
                                send_new_presence(From, NewState),
                                NewState
			end;
		    _ ->
			add_new_user(From, Nick, Packet, StateData)
		end;
	    _ ->
		StateData
	end,
    case (not (StateData1#state.config)#config.persistent) andalso
	(?DICT:to_list(StateData1#state.users) == []) of
	true ->
	    ?INFO_MSG("Destroyed MUC room ~s because it's temporary and empty", 
		      [exmpp_jid:jid_to_list(StateData#state.jid)]),
	    {stop, normal, StateData1};
	_ ->
	    {next_state, normal_state, StateData1}
    end.

is_user_online(JID, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    ?DICT:is_key(LJID, StateData#state.users).

role_to_list(Role) ->
    case Role of
	moderator ->   "moderator";
	participant -> "participant";
	visitor ->     "visitor";
	none ->        "none"
    end.

affiliation_to_list(Affiliation) ->
    case Affiliation of
	owner ->   "owner";
	admin ->   "admin";
	member ->  "member";
	outcast -> "outcast";
	none ->    "none"
    end.

list_to_role(Role) ->
    case Role of
	"moderator" ->   moderator;
	"participant" -> participant;
	"visitor" ->     visitor;
	"none" ->        none
    end.

list_to_affiliation(Affiliation) ->
    case Affiliation of
	"owner" ->   owner;
	"admin" ->   admin;
	"member" ->  member;
	"outcast" -> outcast;
	"none" ->    none
    end.

%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
decide_fate_message(error, Packet, From, StateData) ->
    %% Make a preliminary decision
    PD = case check_error_kick(Packet) of
	     %% If this is an error stanza and its condition matches a criteria
	     true ->
		 Reason = io_lib:format("This participant is considered a ghost and is expulsed: ~s",
					[exmpp_jid:jid_to_list(From)]),
		 {expulse_sender, Reason};
	     false ->
		 continue_delivery
	 end,
    case PD of
	{expulse_sender, R} ->
	    case is_user_online(From, StateData) of
		true ->
		    {expulse_sender, R};
		false ->
		    forget_message
	    end;
	Other ->
	    Other
    end;

decide_fate_message(_, _, _, _) ->
    continue_delivery.

%% Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
check_error_kick(Packet) ->
    case get_error_condition(Packet) of
	'gone' -> true;
	'internal-server-error' -> true;
	'item-not-found' -> true;
	'jid-malformed' -> true;
	'recipient-unavailable' -> true;
	'redirect' -> true;
	'remote-server-not-found' -> true;
	'remote-server-timeout' -> true;
	'service-unavailable' -> true;
	_ -> false
    end.

get_error_condition(Packet) ->
	try exmpp_stanza:get_condition(Packet) of
	      ErrorCondition -> ErrorCondition
    catch
	     _:_ ->
		'badformed-error-stanza'
	end.

expulse_participant(Packet, From, StateData, Reason1) ->
	ErrorCondition = get_error_condition(Packet),
	Reason2 = io_lib:format(Reason1 ++ ": " ++ "~s", [ErrorCondition]),
	NewState = add_user_presence_un(
		From,
        exmpp_presence:presence('unavailable',Reason2),
	StateData),
	send_new_presence(From, NewState),
	remove_online_user(From, NewState).


set_affiliation(JID, Affiliation, StateData) ->
    LJID = jlib:short_prepd_bare_jid(JID),
    Affiliations = case Affiliation of
		       none ->
			   ?DICT:erase(LJID,
				       StateData#state.affiliations);
		       _ ->
			   ?DICT:store(LJID,
				       Affiliation,
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

set_affiliation_and_reason(JID, Affiliation, Reason, StateData) ->
    LJID = jlib:short_prepd_bare_jid(JID),
    Affiliations = case Affiliation of
		       none ->
			   ?DICT:erase(LJID,
				       StateData#state.affiliations);
		       _ ->
			   ?DICT:store(LJID,
				       {Affiliation, Reason},
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

get_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = StateData#state.access,
    Res =
	case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	    allow ->
		owner;
	    _ ->
		LJID = jlib:short_prepd_jid(JID),
		case ?DICT:find(LJID, StateData#state.affiliations) of
		    {ok, Affiliation} ->
			Affiliation;
		    _ ->
            LJID1 = jlib:short_prepd_bare_jid(JID),
			case ?DICT:find(LJID1, StateData#state.affiliations) of
			    {ok, Affiliation} ->
				Affiliation;
			    _ ->
				LJID2 = setelement(1, LJID, undefined),
				case ?DICT:find(LJID2, StateData#state.affiliations) of
				    {ok, Affiliation} ->
					Affiliation;
				    _ ->
                    LJID3 = setelement(1,jlib:short_prepd_bare_jid(JID),undefined),
					case ?DICT:find(LJID3, StateData#state.affiliations) of
					    {ok, Affiliation} ->
						Affiliation;
					    _ ->
						none
					end
				end
			end
		end
	end,
    case Res of
	{A, _Reason} ->
	    A;
	_ ->
	    Res
    end.

get_service_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} =
	StateData#state.access,
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	allow ->
	    owner;
	_ ->
	    none
    end.

set_role(JID, Role, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    LJIDs = case LJID of
		{U, S, undefined} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    Users = case Role of
		none ->
		    lists:foldl(fun(J, Us) ->
					?DICT:erase(J,
						    Us)
				end, StateData#state.users, LJIDs);
		_ ->
		    lists:foldl(fun(J, Us) ->
					{ok, User} = ?DICT:find(J, Us),
					?DICT:store(J,
						    User#user{role = Role},
						    Us)
				end, StateData#state.users, LJIDs)
	    end,
    StateData#state{users = Users}.

get_role(JID, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    case ?DICT:find(LJID, StateData#state.users) of
	{ok, #user{role = Role}} ->
	    Role;
	_ ->
	    none
    end.

get_default_role(Affiliation, StateData) ->
    case Affiliation of
	owner ->   moderator;
	admin ->   moderator;
	member ->  participant;
	outcast -> none;
	none ->
	    case (StateData#state.config)#config.members_only of
		true ->
		    none;
		_ ->
		    case (StateData#state.config)#config.members_by_default of
			true ->
			    participant;
			_ ->
			    visitor
		    end
	    end
    end.

is_visitor(Jid, StateData) ->
    get_role(Jid, StateData) =:= visitor.

get_max_users(StateData) ->
    MaxUsers = (StateData#state.config)#config.max_users,
    ServiceMaxUsers = get_service_max_users(StateData),
    if
	MaxUsers =< ServiceMaxUsers -> MaxUsers;
	true -> ServiceMaxUsers
    end.

get_service_max_users(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users, ?MAX_USERS_DEFAULT).

get_max_users_admin_threshold(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users_admin_threshold, 5).

get_user_activity(JID, StateData) ->
    case treap:lookup(jlib:short_prepd_jid(JID),
		      StateData#state.activity) of
	{ok, _P, A} -> A;
	error ->
	    MessageShaper =
		shaper:new(gen_mod:get_module_opt(
			     StateData#state.server_host,
			     mod_muc, user_message_shaper, none)),
	    PresenceShaper =
		shaper:new(gen_mod:get_module_opt(
			     StateData#state.server_host,
			     mod_muc, user_presence_shaper, none)),
	    #activity{message_shaper = MessageShaper,
		      presence_shaper = PresenceShaper}
    end.

store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
	gen_mod:get_module_opt(
	  StateData#state.server_host,
	  mod_muc, min_message_interval, 0),
    MinPresenceInterval =
	gen_mod:get_module_opt(
	  StateData#state.server_host,
	  mod_muc, min_presence_interval, 0),
    Key = jlib:short_prepd_jid(JID),
    Now = now_to_usec(now()),
    Activity1 = clean_treap(StateData#state.activity, {1, -Now}),
    Activity =
	case treap:lookup(Key, Activity1) of
	    {ok, _P, _A} ->
		treap:delete(Key, Activity1);
	    error ->
		Activity1
	end,
    StateData1 =
	case (MinMessageInterval == 0) andalso
	    (MinPresenceInterval == 0) andalso
	    (UserActivity#activity.message_shaper == none) andalso
	    (UserActivity#activity.presence_shaper == none) andalso
	    (UserActivity#activity.message == undefined) andalso
	    (UserActivity#activity.presence == undefined) of
	    true ->
		StateData#state{activity = Activity};
	    false ->
		case (UserActivity#activity.message == undefined) andalso
		    (UserActivity#activity.presence == undefined) of
		    true ->
			{_, MessageShaperInterval} =
			    shaper:update(UserActivity#activity.message_shaper,
					  100000),
			{_, PresenceShaperInterval} =
			    shaper:update(UserActivity#activity.presence_shaper,
					  100000),
			Delay = lists:max([MessageShaperInterval,
					   PresenceShaperInterval,
					   MinMessageInterval * 1000,
					   MinPresenceInterval * 1000]) * 1000,
			Priority = {1, -(Now + Delay)},
			StateData#state{
			  activity = treap:insert(
				       Key,
				       Priority,
				       UserActivity,
				       Activity)};
		    false ->
			Priority = {0, 0},
			StateData#state{
			  activity = treap:insert(
				       Key,
				       Priority,
				       UserActivity,
				       Activity)}
		end
	end,
    StateData1.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
	true ->
	    Treap;
	false ->
	    {_Key, Priority, _Value} = treap:get_root(Treap),
	    if
		Priority > CleanPriority ->
		    clean_treap(treap:delete_root(Treap), CleanPriority);
		true ->
		    Treap
	    end
    end.


prepare_room_queue(StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    Size = lists:flatlength(exmpp_xml:documenent_to_list(Packet)),
	    {RoomShaper, RoomShaperInterval} =
		shaper:update(StateData#state.room_shaper, Size),
	    erlang:send_after(
	      RoomShaperInterval, self(),
	      process_room_queue),
	    StateData#state{
	      room_shaper = RoomShaper};
	{{value, {presence, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {_Nick, Packet} = Activity#activity.presence,
	    Size = lists:flatlength(exmpp_xml:document_to_list(Packet)),
	    {RoomShaper, RoomShaperInterval} =
		shaper:update(StateData#state.room_shaper, Size),
	    erlang:send_after(
	      RoomShaperInterval, self(),
	      process_room_queue),
	    StateData#state{
	      room_shaper = RoomShaper};
	{empty, _} ->
	    StateData
    end.


add_online_user(JID, Nick, Role, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    Users = ?DICT:store(LJID,
			#user{jid = JID,
			      nick = Nick,
			      role = Role},
			StateData#state.users),
    add_to_log(join, Nick, StateData),
    tab_add_online_user(JID, StateData),
    StateData#state{users = Users}.

remove_online_user(JID, StateData) ->
	remove_online_user(JID, StateData, <<>>).

remove_online_user(JID, StateData, Reason) ->
    LJID = jlib:short_prepd_jid(JID),
    {ok, #user{nick = Nick}} =
    	?DICT:find(LJID, StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = ?DICT:erase(LJID, StateData#state.users),
    StateData#state{users = Users}.


filter_presence(#xmlel{name = 'presence'} = Packet) ->
    FEls = lists:filter(
	     fun(El) ->
            case El of
			 #xmlel{ns = XMLNS} ->
			     case atom_to_list(XMLNS) of
				 ?NS_MUC_s ++ _ ->
				     false;
				 _ ->
				     true
			     end
		     end
	     end, exmpp_xml:get_child_elements(Packet)),
    exmpp_xml:set_children(Packet, FEls).

strip_status(#xmlel{name = 'presence', children = Children} = Packet) ->
    FEls = lists:filter(
	     fun(#xmlel{name = 'status'}) ->
                     false;
                (_) -> true
	     end, Children),
    exmpp_xml:set_children(Packet,FEls).

add_user_presence(JID, Presence, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence}
	   end, StateData#state.users),
    StateData#state{users = Users}.

add_user_presence_un(JID, Presence, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence,
			     role = none}
	   end, StateData#state.users),
    StateData#state{users = Users}.


is_nick_exists(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{nick = N}, B) ->
		       B orelse (N == Nick)
	       end, false, StateData#state.users).

find_jid_by_nick(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{jid = JID, nick = N}, R) ->
		       case Nick of
			   N -> JID;
			   _ -> R
		       end
	       end, false, StateData#state.users).

is_nick_change(JID, Nick, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    case Nick of
	"" ->
	    false;
	_ ->
	    {ok, #user{nick = OldNick}} =
		?DICT:find(LJID, StateData#state.users),
	    Nick /= OldNick
    end.

add_new_user(From, Nick, Packet, StateData) ->
    Lang = exmpp_stanza:get_lang(Packet),
    MaxUsers = get_max_users(StateData),
    MaxAdminUsers = MaxUsers + get_max_users_admin_threshold(StateData),
    NUsers = dict:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
		       StateData#state.users),
    Affiliation = get_affiliation(From, StateData),
    ServiceAffiliation = get_service_affiliation(From, StateData),
    NConferences = tab_count_user(From),
    MaxConferences = gen_mod:get_module_opt(
		       StateData#state.server_host,
		       mod_muc, max_user_conferences, 10),
    case {(ServiceAffiliation == owner orelse
	   MaxUsers == none orelse
	   ((Affiliation == admin orelse Affiliation == owner) andalso
	    NUsers < MaxAdminUsers) orelse
	   NUsers < MaxUsers) andalso
	  NConferences < MaxConferences,
	  is_nick_exists(Nick, StateData),
	  mod_muc:can_use_nick(StateData#state.host, From, Nick),
	  get_default_role(Affiliation, StateData)} of
	{false, _, _, _} ->
	    % max user reached and user is not admin or owner
	    Err = exmpp_stanza:reply_with_error(
		    Packet,
		    'service-unavailable'),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, none} ->
	    Err = exmpp_stanza:reply_with_error(
		    Packet,
		    case Affiliation of
			outcast ->
			    ErrText = "You have been banned from this room",
			    exmpp_stanza:error(Packet#xmlel.ns,
			      'forbidden',
			      {Lang, translate:translate(Lang, ErrText)});
			_ ->
			    ErrText = "Membership required to enter this room",
			    exmpp_stanza:error(Packet#xmlel.ns,
			      'registration-required',
			      {Lang, translate:translate(Lang, ErrText)})
		    end),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, true, _, _} ->
	    ErrText = "Nickname is already in use by another occupant",
	    Err = exmpp_stanza:reply_with_error(Packet, 
	      exmpp_stanza:error(Packet#xmlel.ns,
		'conflict',
		{Lang, translate:translate(Lang, ErrText)})),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, false, _} ->
	    ErrText = "Nickname is registered by another person",
	    Err = exmpp_stanza:reply_with_error(Packet, 
	      ?ERR(Packet, 'conflict', Lang, ErrText)),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, Role} ->
	    case check_password(Affiliation, 
		  exmpp_xml:get_child_elements(Packet), 
		  StateData) of
		true ->
		    NewState =
			add_user_presence(
			  From, Packet,
			  add_online_user(From, Nick, Role, StateData)),
		    if not (NewState#state.config)#config.anonymous ->
			    WPacket = 
			    #xmlel{name = 'message',
			      attrs = [#xmlattr{name = 'type', value = "groupchat"}],
			      children = [
				#xmlel{name = 'body',
				  children = [#xmlcdata{cdata =
				      translate:translate(Lang,
					"This room is not anonymous")}]}]},
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, WPacket);
			true ->
			    ok
		    end,
		    send_existing_presences(From, NewState),
		    send_new_presence(From, NewState),
		    Shift = count_stanza_shift(Nick, 
		      exmpp_xml:get_child_elements(Packet), 
		      NewState),
		    case send_history(From, Shift, NewState) of
			true ->
			    ok;
			_ ->
			    send_subject(From, Lang, StateData)
		    end,
		    case NewState#state.just_created of
			true ->
			    NewState#state{just_created = false};
			false ->
			    NewState
		    end;
		nopass ->
		    ErrText = "Password required to enter this room",
		    Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-authorized', Lang, ErrText)),
		    ejabberd_router:route( % TODO: s/Nick/""/
		      jid_replace_resource(
			StateData#state.jid, Nick),
		      From, Err),
		    StateData;
		_ ->
		    ErrText = "Incorrect password",
		    Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-authorized', Lang, ErrText)),
		    ejabberd_router:route( % TODO: s/Nick/""/
		      jid_replace_resource(
			StateData#state.jid, Nick),
		      From, Err),
		    StateData
	   end
    end.

check_password(owner, _Els, _StateData) ->
    true;
check_password(_Affiliation, Els, StateData) ->
    case (StateData#state.config)#config.password_protected of
	false ->
	    true;
	true ->
	    Pass = extract_password(Els),
	    case Pass of
		false ->
		    nopass;
		_ ->
		    case (StateData#state.config)#config.password of
			Pass ->
			    true;
			_ ->
			false
		    end
	    end
    end.

extract_password([]) ->
    false;
extract_password([#xmlel{ns = XMLNS} = El | Els]) ->
    case XMLNS of
	?NS_MUC ->
	    case exmpp_xml:get_element(El, 'password') of
		undefined ->
		    false;
		SubEl ->
		    exmpp_xml:get_cdata(SubEl)
	    end;
	_ ->
	    extract_password(Els)
    end;
extract_password([_ | Els]) ->
    extract_password(Els).

count_stanza_shift(Nick, Els, StateData) ->
    HL = lqueue_to_list(StateData#state.history),
    Since = extract_history(Els, "since"),
    Shift0 = case Since of
		 false ->
		     0;
		 _ ->
		     Sin = calendar:datetime_to_gregorian_seconds(Since),
		     count_seconds_shift(Sin, HL)
	     end,
    Seconds = extract_history(Els, "seconds"),
    Shift1 = case Seconds of
		 false ->
		     0;
		 _ ->
		     Sec = calendar:datetime_to_gregorian_seconds(
			     calendar:now_to_universal_time(now())) - Seconds,
		     count_seconds_shift(Sec, HL)
	     end,
    MaxStanzas = extract_history(Els, "maxstanzas"),
    Shift2 = case MaxStanzas of
		 false ->
		     0;
		 _ ->
		     count_maxstanzas_shift(MaxStanzas, HL)
	     end,
    MaxChars = extract_history(Els, "maxchars"),
    Shift3 = case MaxChars of
		 false ->
		     0;
		 _ ->
		     count_maxchars_shift(Nick, MaxChars, HL)
	     end,
    lists:max([Shift0, Shift1, Shift2, Shift3]).

count_seconds_shift(Seconds, HistoryList) ->
    lists:sum(
      lists:map(
	fun({_Nick, _Packet, _HaveSubject, TimeStamp, _Size}) ->
	    T = calendar:datetime_to_gregorian_seconds(TimeStamp),
	    if
		T < Seconds ->
		    1;
		true ->
		    0
	    end
	end, HistoryList)).

count_maxstanzas_shift(MaxStanzas, HistoryList) ->
    S = length(HistoryList) - MaxStanzas,
    if
	S =< 0 ->
	    0;
	true ->
	    S
    end.

count_maxchars_shift(Nick, MaxSize, HistoryList) ->
    NLen = string:len(Nick) + 1,
    Sizes = lists:map(
	      fun({_Nick, _Packet, _HaveSubject, _TimeStamp, Size}) ->
		  Size + NLen
	      end, HistoryList),
    calc_shift(MaxSize, Sizes).

calc_shift(MaxSize, Sizes) ->
    Total = lists:sum(Sizes),
    calc_shift(MaxSize, Total, 0, Sizes).

calc_shift(_MaxSize, _Size, Shift, []) ->
    Shift;
calc_shift(MaxSize, Size, Shift, [S | TSizes]) ->
    if
	MaxSize >= Size ->
	    Shift;
	true ->
	    calc_shift(MaxSize, Size - S, Shift + 1, TSizes)
    end.

extract_history([], _Type) ->
    false;
extract_history([#xmlel{ns = XMLNS} = El | Els], Type) ->
    case XMLNS of
	?NS_MUC ->
	    AttrVal = exmpp_xml:get_path(El,
		       [{element, 'history'}, {attribute, Type,""}]),
	    case Type of
		"since" ->
		    case jlib:datetime_string_to_timestamp(AttrVal) of
			undefined ->
			    false;
			TS ->
			    calendar:now_to_universal_time(TS)
		    end;
		_ ->
		    case catch list_to_integer(AttrVal) of
			IntVal when is_integer(IntVal) and (IntVal >= 0) ->
			    IntVal;
			_ ->
			    false
		    end
	    end;
	_ ->
	    extract_history(Els, Type)
    end;
extract_history([_ | Els], Type) ->
    extract_history(Els, Type).


send_update_presence(JID, StateData) ->
    send_update_presence(JID, "", StateData).

send_update_presence(JID, Reason, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    LJIDs = case LJID of
		{U, S, undefined} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    lists:foreach(fun(J) ->
			  send_new_presence(J, Reason, StateData)
		  end, LJIDs).

send_new_presence(NJID, StateData) ->
    send_new_presence(NJID, "", StateData).

send_new_presence(NJID, Reason, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(jlib:short_prepd_jid(NJID), StateData#state.users),
    Affiliation = get_affiliation(NJID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    SRole = role_to_list(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_list(RealJID)},
			   #xmlattr{name = 'affiliation', value = SAffiliation},
			   #xmlattr{name = 'role', value = SRole}];
		      _ ->
			  [#xmlattr{name = 'affiliation', value = SAffiliation},
			   #xmlattr{name = 'role', value = SRole}]
		  end,
	      ItemEls = case Reason of
			    "" ->
				[];
			    _ ->
                [#xmlel{name = 'reason',
                       children = [#xmlcdata{cdata = Reason}]}]
			end,
	      Status = case StateData#state.just_created of
			   true ->
			       [#xmlel{name = 'status', 
                           attrs = [#xmlattr{name = 'code', value = "201"}]}];
			   false ->
			       []
		       end,
          Packet = exmpp_xml:append_child(Presence,
             #xmlel{ns = ?NS_MUC_USER, name = 'x',
                   children = [#xmlel{ns = ?NS_MUC_USER, name ='item',
                                      attrs = ItemAttrs,
                                      children = ItemEls} | Status]}),
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).


send_existing_presences(ToJID, StateData) ->
    LToJID = jlib:short_prepd_jid(ToJID),
    {ok, #user{jid = RealToJID,
	       role = Role}} =
	?DICT:find(LToJID, StateData#state.users),
    lists:foreach(
      fun({LJID, #user{jid = FromJID,
		       nick = FromNick,
		       role = FromRole,
		       last_presence = Presence
		      }}) ->
	      case RealToJID of
		  FromJID ->
		      ok;
		  _ ->
              {N,D,R} = LJID,
		      FromAffiliation = get_affiliation(exmpp_jid:make_jid(N,D,R), 
                                                StateData),
		      ItemAttrs =
			  case (Role == moderator) orelse
			      ((StateData#state.config)#config.anonymous ==
			       false) of
			      true ->
				  [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_list(FromJID)},
				   #xmlattr{name = 'affiliation', 
                           value = affiliation_to_list(FromAffiliation)},
				   #xmlattr{name = 'role', value = role_to_list(FromRole)}];
			      _ ->
				  [#xmlattr{name = 'affiliation', 
                           value = affiliation_to_list(FromAffiliation)},
				   #xmlattr{name = 'role', value = role_to_list(FromRole)}]
			  end,
		      Packet = exmpp_xml:append_child(Presence,
				 #xmlel{ns = ?NS_MUC_USER, name = 'x',
                       children = [#xmlel{ns = ?NS_MUC_USER, name ='item',
                                          attrs = ItemAttrs}]}),
		      ejabberd_router:route(
			jid_replace_resource(
			  StateData#state.jid, FromNick),
			RealToJID,
			Packet)
	      end
      end, ?DICT:to_list(StateData#state.users)).



now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.


change_nick(JID, Nick, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    {ok, #user{nick = OldNick}} =
	?DICT:find(LJID, StateData#state.users),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{nick = Nick}
	   end, StateData#state.users),
    NewStateData = StateData#state{users = Users},
    send_nick_changing(JID, OldNick, NewStateData),
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.

send_nick_changing(JID, OldNick, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(jlib:short_prepd_jid(JID), StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    SRole = role_to_list(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs1 =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_list(RealJID)},
			   #xmlattr{name = 'affiliation', value = SAffiliation},
			   #xmlattr{name = 'role', value = SRole},
			   #xmlattr{name = 'nick', value = Nick}];
		      _ ->
			  [#xmlattr{name = 'affiliation', value = SAffiliation},
			   #xmlattr{name = 'role', value = SRole},
			   #xmlattr{name = 'nick', value = Nick}]
		  end,
	      ItemAttrs2 =
		  case (Info#user.role == moderator) orelse
		      ((StateData#state.config)#config.anonymous == false) of
		      true ->
			  [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_list(RealJID)},
			   #xmlattr{name = 'affiliation', value = SAffiliation},
			   #xmlattr{name = 'role', value = SRole}];
		      _ ->
			  [#xmlattr{name = 'affiliation', value = SAffiliation},
			   #xmlattr{name = 'role', value = SRole}]
		  end,
	      Packet1 =
               #xmlel{ns = ?NS_JABBER_CLIENT,
                    name = 'presence', 
                    attrs = [#xmlattr{name = 'type', value = "unavailable"}],
                    children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
                                 children = [
                                  #xmlel{ns = ?NS_MUC_USER, name = 'item',
                                         attrs = ItemAttrs1},
                                  #xmlel{ns = ?NS_MUC_USER, name = 'status',
                                         attrs = [#xmlattr{name = 'code', 
                                                         value = "303"}]}]}]},

	      Packet2 = exmpp_xml:append_child(
			  Presence,
			  #xmlel{ns = ?NS_MUC_USER, name = 'x',
                     children =[#xmlel{ns = ?NS_MUC_USER, 
                                       name = 'item', 
                                       attrs = ItemAttrs2}]}),

	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, OldNick),
		Info#user.jid,
		Packet1),
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet2)
      end, ?DICT:to_list(StateData#state.users)).


lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
	    len = 0,
	    max = Max}.

%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) ->
    LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item, #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    if
	Len >= Max ->
	    Q3 = lqueue_cut(Q2, Len - Max + 1),
	    #lqueue{queue = Q3, len = Max, max = Max};
	true ->
	    #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.

lqueue_cut(Q, 0) ->
    Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q),
    lqueue_cut(Q1, N - 1).

lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).


add_message_to_history(FromNick, Packet, StateData) ->
    HaveSubject = exmpp_xml:has_element(Packet, 'subject'),
    TimeStamp = calendar:now_to_universal_time(now()),
    TSPacket = exmpp_xml:append_child(Packet,
			      jlib:timestamp_to_xml(TimeStamp)),
    SPacket = exmpp_stanza:set_recipient(
                    exmpp_stanza:set_sender(TSPacket,
		                jid_replace_resource(StateData#state.jid, FromNick)),
		            StateData#state.jid),

    Size = lists:flatlength(exmpp_xml:document_to_list(SPacket)),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject, TimeStamp, Size},
		   StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    StateData#state{history = Q1}.

send_history(JID, Shift, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject, _TimeStamp, _Size}, B) ->
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		JID,
		Packet),
	      B or HaveSubject
      end, false, lists:nthtail(Shift, lqueue_to_list(StateData#state.history))).


send_subject(JID, Lang, StateData) ->
    case StateData#state.subject_author of
	"" ->
	    ok;
	Nick ->
	    Subject = StateData#state.subject,
        Packet = exmpp_message:groupchat(Subject,
                    Nick ++ translate:translate(Lang,
                              " has set the subject to: ") ++ Subject),
	    ejabberd_router:route(
	      StateData#state.jid,
	      JID,
	      Packet)
    end.

check_subject(Packet) ->
    case exmpp_message:get_subject(Packet) of
	undefined ->
	    false;
	Subj ->
	    Subj
    end.

can_change_subject(Role, StateData) ->
    case (StateData#state.config)#config.allow_change_subj of
	true ->
	    (Role == moderator) orelse (Role == participant);
	_ ->
	    Role == moderator
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

process_iq_admin(From, set, Lang, SubEl, StateData) ->
    #xmlel{children = Items} = SubEl,
    process_admin_items_set(From, Items, Lang, StateData);

process_iq_admin(From, get, Lang, SubEl, StateData) ->
    case exmpp_xml:get_element(SubEl, 'item') of
	'undefined' ->
	    {error, 'bad-request'};
	Item ->
	    FAffiliation = get_affiliation(From, StateData),
	    FRole = get_role(From, StateData),
	    case exmpp_xml:get_attribute(Item, 'role', false) of
		false ->
		    case exmpp_xml:get_attribute(Item, 'affiliation', false) of
			false ->
			    {error, 'bad-request'};
			StrAffiliation ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    {error, 'bad-request'};
				SAffiliation ->
				    if
					(FAffiliation == owner) or
					(FAffiliation == admin) ->
					    Items = items_with_affiliation(
						      SAffiliation, StateData),
					    {result, Items, StateData};
					true ->
					    ErrText = "Administrator privileges required",
					    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
				    end
			    end
		    end;
		StrRole ->
		    case catch list_to_role(StrRole) of
			{'EXIT', _} ->
			    {error, 'bad-request'};
			SRole ->
			    if
				FRole == moderator ->
				    Items = items_with_role(SRole, StateData),
				    {result, Items, StateData};
				true ->
				    ErrText = "Moderator privileges required",
				    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
			    end
		    end
	    end
    end.


items_with_role(SRole, StateData) ->
    lists:map(
      fun({_, U}) ->
	      user_to_item(U, StateData)
      end, search_role(SRole, StateData)).

items_with_affiliation(SAffiliation, StateData) ->
    lists:map(
      fun({JID, {Affiliation, Reason}}) ->
        {N, D, R} = JID,
        #xmlel{name = 'item', 
               attrs = [#xmlattr{name = 'affiliation', 
                                 value = affiliation_to_list(Affiliation)},
                        #xmlattr{name = 'jid',
                                 value = exmpp_jid:jid_to_list(N, D, R)}],
               children = [ #xmlel{name = 'reason',
                                   children = [#xmlcdata{cdata = Reason}]}]};

	 ({JID, Affiliation}) ->
        {N, D, R} = JID,
        #xmlel{name = 'item', 
               attrs = [#xmlattr{name = 'affiliation', 
                                 value = affiliation_to_list(Affiliation)},
                        #xmlattr{name = 'jid',
                                 value = exmpp_jid:jid_to_list(N, D, R)}]}
      end, search_affiliation(SAffiliation, StateData)).

user_to_item(#user{role = Role,
		   nick = Nick,
		   jid = JID
		  }, StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    #xmlel{name = 'item',
           attrs = [
      #xmlattr{name = 'role', value = role_to_list(Role)},
      #xmlattr{name = 'affiliation', value = affiliation_to_list(Affiliation)},
      #xmlattr{name = 'nick', value = Nick},
      #xmlattr{name = 'jid', value = exmpp_jid:jid_to_list(JID)}]
     }.

search_role(Role, StateData) ->
    lists:filter(
      fun({_, #user{role = R}}) ->
	      Role == R
      end, ?DICT:to_list(StateData#state.users)).

search_affiliation(Affiliation, StateData) ->
    lists:filter(
      fun({_, A}) ->
	      case A of
		  {A1, _Reason} ->
		      Affiliation == A1;
		  _ ->
		      Affiliation == A
	      end
      end, ?DICT:to_list(StateData#state.affiliations)).


process_admin_items_set(UJID, Items, Lang, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, []) of
	{result, Res} ->
	    ?INFO_MSG("Processing MUC admin query from ~s in room ~s:~n ~p",
		      [exmpp_jid:jid_to_list(UJID), exmpp_jid:jid_to_list(StateData#state.jid), Res]),
	    NSD =
		lists:foldl(
		  fun(E, SD) ->
			  case catch (
				 case E of
				     {JID, affiliation, owner, _} ->
                        case exmpp_jid:lnode(JID) of
                            <<>> ->
					            SD;
                     %% TODO: <<>> or 'undefined' ?
                     %% TODO: double case on the E var, because 
                     %%       exmpp_jid:lnode/1 can't be used in guards
					 %% If the provided JID does not have username,
					 %% forget the affiliation completely
                            _ -> case E of 
				     {JID, role, none, Reason} ->
					 catch send_kickban_presence(
						 JID, Reason, "307", SD),
					 set_role(JID, none, SD);
				     {JID, affiliation, none, Reason} ->
					 case (SD#state.config)#config.members_only of
					     true ->
						 catch send_kickban_presence(
							 JID, Reason, "321", SD),
						 SD1 = set_affiliation(JID, none, SD),
						 set_role(JID, none, SD1);
					     _ ->
						 SD1 = set_affiliation(JID, none, SD),
						 send_update_presence(JID, SD1),
						 SD1
					 end;
				     {JID, affiliation, outcast, Reason} ->
					 catch send_kickban_presence(
						 JID, Reason, "301", SD),
					 set_affiliation_and_reason(
					   JID, outcast, Reason,
					   set_role(JID, none, SD));
				     {JID, affiliation, A, Reason} when
					   (A == admin) or (A == owner) ->
					 SD1 = set_affiliation_and_reason(JID, A, Reason, SD),
					 SD2 = set_role(JID, moderator, SD1),
					 send_update_presence(JID, Reason, SD2),
					 SD2;
				     {JID, affiliation, member, Reason} ->
					 SD1 = set_affiliation_and_reason(
						 JID, member, Reason, SD),
					 SD2 = set_role(JID, participant, SD1),
					 send_update_presence(JID, Reason, SD2),
					 SD2;
				     {JID, role, Role, Reason} ->
					 SD1 = set_role(JID, Role, SD),
					 catch send_new_presence(JID, Reason, SD1),
					 SD1;
				     {JID, affiliation, A, _Reason} ->
					 SD1 = set_affiliation(JID, A, SD),
					 send_update_presence(JID, SD1),
					 SD1
				       end
                  end
                end
				) of
			      {'EXIT', ErrReason} ->
				  ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n",
					     [ErrReason]),
				  SD;
			      NSD ->
				  NSD
			  end
		  end, StateData, Res),
	    case (NSD#state.config)#config.persistent of
		true ->
		    mod_muc:store_room(NSD#state.host, NSD#state.room,
				       make_opts(NSD));
		_ ->
		    ok
	    end,
	    {result, [], NSD};
	Err ->
	    Err
    end.


find_changed_items(_UJID, _UAffiliation, _URole, [], _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [#xmlcdata{} | Items],
		   Lang, StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
		   [#xmlel{name = 'item'} = Item | Items],
		   Lang, StateData, Res) ->
    TJID = case exmpp_xml:get_attribute(Item, 'jid',false) of
	       S when S =/= false ->
		   try exmpp_jid:list_to_jid(S) of
		       J ->
			   {value, J}
            catch
		       _:_ ->
			   ErrText = io_lib:format(
				       translate:translate(
					 Lang,
					 "JID ~s is invalid"), [S]),
			   {error, ?ERR(Item, 'not-acceptable', Lang, ErrText)}
		   end;
	       _ ->
		   case exmpp_xml:get_attribute(Item, 'nick', false) of
		       N when N =/= false ->
			   case find_jid_by_nick(N, StateData) of
			       false ->
				   ErrText =
				       io_lib:format(
					 translate:translate(
					   Lang,
					   "Nickname ~s does not exist in the room"),
					 [N]),
				   {error, ?ERR(Item, 'not-acceptable', Lang, ErrText)};
			       J ->
				   {value, J}
			   end;
		       _ ->
			   {error, 'bad-request'}
		   end
	   end,
    case TJID of
	{value, JID} ->
	    TAffiliation = get_affiliation(JID, StateData),
	    TRole = get_role(JID, StateData),
	    case exmpp_xml:get_attribute(Item, 'role',false) of
		false ->
		    case exmpp_xml:get_attribute(Item, 'affiliation', false) of
			false ->
			    {error, 'bad-request'};
			StrAffiliation ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    ErrText1 =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					    [StrAffiliation]),
				    {error, ?ERR(Item, 'not-acceptable', Lang, ErrText1)};
				SAffiliation ->
				    ServiceAf = get_service_affiliation(JID, StateData),
				    CanChangeRA =
					case can_change_ra(
					       UAffiliation, URole,
					       TAffiliation, TRole,
					       affiliation, SAffiliation,
						   ServiceAf) of
					    nothing ->
						nothing;
					    true ->
						true;
					    check_owner ->
						case search_affiliation(
						       owner, StateData) of
						    [{OJID, _}] ->
                            jlib:short_bare_jid(OJID) /= 
                                jlib:short_prepd_bare_jid(UJID);
						    _ ->
							true
						end;
					    _ ->
						false
					end,
				    case CanChangeRA of
					nothing ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, StateData,
					      Res);
					true ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, StateData,
					      [{exmpp_jid:jid_to_bare_jid(JID),
						affiliation,
						SAffiliation,
						exmpp_xml:get_path(
						  Item, [{element, 'reason'},
							 cdata])} | Res]);
					false ->
					    {error, 'not-allowed'}
				    end
			    end
		    end;
		StrRole ->
		    case catch list_to_role(StrRole) of
			{'EXIT', _} ->
			    ErrText1 =
				io_lib:format(
				  translate:translate(
				    Lang,
				    "Invalid role: ~s"),
				  [StrRole]),
			    {error, ?ERR(Item, 'bad-request', Lang, ErrText1)};
			SRole ->
			    ServiceAf = get_service_affiliation(JID, StateData),
			    CanChangeRA =
				case can_change_ra(
				       UAffiliation, URole,
				       TAffiliation, TRole,
				       role, SRole,
					   ServiceAf) of
				    nothing ->
					nothing;
				    true ->
					true;
				    check_owner ->
					case search_affiliation(
					       owner, StateData) of
					    [{OJID, _}] ->
                        jlib:short_bare_jid(OJID) /= 
                            jlib:short_prepd_bare_jid(UJID);
					    _ ->
						true
					end;
				    _ ->
					false
			    end,
			    case CanChangeRA of
				nothing ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, StateData,
				      Res);
				true ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, StateData,
				      [{JID, role, SRole,
					exmpp_xml:get_path(
					  Item, [{element, 'reason'},
						 cdata])} | Res]);
				_ ->
				    {error, 'not-allowed'}
			    end
		    end
	    end;
	Err ->
	    Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items,
		   _Lang, _StateData, _Res) ->
    {error, 'bad-request'}.


can_change_ra(_FAffiliation, _FRole,
	      owner, _TRole,
	      affiliation, owner, owner) ->
    %% A room owner tries to add as persistent owner a
    %% participant that is already owner because he is MUC admin
    true;
can_change_ra(_FAffiliation, _FRole,
	      TAffiliation, _TRole,
	      affiliation, Value, _ServiceAf)
  when (TAffiliation == Value) ->
    nothing;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, TRole,
	      role, Value, _ServiceAf)
  when (TRole == Value) ->
    nothing;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, none, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, member, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, outcast, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, member, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, outcast, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, none, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      admin, _TRole,
	      affiliation, _Affiliation, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
	      owner, _TRole,
	      affiliation, _Affiliation, _ServiceAf) ->
    check_owner;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      affiliation, _Value, _ServiceAf) ->
    false;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, visitor,
	      role, moderator, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, participant,
	      role, moderator, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, visitor, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, visitor, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, participant, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, participant, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      role, _Value, _ServiceAf) ->
    false.


send_kickban_presence(JID, Reason, Code, StateData) ->
    LJID = jlib:short_prepd_jid(JID),
    LJIDs = case LJID of
		{U, S, undefined} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    lists:foreach(fun(J) ->
			  {ok, #user{nick = Nick}} =
			      ?DICT:find(J, StateData#state.users),
			  add_to_log(kickban, {Nick, Reason, Code}, StateData),
			  tab_remove_online_user(J, StateData),
			  send_kickban_presence1(J, Reason, Code, StateData)
		  end, LJIDs).

send_kickban_presence1(UJID, Reason, Code, StateData) ->
    {ok, #user{jid = _RealJID,
	       nick = Nick}} = ?DICT:find(UJID, StateData#state.users),
    {N,D,R} = UJID,
    Affiliation = get_affiliation(exmpp_jid:make_jid(N,D,R), StateData),
    SAffiliation = affiliation_to_list(Affiliation),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs = [#xmlattr{name = 'affiliation', value = SAffiliation},
			           #xmlattr{name = 'role', value = "none"}],
	      ItemEls = case Reason of
			    <<>> ->
				[];
			    _ ->
				[#xmlel{name = 'reason',
                        children = [#xmlcdata{cdata = Reason}]}]
                end,
          Packet = 
            #xmlel{ns = ?NS_JABBER_CLIENT,
                  name = 'presence',
                  attrs = [#xmlattr{name = 'type', value = "unavailable"}],
                  children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
                                 children = [
                                 #xmlel{ns = ?NS_MUC_USER, name = 'item',
                                        attrs = ItemAttrs,
                                        children = ItemEls},
                                 #xmlel{ns = ?NS_MUC_USER, name = 'status',
                                        attrs = [#xmlattr{name='code', 
                                                          value = Code}]}]}]},
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

process_iq_owner(From, set, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    case exmpp_xml:get_child_elements(SubEl) of
		[#xmlel{ns = XMLNS, name = 'x'} = XEl] ->
		    case {XMLNS, exmpp_xml:get_attribute(XEl, 'type',false)} of
			{?NS_DATA_FORMS, "cancel"} ->
			    {result, [], StateData};
			{?NS_DATA_FORMS, "submit"} ->
			    case {check_allowed_log_change(XEl, StateData, From),
					check_allowed_persistent_change(XEl, StateData, From)} of
					{allow, allow} -> set_config(XEl, StateData);
					_ -> {error, 'bad-request'}
				end;
			_ ->
			    {error, 'bad-request'}
		    end;
		[#xmlel{name = 'destroy'} = SubEl1] ->
		    ?INFO_MSG("Destroyed MUC room ~s by the owner ~s", 
			      [exmpp_jid:jid_to_list(StateData#state.jid), exmpp_jid:jid_to_list(From)]),
		    destroy_room(SubEl1, StateData);
		Items ->
		    process_admin_items_set(From, Items, Lang, StateData)
	    end;
	_ ->
	    ErrText = "Owner privileges required",
	    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
    end;

process_iq_owner(From, get, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
	owner ->
	    case exmpp_xml:get_child_elements(SubEl) of
		[] ->
		    get_config(Lang, StateData, From);
		[Item] ->
		    case exmpp_xml:get_attribute(Item, 'affiliation',false) of
			false ->
			    {error, 'bad-request'};
			StrAffiliation ->
			    case catch list_to_affiliation(StrAffiliation) of
				{'EXIT', _} ->
				    ErrText =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					  [StrAffiliation]),
				    {error, ?ERR(SubEl, 'not-acceptable', Lang, ErrText)};
				SAffiliation ->
				    Items = items_with_affiliation(
					      SAffiliation, StateData),
				    {result, Items, StateData}
			    end
		    end;
		_ ->
		    {error, 'feature-not-implemented'}
	    end;
	_ ->
	    ErrText = "Owner privileges required",
	    {error, ?ERR(SubEl, 'forbidden', Lang, ErrText)}
    end.

check_allowed_log_change(XEl, StateData, From) ->
    case lists:keymember("muc#roomconfig_enablelogging", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    allow;
	true ->
	    mod_muc_log:check_access_log(
	      StateData#state.server_host, From)
    end.

check_allowed_persistent_change(XEl, StateData, From) ->
    case lists:keymember("muc#roomconfig_persistentroom", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    allow;
	true ->
		{_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = StateData#state.access,
		acl:match_rule(StateData#state.server_host, AccessPersistent, From)
    end.

-define(XFIELD(Type, Label, Var, Val),
    #xmlel{name = 'field', 
           attrs = [#xmlattr{name = 'type', value = Type},
                    #xmlattr{name = 'label', value = translate:translate(Lang, Label)},
                    #xmlattr{name = 'var', value = Var}],
           children = [#xmlel{name = 'value',
                              children = [#xmlcdata{cdata = Val} ]}]}).


-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> "1";
		    _ -> "0"
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(PRIVATEXFIELD(Label, Var, Val),
	?XFIELD("text-private", Label, Var, Val)).


get_default_room_maxusers(RoomState) ->
    DefRoomOpts = gen_mod:get_module_opt(RoomState#state.server_host, mod_muc, default_room_options, []),
    RoomState2 = set_opts(DefRoomOpts, RoomState),
    (RoomState2#state.config)#config.max_users.

get_config(Lang, StateData, From) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = StateData#state.access,
    ServiceMaxUsers = get_service_max_users(StateData),
    DefaultRoomMaxUsers = get_default_room_maxusers(StateData),
    Config = StateData#state.config,
    {MaxUsersRoomInteger, MaxUsersRoomString} =
	case get_max_users(StateData) of
	    N when is_integer(N) ->
		{N, erlang:integer_to_list(N)};
	    _ -> {0, "none"}
	end,
    Res =
	[#xmlel{name = 'title', children = [ #xmlcdata{cdata =
                translate:translate(Lang, "Configuration for ") ++
                exmpp_jid:jid_to_list(StateData#state.jid)}]},
    #xmlel{name = 'field', attrs = [#xmlattr{name = 'type', value = "hidden"},
                                  #xmlattr{name = 'var', value = "FORM_TYPE"}],
           children = [#xmlel{name = 'value', children = [#xmlcdata{cdata = 
                               <<"http://jabber.org/protocol/muc#roomconfig">>
                        }]}]},
	 ?STRINGXFIELD("Room title",
		       "muc#roomconfig_roomname",
		       Config#config.title),
	 ?STRINGXFIELD("Room description",
		       "muc#roomconfig_roomdesc",
		       Config#config.description)
	] ++
	 case acl:match_rule(StateData#state.server_host, AccessPersistent, From) of
		allow ->
			[?BOOLXFIELD(
			 "Make room persistent",
			 "muc#roomconfig_persistentroom",
			 Config#config.persistent)];
		_ -> []
	 end ++ [
	 ?BOOLXFIELD("Make room public searchable",
		     "muc#roomconfig_publicroom",
		     Config#config.public),
	 ?BOOLXFIELD("Make participants list public",
		     "public_list",
		     Config#config.public_list),
	 ?BOOLXFIELD("Make room password protected",
		     "muc#roomconfig_passwordprotectedroom",
		     Config#config.password_protected),
	 ?PRIVATEXFIELD("Password",
			"muc#roomconfig_roomsecret",
			case Config#config.password_protected of
			    true -> Config#config.password;
			    false -> ""
			end),
     #xmlel{name = 'field', attrs = [
                #xmlattr{name = 'type', value = "list-single"},
                #xmlattr{name = 'label', value = translate:translate(Lang,
                                "Maximum Number of Occupants")},
                #xmlattr{name = 'var', value = "muc#roomconfig_maxusers"}],
            children = [#xmlel{name = 'value',
                               children = [#xmlcdata{cdata = 
                                MaxUsersRoomString}]}] ++
	  if
	      is_integer(ServiceMaxUsers) -> [];
	      true ->
          [#xmlel{name = 'option', attrs = [#xmlattr{name = 'label', 
                        value = translate:translate(Lang, "No limit")}],
                  children = [#xmlel{name = 'value', children = [#xmlcdata{
                                cdata = <<"none">>}]}]}]
	  end ++
      [#xmlel{name = 'option', attrs = [#xmlattr{name = 'label', 
                                    value = erlang:integer_to_list(N)}],
              children = [#xmlel{name = 'value', children = [
                #xmlcdata{cdata = erlang:integer_to_list(N)}]}]} ||
              N <- lists:usort([ServiceMaxUsers, DefaultRoomMaxUsers, MaxUsersRoomInteger |
                               ?MAX_USERS_DEFAULT_LIST]), N =< ServiceMaxUsers]}, 
    #xmlel{name = 'field', attrs = [
                #xmlattr{name = 'type', value = "list-single"},
                #xmlattr{name = 'label', 
                    value = translate:translate(Lang, "Present real JIDs to")},
                #xmlattr{name = 'var', value = "muc#roomconfig_whois"}],
          children = [#xmlel{name = 'value',
                        children = [#xmlcdata{cdata = 
                                if Config#config.anonymous -> <<"moderators">>;
                                   true -> <<"anyone">>
                                end}]},
                     #xmlel{name = 'option', attrs = [
                            #xmlattr{name = 'label', value = 
                              translate:translate(Lang, "moderators only")}],
                           children = [#xmlel{name = 'value', 
                                       children = [#xmlcdata{cdata = 
                                                        <<"moderators">>}]}]},
                     #xmlel{name = 'option', attrs = [
                            #xmlattr{name = 'label', value = 
                              translate:translate(Lang, "anyone")}],
                           children = [#xmlel{name = 'value', 
                                       children = [#xmlcdata{cdata = 
                                                        <<"anyone">>}]}]}]},
         ?BOOLXFIELD("Make room members-only",
                 "muc#roomconfig_membersonly",
                 Config#config.members_only),
         ?BOOLXFIELD("Make room moderated",
                 "muc#roomconfig_moderatedroom",
                 Config#config.moderated),
         ?BOOLXFIELD("Default users as participants",
                 "members_by_default",
                 Config#config.members_by_default),
         ?BOOLXFIELD("Allow users to change subject",
                 "muc#roomconfig_changesubject",
                 Config#config.allow_change_subj),
         ?BOOLXFIELD("Allow users to send private messages",
                 "allow_private_messages",
                 Config#config.allow_private_messages),
         ?BOOLXFIELD("Allow users to query other users",
                 "allow_query_users",
                 Config#config.allow_query_users),
         ?BOOLXFIELD("Allow users to send invites",
                 "muc#roomconfig_allowinvites",
                 Config#config.allow_user_invites),
         ?BOOLXFIELD("Allow visitors to send status text in presence updates",
                 "muc#roomconfig_allowvisitorstatus",
                 Config#config.allow_visitor_status),
         ?BOOLXFIELD("Allow visitors to change nickname",
                 "muc#roomconfig_allowvisitornickchange",
                 Config#config.allow_visitor_nickchange)
        ] ++


	case mod_muc_log:check_access_log(
       StateData#state.server_host, From) of
	    allow ->
		[?BOOLXFIELD(
		    "Enable logging",
		    "muc#roomconfig_enablelogging",
		    Config#config.logging)];
	    _ -> []
	end,
    {result , [#xmlel{name = 'instructions', children = [
                #xmlcdata{cdata = translate:translate(Lang,
		         "You need an x:data capable client to configure room")}]},
                 #xmlel{ns = ?NS_DATA_FORMS, name = 'x', 
                attrs = [#xmlattr{name = 'type', value = "form"}],
                children = Res}],
        StateData}.



set_config(XEl, StateData) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
	invalid ->
	    {error, 'bad-request'};
	_ ->
	    case set_xoption(XData, StateData#state.config) of
		#config{} = Config ->
		    Res = change_config(Config, StateData),
		    {result, _, NSD} = Res,
		    add_to_log(roomconfig_change, [], NSD),
		    Res;
		Err ->
		    Err
	    end
    end.

-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	    "0" -> set_xoption(Opts, Config#config{Opt = false});
	    "false" -> set_xoption(Opts, Config#config{Opt = false});
	    "1" -> set_xoption(Opts, Config#config{Opt = true});
	    "true" -> set_xoption(Opts, Config#config{Opt = true});
	    _ -> {error, 'bad-request'}
	end).

-define(SET_NAT_XOPT(Opt, Val),
	case catch list_to_integer(Val) of
	    I when is_integer(I),
	           I > 0 ->
		set_xoption(Opts, Config#config{Opt = I});
	    _ ->
		{error, 'bad-request'}
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, Config#config{Opt = Val})).


set_xoption([], Config) ->
    Config;
set_xoption([{"muc#roomconfig_roomname", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(title, Val);
set_xoption([{"muc#roomconfig_roomdesc", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(description, Val);
set_xoption([{"muc#roomconfig_changesubject", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_change_subj, Val);
set_xoption([{"allow_query_users", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_query_users, Val);
set_xoption([{"allow_private_messages", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_private_messages, Val);
set_xoption([{"muc#roomconfig_allowvisitorstatus", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_status, Val);
set_xoption([{"muc#roomconfig_allowvisitornickchange", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_nickchange, Val);
set_xoption([{"muc#roomconfig_publicroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public, Val);
set_xoption([{"public_list", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public_list, Val);
set_xoption([{"muc#roomconfig_persistentroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(persistent, Val);
set_xoption([{"muc#roomconfig_moderatedroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(moderated, Val);
set_xoption([{"members_by_default", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_by_default, Val);
set_xoption([{"muc#roomconfig_membersonly", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_only, Val);
set_xoption([{"muc#roomconfig_allowinvites", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_user_invites, Val);
set_xoption([{"muc#roomconfig_passwordprotectedroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{"muc#roomconfig_roomsecret", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(password, Val);
set_xoption([{"anonymous", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{"muc#roomconfig_whois", [Val]} | Opts], Config) ->
    case Val of
	"moderators" ->
	    ?SET_BOOL_XOPT(anonymous, "1");
	"anyone" ->
	    ?SET_BOOL_XOPT(anonymous, "0");
	_ ->
	    {error, 'bad-request'}
    end;
set_xoption([{"muc#roomconfig_maxusers", [Val]} | Opts], Config) ->
    case Val of
	"none" ->
	    ?SET_STRING_XOPT(max_users, none);
	_ ->
	    ?SET_NAT_XOPT(max_users, Val)
    end;
set_xoption([{"muc#roomconfig_enablelogging", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([{"FORM_TYPE", _} | Opts], Config) ->
    %% Ignore our FORM_TYPE
    set_xoption(Opts, Config);
set_xoption([_ | _Opts], _Config) ->
    {error, 'bad-request'}.


change_config(Config, StateData) ->
    NSD = StateData#state{config = Config},
    case {(StateData#state.config)#config.persistent,
	  Config#config.persistent} of
	{_, true} ->
	    mod_muc:store_room(NSD#state.host, NSD#state.room, make_opts(NSD));
	{true, false} ->
	    mod_muc:forget_room(NSD#state.host, NSD#state.room);
	{false, false} ->
	    ok
    end,
    case {(StateData#state.config)#config.members_only,
          Config#config.members_only} of
	{false, true} ->
	    NSD1 = remove_nonmembers(NSD),
	    {result, [], NSD1};
	_ ->
	    {result, [], NSD}
    end.

remove_nonmembers(StateData) ->
    lists:foldl(
      fun({_LJID, #user{jid = JID}}, SD) ->
	    Affiliation = get_affiliation(JID, SD),
	    case Affiliation of
		none ->
		    catch send_kickban_presence(
			    JID, "", "322", SD),
		    set_role(JID, none, SD);
		_ ->
		    SD
	    end
      end, StateData, ?DICT:to_list(StateData#state.users)).


-define(CASE_CONFIG_OPT(Opt),
	Opt -> StateData#state{
		 config = (StateData#state.config)#config{Opt = Val}}).

set_opts([], StateData) ->
    StateData;
set_opts([{Opt, Val} | Opts], StateData) ->
    NSD = case Opt of
	      title -> StateData#state{config = (StateData#state.config)#config{title = Val}};
	      description -> StateData#state{config = (StateData#state.config)#config{description = Val}};
	      allow_change_subj -> StateData#state{config = (StateData#state.config)#config{allow_change_subj = Val}};
	      allow_query_users -> StateData#state{config = (StateData#state.config)#config{allow_query_users = Val}};
	      allow_private_messages -> StateData#state{config = (StateData#state.config)#config{allow_private_messages = Val}};
	      allow_visitor_nickchange -> StateData#state{config = (StateData#state.config)#config{allow_visitor_nickchange = Val}};
	      allow_visitor_status -> StateData#state{config = (StateData#state.config)#config{allow_visitor_status = Val}};
	      public -> StateData#state{config = (StateData#state.config)#config{public = Val}};
	      public_list -> StateData#state{config = (StateData#state.config)#config{public_list = Val}};
	      persistent -> StateData#state{config = (StateData#state.config)#config{persistent = Val}};
	      moderated -> StateData#state{config = (StateData#state.config)#config{moderated = Val}};
	      members_by_default -> StateData#state{config = (StateData#state.config)#config{members_by_default = Val}};
	      members_only -> StateData#state{config = (StateData#state.config)#config{members_only = Val}};
	      allow_user_invites -> StateData#state{config = (StateData#state.config)#config{allow_user_invites = Val}};
	      password_protected -> StateData#state{config = (StateData#state.config)#config{password_protected = Val}};
	      password -> StateData#state{config = (StateData#state.config)#config{password = Val}};
	      anonymous -> StateData#state{config = (StateData#state.config)#config{anonymous = Val}};
	      logging -> StateData#state{config = (StateData#state.config)#config{logging = Val}};
	      max_users ->
		  ServiceMaxUsers = get_service_max_users(StateData),
		  MaxUsers = if
				 Val =< ServiceMaxUsers -> Val;
				 true -> ServiceMaxUsers
			     end,
		  StateData#state{
		    config = (StateData#state.config)#config{
			       max_users = MaxUsers}};
	      affiliations ->
		  StateData#state{affiliations = ?DICT:from_list(Val)};
	      subject ->
		  StateData#state{subject = Val};
	      subject_author ->
		  StateData#state{subject_author = Val};
	      _ -> StateData
	  end,
    set_opts(Opts, NSD).

-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).

make_opts(StateData) ->
    Config = StateData#state.config,
    [
     ?MAKE_CONFIG_OPT(title),
     ?MAKE_CONFIG_OPT(description),
     ?MAKE_CONFIG_OPT(allow_change_subj),
     ?MAKE_CONFIG_OPT(allow_query_users),
     ?MAKE_CONFIG_OPT(allow_private_messages),
     ?MAKE_CONFIG_OPT(allow_visitor_status),
     ?MAKE_CONFIG_OPT(allow_visitor_nickchange),
     ?MAKE_CONFIG_OPT(public),
     ?MAKE_CONFIG_OPT(public_list),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(password),
     ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging),
     ?MAKE_CONFIG_OPT(max_users),
     {affiliations, ?DICT:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author}
    ].



destroy_room(DEl, StateData) ->
    lists:foreach(
      fun({_LJID, Info}) ->
	      Nick = Info#user.nick,
	      ItemAttrs = [#xmlattr{name = 'affiliation', value = "none"},
			   #xmlattr{name = 'role', value = "none"}],
          Packet = #xmlel{ns = ?NS_JABBER_CLIENT,
                          name = 'presence', 
                          attrs = [#xmlattr{name = 'type', 
                                            value = "unavailable"}],
                          children = [
                            #xmlel{ns = ?NS_MUC_USER, name = 'x', children =
                                    [#xmlel{name = 'item', attrs = ItemAttrs},
                                     DEl]}]},
	      ejabberd_router:route(
		jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)),
    case (StateData#state.config)#config.persistent of
	true ->
	    mod_muc:forget_room(StateData#state.host, StateData#state.room);
	false ->
	    ok
	end,
    {result, [], stop}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

-define(FEATURE(Var), #xmlel{name = 'feature', 
                        attrs = [#xmlattr{name = 'var', value = Var}]}).

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
    case Opt of
	true ->
	    ?FEATURE(Fiftrue);
	false ->
	    ?FEATURE(Fiffalse)
    end).

process_iq_disco_info(_From, set, _Lang, _StateData) ->
    {error, 'not-allowed'};

process_iq_disco_info(_From, get, Lang, StateData) ->
    Config = StateData#state.config,
    {result, [ #xmlel{name = 'identity',
                      attrs = [#xmlattr{name = 'category', 
                                        value = "conference"},
                               #xmlattr{name = 'type', value = "text"},
                        	   #xmlattr{name = 'name', 
                                        value = get_title(StateData)}]},
               #xmlel{name = 'feature', 
                      attrs = [#xmlattr{name = 'var', value = ?NS_MUC_s}]},
    
	      ?CONFIG_OPT_TO_FEATURE(Config#config.public,
				     "muc_public", "muc_hidden"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.persistent,
				     "muc_persistent", "muc_temporary"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.members_only,
				     "muc_membersonly", "muc_open"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.anonymous,
				     "muc_semianonymous", "muc_nonanonymous"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.moderated,
				     "muc_moderated", "muc_unmoderated"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.password_protected,
				     "muc_passwordprotected", "muc_unsecured")
	     ] ++ iq_disco_info_extras(Lang, StateData), StateData}.

-define(RFIELDT(Type, Var, Val),
    #xmlel{name = 'field', attrs = [#xmlattr{name = 'type', value = Type},
                                    #xmlattr{name = 'var', value = Var}],
           children = [#xmlel{name = 'value', 
                             children = [#xmlcdata{cdata = Val}]}]}).

-define(RFIELD(Label, Var, Val),
    #xmlel{name = 'field', attrs = [#xmlattr{name = 'label', value =    
                                        translate:translate(Lang, Label)},
                                    #xmlattr{name = 'var', value = Var}],
            children = [#xmlel{name = 'value', children = [
                            #xmlcdata{cdata = Val}]}]}).

iq_disco_info_extras(Lang, StateData) ->
    Len = length(?DICT:to_list(StateData#state.users)),
    RoomDescription = (StateData#state.config)#config.description,
    [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', 
             attrs = [#xmlattr{name = 'type', value = "result"}],
             children = 
      [?RFIELDT("hidden", "FORM_TYPE",
		"http://jabber.org/protocol/muc#roominfo"),
       ?RFIELD("Room description", "muc#roominfo_description",
	       RoomDescription),
       ?RFIELD("Number of occupants", "muc#roominfo_occupants",
	       integer_to_list(Len))
      ]}].

process_iq_disco_items(_From, set, _Lang, _StateData) ->
    {error, 'not-allowed'};

process_iq_disco_items(From, get, _Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    FRole = get_role(From, StateData),
    case ((StateData#state.config)#config.public_list == true) orelse
	(FRole /= none) orelse
	(FAffiliation == admin) orelse
	(FAffiliation == owner) of
	true ->
	    UList =
		lists:map(
		  fun({_LJID, Info}) ->
			  Nick = Info#user.nick,
              #xmlel{name = 'item', attrs = [#xmlattr{name = 'jid', 
                                            value = exmpp_jid:jid_to_list(
                                                    StateData#state.room,
                              				        StateData#state.host,
                            				        Nick)},
                                            #xmlattr{name = 'name', 
                                                     value = Nick}]}
		  end,
		  ?DICT:to_list(StateData#state.users)),
	    {result, UList, StateData};
	_ ->
	    {error, 'forbidden'}
    end.

get_title(StateData) ->
    case (StateData#state.config)#config.title of
	"" ->
	    StateData#state.room;
	Name ->
	    Name
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Invitation support

check_invitation(From, Els, Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites
	orelse (FAffiliation == admin) orelse (FAffiliation == owner),
    InviteEl = case Els of
            [#xmlel{ns = XMLNS, name = 'x'} = XEl] ->
                case XMLNS of
                    ?NS_MUC_USER -> ok;
                    _ -> throw({error, 'bad-request'})
                end,
                case exmpp_xml:get_child_elements(XEl) of
                    [#xmlel{name = 'invite'} = InviteEl1] ->
                        InviteEl1;
                    _ ->
                        throw({error, 'bad-request'})
                end;
            _ -> 
                throw({error, 'bad-request'})
            end,
    JID = try exmpp_jid:list_to_jid(exmpp_xml:get_attribute(InviteEl,
                                                            'to',
                                                            false)) of
	        JID1 -> JID1
          catch
	      _:_ ->
    		  throw({error, 'jid-malformed'})
	      end,
    case CanInvite of
	false ->
	    throw({error, 'not-allowed'});
	true ->
	    Reason =
		exmpp_xml:get_path(
		  InviteEl,
		  [{element, 'reason'}, cdata]),
	    ContinueEl =
		case exmpp_xml:get_path(
		       InviteEl,
		       [{element, 'continue'}]) of
		    'undefined' -> [];
		    Continue1 -> [Continue1]
		end,
	    IEl =
	    [#xmlel{ns = ?NS_MUC_USER,
                name = 'invite', 
		        attrs = [#xmlattr{name = 'from', 
		                          value = exmpp_jid:jid_to_list(From)}],
 		        children = [#xmlel{ns =?NS_MUC_USER, name = 'reason', 
		                            children = [#xmlcdata{cdata = Reason} ]}] 
  		                    ++ ContinueEl}],
	    PasswdEl =
		case (StateData#state.config)#config.password_protected of
		    true ->
			[#xmlel{ns = ?NS_MUC_USER, name = 'password',
			    children = [#xmlcdata{cdata = 
				(StateData#state.config)#config.password}]}];
		    _ ->
			[]
		end,
	    Body =
	    #xmlel{name = 'body',
	      children = [#xmlcdata{cdata =
		  list_to_binary([
		    io_lib:format(
		      translate:translate(Lang,
			"~s invites you to the room ~s"),
		      [exmpp_jid:jid_to_list(From),
			exmpp_jid:jid_to_list(StateData#state.room,
			  StateData#state.host,
			  "")
		      ]), 
		   case (StateData#state.config)#config.password_protected of
		       true ->
			   ", " ++
			       translate:translate(Lang, "the password is") ++
			       " '" ++
			       (StateData#state.config)#config.password ++ "'";
		       _ ->
			   ""
		   end,
		   case Reason of
		       <<>> -> "";
		       _ -> [" (",  Reason, ") "]
		   end
          ])}]},
        %%TODO: always NS_JABBER_CLIENT?
	    Msg =
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', 
	      attrs = [#xmlattr{name = 'type', value = "normal"}], 
	      children = [#xmlel{ns = ?NS_MUC_USER, name = 'x',
		  children = IEl ++ PasswdEl},
		#xmlel{ns = 'jabber:x:conference', name = 'x',
		  attrs = [#xmlattr{name = 'jid',
		      value = exmpp_jid:jid_to_list(
			StateData#state.room,
			StateData#state.host,
			"")}],
		  children = [#xmlcdata{cdata = Reason}]},
		Body]},
	    ejabberd_router:route(StateData#state.jid, JID, Msg),
	    JID
    end.

%% Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From) ->
    case catch check_decline_invitation(Packet) of
	{true, Decline_data} ->
	    send_decline_invitation(Decline_data, StateData#state.jid, From);
	_ ->
	    send_error_only_occupants(Packet, Lang, StateData#state.jid, From)
    end.

%% Check in the packet is a decline.
%% If so, also returns the splitted packet.
%% This function must be catched, 
%% because it crashes when the packet is not a decline message.
check_decline_invitation(Packet) ->
    #xmlel{name = 'message'} = Packet,
    #xmlel{ns = ?NS_MUC_USER} = XEl = exmpp_xml:get_element(Packet, 'x'),
    DEl = exmpp_xml:get_element(XEl, 'decline'),
    ToString = exmpp_xml:get_attribute(DEl, 'to', false),
    ToJID = exmpp_jid:list_to_jid(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.

%% Send the decline to the inviter user.
%% The original stanza must be slightly modified.
send_decline_invitation({Packet, XEl, DEl = #xmlel{name='decline'}, ToJID}, 
                            RoomJID, FromJID) ->
    FromString = exmpp_jid:jid_to_list(FromJID),

    DEl1 = exmpp_xml:remove_attribute(DEl, 'to'),
    DEl2 = exmpp_xml:set_attribute(DEl1, 'from',FromString),
    XEl2 = replace_subelement(XEl,DEl2),
    Packet2 = replace_subelement(Packet,XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% Given an element and a new subelement, 
%% replace the instance of the subelement in element with the new subelement.
replace_subelement(#xmlel{children = Els} = El, #xmlel{name = Name} = NewSubEl) ->
    Els2 = lists:map(fun(#xmlel{name = Name2}) when Name2 =:= Name -> NewSubEl;
                        (S) -> S
                     end, Els),
    exmpp_xml:set_children(El, Els2).

send_error_only_occupants(Packet, Lang, RoomJID, From) ->
    ErrText = "Only occupants are allowed to send messages to the conference",
    Err = exmpp_stanza:reply_with_error(
			    Packet, ?ERR(Packet, 'not-acceptable', Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

add_to_log(Type, Data, StateData) ->
    case (StateData#state.config)#config.logging of
	true ->
	    mod_muc_log:add_to_log(
	      StateData#state.server_host, Type, Data,
	      StateData#state.jid, make_opts(StateData));
	false ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

tab_add_online_user(JID, StateData) ->
    LUser = exmpp_jid:lnode(JID),
    LServer = exmpp_jid:ldomain(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
	    muc_online_users,
	    #muc_online_users{us = US, room = Room, host = Host}).



tab_remove_online_user(JID, StateData) when ?IS_JID(JID) ->
 LUser = exmpp_jid:lnode(JID),
 LServer = exmpp_jid:ldomain(JID),
    tab_remove_online_user({LUser, LServer, none},StateData);

tab_remove_online_user({LUser, LServer,_}, StateData) ->
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
	    muc_online_users,
	    #muc_online_users{us = US, room = Room, host = Host}).

tab_count_user(JID) ->
    LUser = exmpp_jid:lnode(JID),
    LServer = exmpp_jid:ldomain(JID),
    US = {LUser, LServer},
    case catch ets:select(
		 muc_online_users,
		 [{#muc_online_users{us = US, _ = '_'}, [], [[]]}]) of
	Res when is_list(Res) ->
	    length(Res);
	_ ->
	    0
    end.
    
    
jid_replace_resource(JID, Resource) ->
    exmpp_jid:bare_jid_to_jid(JID, Resource).




%%%----------------------------------------------------------------------
%%% File    : mod_privacy_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created :  5 Oct 2006 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_privacy_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_iq_set/4,
	 process_iq_get/5,
	 get_user_list/3,
	 check_packet/6,
	 updated_list/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(privacy_iq_get, Host,
		       ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, Host,
		       ?MODULE, process_iq_set, 50),
    ejabberd_hooks:add(privacy_get_user_list, Host,
		       ?MODULE, get_user_list, 50),
    ejabberd_hooks:add(privacy_check_packet, Host,
		       ?MODULE, check_packet, 50),
    ejabberd_hooks:add(privacy_updated_list, Host,
		       ?MODULE, updated_list, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(privacy_iq_get, Host,
			  ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, Host,
			  ?MODULE, process_iq_set, 50),
    ejabberd_hooks:delete(privacy_get_user_list, Host,
			  ?MODULE, get_user_list, 50),
    ejabberd_hooks:delete(privacy_check_packet, Host,
			  ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(privacy_updated_list, Host,
			  ?MODULE, updated_list, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY).

process_iq(_From, _To, IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_iq_get(_, From, _To, #iq{payload = SubEl},
	       #userlist{name = Active}) ->
    #jid{lnode = LUser, ldomain = LServer} = From,
    case exmpp_xml:get_child_elements(SubEl) of
	[] ->
	    process_lists_get(LUser, LServer, Active);
	[#xmlel{name = Name} = Child] ->
	    case Name of
		list ->
		    ListName = exmpp_xml:get_attribute(Child, name, false),
		    process_list_get(LUser, LServer, ListName);
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end.


process_lists_get(LUser, LServer, Active) ->
    Default = case catch sql_get_default_privacy_list(LUser, LServer) of
		  {selected, ["name"], []} ->
		      none;
		  {selected, ["name"], [{DefName}]} ->
		      DefName;
		  _ ->
		      none
	      end,
    case catch sql_get_privacy_list_names(LUser, LServer) of
	{selected, ["name"], []} ->
	    {result, #xmlel{ns = ?NS_PRIVACY, name = 'query'}};
	{selected, ["name"], Names} ->
	    LItems = lists:map(
		       fun({N}) ->
			       exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = list}, name, N)
		       end, Names),
	    DItems =
		case Default of
		    none ->
			LItems;
		    _ ->
			[exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = default}, name, Default) | LItems]
		end,
	    ADItems =
		case Active of
		    none ->
			DItems;
		    _ ->
			[exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = active}, name, Active) | DItems]
		end,
		{result, #xmlel{ns = ?NS_PRIVACY, name = 'query', children = ADItems}};
	_ ->
	    {error, 'internal-server-error'}
    end.

process_list_get(_LUser, _LServer, false) ->
    {error, 'bad-request'};

process_list_get(LUser, LServer, Name) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
	{selected, ["id"], []} ->
	    {error, 'item-not-found'};
	{selected, ["id"], [{ID}]} ->
	    case catch sql_get_privacy_list_data_by_id(ID, LServer) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    Items = lists:map(fun raw_to_item/1, RItems),
		    LItems = lists:map(fun item_to_xml/1, Items),
            ListEl = exmpp_xml:set_attribute(#xmlel{name = list,
                                                  ns = ?NS_PRIVACY,
                                                  children = LItems},
                                           name,
                                           Name),
            {result, #xmlel{ns = ?NS_PRIVACY, name = 'query', children = [ListEl]}};
        _ ->
		    {error, 'internal-server-error'}
	    end;
	_ ->
	    {error, 'internal-server-error'}
    end.


item_to_xml(Item) ->
    Attrs1 = [#xmlattr{name = 'action', value = action_to_list(Item#listitem.action)},
	      #xmlattr{name = 'order', value = order_to_list(Item#listitem.order)}],
    Attrs2 = case Item#listitem.type of
		 none ->
		     Attrs1;
		 Type ->
		     [#xmlattr{name = 'type', value = type_to_list(Item#listitem.type)},
		      #xmlattr{name = 'value', value = value_to_list(Type, Item#listitem.value)} |
		      Attrs1]
	     end,
    SubEls = case Item#listitem.match_all of
		 true ->
		     [];
		 false ->
		     SE1 = case Item#listitem.match_iq of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = iq}];
			       false ->
				   []
			   end,
		     SE2 = case Item#listitem.match_message of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = message} | SE1];
			       false ->
				   SE1
			   end,
		     SE3 = case Item#listitem.match_presence_in of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = 'presence-in'} | SE2];
			       false ->
				   SE2
			   end,
		     SE4 = case Item#listitem.match_presence_out of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = 'presence-out'} | SE3];
			       false ->
				   SE3
			   end,
		     SE4
	     end,
    exmpp_xml:set_attributes(#xmlel{ns = ?NS_PRIVACY, name = item, children = SubEls}, Attrs2).


action_to_list(Action) ->
    case Action of
	allow -> "allow";
	deny -> "deny"
    end.

order_to_list(Order) ->
    integer_to_list(Order).

type_to_list(Type) ->
    case Type of
	jid -> "jid";
	group -> "group";
	subscription -> "subscription"
    end.

value_to_list(Type, Val) ->
    case Type of
	jid ->
	    {N, D, R} = Val,
	    exmpp_jid:jid_to_list(N, D, R);
	group -> Val;
	subscription ->
	    case Val of
		both -> "both";
		to -> "to";
		from -> "from";
		none -> "none"
	    end
    end.



list_to_action(S) ->
    case S of
	"allow" -> allow;
	"deny" -> deny
    end.



process_iq_set(_, From, _To, #iq{payload = SubEl}) ->
    #jid{lnode = LUser, ldomain = LServer} = From,
    case exmpp_xml:get_child_elements(SubEl) of
	[#xmlel{name = Name} = Child] ->
	    ListName = exmpp_xml:get_attribute(Child, 'name', false),
	    case Name of
		list ->
		    process_list_set(LUser, LServer, ListName,
				     exmpp_xml:get_child_elements(Child));
		active ->
		    process_active_set(LUser, LServer, ListName);
		default ->
		    process_default_set(LUser, LServer, ListName);
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end.


process_default_set(LUser, LServer, false) ->
    case catch sql_unset_default_privacy_list(LUser, LServer) of
	{'EXIT', _Reason} ->
	    {error, 'internal_server_error'};
	{error, _Reason} ->
	    {error, 'internal_server_error'};
	_ ->
	    {result, []}
    end;

process_default_set(LUser, LServer, Name) ->
    F = fun() ->
		case sql_get_privacy_list_names_t(LUser) of
		    {selected, ["name"], []} ->
			  {error, 'item-not-found'};
		    {selected, ["name"], Names} ->
			  case lists:member({Name}, Names) of
			    true ->
				sql_set_default_privacy_list(LUser, Name),
				{result, []};
			    false ->
				{error, 'item-not-found'}
			end
		end
	end,
    case odbc_queries:sql_transaction(LServer, F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, _} = Res} ->
	    Res;
	_ ->
	    {error, 'internal-server-error'}
    end.


process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}};

process_active_set(LUser, LServer,  Name) ->
    case catch sql_get_privacy_list_id(LUser, LServer, Name) of
	{selected, ["id"], []} ->
	    {error, 'item-not-found'};
	{selected, ["id"], [{ID}]} ->
	    case catch sql_get_privacy_list_data_by_id(ID, LServer) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    Items = lists:map(fun raw_to_item/1, RItems),
		    {result, [], #userlist{name = Name, list = Items}};
		_ ->
		    {error, 'internal-server-error'}
	    end;
	_ ->
	    {error, 'internal_server_error'}
    end.



process_list_set(_LUser, _LServer, false, _Els) ->
    {error, 'bad-request'};

process_list_set(LUser, LServer, Name, Els) ->
    case parse_items(Els) of
	false ->
	    {error, 'bad-request'};
	remove ->
	    F =
		fun() ->
			case sql_get_default_privacy_list_t(LUser) of
			    {selected, ["name"], []} ->
				sql_remove_privacy_list(LUser, Name),
				{result, []};
			    {selected, ["name"], [{Default}]} ->
				% TODO: check active
				if
				    Name == Default ->
					{error, 'conflict'};
				    true ->
					sql_remove_privacy_list(LUser, Name),
					{result, []}
				end
			end
		end,
	    case odbc_queries:sql_transaction(LServer, F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ejabberd_router:route(
		      exmpp_jid:make_bare_jid(LUser, LServer),
		      exmpp_jid:make_bare_jid(LUser, LServer),
		      #xmlel{name = 'broadcast', 
			children=[{privacy_list,
				   #userlist{name = Name, list = []},
					     Name}]}),
		    Res;
		_ ->
		    {error, 'internal-server-error'}
	    end;
	List ->
	    RItems = lists:map(fun item_to_raw/1, List),
	    F =
		fun() ->
			ID =
			    case sql_get_privacy_list_id_t(LUser, Name) of
				{selected, ["id"], []} ->
				    sql_add_privacy_list(LUser, Name),
				    {selected, ["id"], [{I}]} =
					sql_get_privacy_list_id_t(LUser, Name),
				    I;
				{selected, ["id"], [{I}]} ->
				    I
			    end,
			sql_set_privacy_list(ID, RItems),
			{result, []}
		end,
	    case odbc_queries:sql_transaction(LServer, F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ejabberd_router:route(
		      exmpp_jid:make_bare_jid(LUser, LServer),
		      exmpp_jid:make_bare_jid(LUser, LServer),
		      #xmlel{name = 'broadcast', 
			children=[{privacy_list,
				   #userlist{name = Name, list = List},
					     Name}]}),
		    Res;
		_ ->
		    {error, 'internal_server_error'}
	    end
    end.



parse_items([]) ->
    remove;
parse_items(Els) ->
    parse_items(Els, []).

parse_items([], Res) ->
    lists:reverse(Res);
parse_items([El = #xmlel{name = item} | Els], Res) ->
    Type   = exmpp_xml:get_attribute(El, type, false),
    Value  = exmpp_xml:get_attribute(El, value, false),
    SAction =exmpp_xml:get_attribute(El, action, false),
    SOrder = exmpp_xml:get_attribute(El, order, false),
    Action = case catch list_to_action(SAction) of
		 {'EXIT', _} -> false;
		 Val -> Val
	     end,
    Order = case catch list_to_integer(SOrder) of
		{'EXIT', _} ->
		    false;
		IntVal ->
		    if
			IntVal >= 0 ->
			    IntVal;
			true ->
			    false
		    end
	    end,
    if
	(Action /= false) and (Order /= false) ->
	    I1 = #listitem{action = Action, order = Order},
	    I2 = case {Type, Value} of
		     {T,  V} when is_list(T), is_list(V) ->
			 case T of
			     "jid" ->
				 try
				     JID = exmpp_jid:list_to_jid(V),
				     I1#listitem{
				       type = jid,
				       value = jlib:short_prepd_jid(JID)}
				 catch
				     _ ->
					 false
				 end;
			     "group" ->
				 I1#listitem{type = group,
					     value = V};
			     "subscription" ->
				 case V of
				     "none" ->
					 I1#listitem{type = subscription,
						     value = none};
				     "both" ->
					 I1#listitem{type = subscription,
						     value = both};
				     "from" ->
					 I1#listitem{type = subscription,
						     value = from};
				     "to" ->
					 I1#listitem{type = subscription,
						     value = to};
				     _ ->
					 false
				 end
			 end;
		     {T, false} when is_list(T) ->
			 false;
		     _ ->
			 I1
		 end,
	    case I2 of
		false ->
		    false;
		_ ->
		    case parse_matches(I2, exmpp_xml:get_child_elements(El)) of
			false ->
			    false;
			I3 ->
			    parse_items(Els, [I3 | Res])
		    end
	    end;
	true ->
	    false
    end;

parse_items(_, _Res) ->
    false.


parse_matches(Item, []) ->
    Item#listitem{match_all = true};
parse_matches(Item, Els) ->
    parse_matches1(Item, Els).

parse_matches1(Item, []) ->
    Item;
parse_matches1(Item, [#xmlel{name = message} | Els]) ->
    parse_matches1(Item#listitem{match_message = true}, Els);
parse_matches1(Item, [#xmlel{name = iq} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item, [#xmlel{name = 'presence-in'} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true}, Els);
parse_matches1(Item, [#xmlel{name = 'presence-out'} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true}, Els);
parse_matches1(_Item, [#xmlel{} | _Els]) ->
    false.







get_user_list(_, User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	case catch sql_get_default_privacy_list(LUser, LServer) of
	    {selected, ["name"], []} ->
		#userlist{};
	    {selected, ["name"], [{Default}]} ->
		case catch sql_get_privacy_list_data(LUser, LServer, Default) of
		    {selected, ["t", "value", "action", "ord", "match_all",
				"match_iq", "match_message",
				"match_presence_in", "match_presence_out"],
		     RItems} ->
			Items = lists:map(fun raw_to_item/1, RItems),
			#userlist{name = Default, list = Items};
		    _ ->
			#userlist{}
		end;
	    _ ->
		#userlist{}
	end
    catch
	_ ->
	    #userlist{}
    end.


check_packet(_, User, Server,
	     #userlist{list = List},
	     {From, To, #xmlel{name = PName}},
	     Dir) when PName =:= message ; 
		       PName =:= iq ;
		       PName =:= presence ->
    case List of
	[] ->
	    allow;
	_ ->
	    case {PName, Dir} of
		{message, in} ->
		    LJID = jlib:short_prepd_jid(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, exmpp_stringprep:nameprep(Server),
			  {none, []}, [User, Server, From]),
		    check_packet_aux(List, message,
				     LJID, Subscription, Groups);
		{iq, in} ->
		    LJID = jlib:short_prepd_jid(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, exmpp_stringprep:nameprep(Server),
			  {none, []}, [User, Server, From]),
		    check_packet_aux(List, iq,
				     LJID, Subscription, Groups);
		{presence, in} ->
		    LJID = jlib:short_prepd_jid(From),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, exmpp_stringprep:nameprep(Server),
			  {none, []}, [User, Server, From]),
		    check_packet_aux(List, presence_in,
				     LJID, Subscription, Groups);
		{presence, out} ->
		    LJID = jlib:short_prepd_jid(To),
		    {Subscription, Groups} =
			ejabberd_hooks:run_fold(
			  roster_get_jid_info, exmpp_stringprep:nameprep(Server),
			  {none, []}, [User, Server, To]),
		    check_packet_aux(List, presence_out,
				     LJID, Subscription, Groups);
		_ ->
		    allow
	    end
    end.

check_packet_aux([], _PType, _JID, _Subscription, _Groups) ->
    allow;
check_packet_aux([Item | List], PType, JID, Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    case is_ptype_match(Item, PType) of
	true ->
	    case Type of
		none ->
		    Action;
		_ ->
		    case is_type_match(Type, Value,
				       JID, Subscription, Groups) of
			true ->
			    Action;
			false ->
			    check_packet_aux(List, PType,
					     JID, Subscription, Groups)
		    end
	    end;
	false ->
	    check_packet_aux(List, PType, JID, Subscription, Groups)
    end.


is_ptype_match(Item, PType) ->
    case Item#listitem.match_all of
	true ->
	    true;
	false ->
	    case PType of
		message ->
		    Item#listitem.match_message;
		iq ->
		    Item#listitem.match_iq;
		presence_in ->
		    Item#listitem.match_presence_in;
		presence_out ->
		    Item#listitem.match_presence_out
	    end
    end.


is_type_match(Type, Value, JID, Subscription, Groups) ->
    case Type of
	jid ->
	    case Value of
		{undefined, Server, undefined} ->
		    case JID of
			{_, Server, _} ->
			    true;
			_ ->
			    false
		    end;
		{User, Server, undefined} ->
		    case JID of
			{User, Server, _} ->
			    true;
			_ ->
			    false
		    end;
		_ ->
		    Value == JID
	    end;
	subscription ->
	    Value == Subscription;
	group ->
	    lists:member(Value, Groups)
    end.


updated_list(_,
	     #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if
	OldName == NewName ->
	    New;
	true ->
	    Old
    end.


raw_to_item({SType, SValue, SAction, SOrder, SMatchAll, SMatchIQ,
	     SMatchMessage, SMatchPresenceIn, SMatchPresenceOut}) ->
    {Type, Value} =
	case SType of
	    "n" ->
		{none, none};
	    "j" ->
		JID = exmpp_jid:list_to_jid(SValue),
		{jid, jlib:short_prepd_jid(JID)};
	    "g" ->
		{group, SValue};
	    "s" ->
		case SValue of
		    "none" ->
			{subscription, none};
		    "both" ->
			{subscription, both};
		    "from" ->
			{subscription, from};
		    "to" ->
			{subscription, to}
		end
	end,
    Action =
	case SAction of
	    "a" -> allow;
	    "d" -> deny
	end,
    Order = list_to_integer(SOrder),
    MatchAll = SMatchAll == "1" orelse SMatchAll == "t",
    MatchIQ = SMatchIQ == "1" orelse SMatchIQ == "t" ,
    MatchMessage =  SMatchMessage == "1" orelse SMatchMessage == "t",
    MatchPresenceIn =  SMatchPresenceIn == "1" orelse SMatchPresenceIn == "t",
    MatchPresenceOut =  SMatchPresenceOut == "1" orelse SMatchPresenceOut == "t",
    #listitem{type = Type,
	      value = Value,
	      action = Action,
	      order = Order,
	      match_all = MatchAll,
	      match_iq = MatchIQ,
	      match_message = MatchMessage,
	      match_presence_in = MatchPresenceIn,
	      match_presence_out = MatchPresenceOut
	     }.

item_to_raw(#listitem{type = Type,
		      value = Value,
		      action = Action,
		      order = Order,
		      match_all = MatchAll,
		      match_iq = MatchIQ,
		      match_message = MatchMessage,
		      match_presence_in = MatchPresenceIn,
		      match_presence_out = MatchPresenceOut
		     }) ->
    {SType, SValue} =
	case Type of
	    none ->
		{"n", ""};
	    jid ->
		{N0, D0, R0} = Value,
		{"j", exmpp_jid:jid_to_list(N0, D0, R0)};
	    group ->
		{"g", Value};
	    subscription ->
		case Value of
		    none ->
			{"s", "none"};
		    both ->
			{"s", "both"};
		    from ->
			{"s", "from"};
		    to ->
			{"s", "to"}
		end
	end,
    SAction =
	case Action of
	    allow -> "a";
	    deny -> "d"
	end,
    SOrder = integer_to_list(Order),
    SMatchAll = if MatchAll -> "1"; true -> "0" end,
    SMatchIQ = if MatchIQ -> "1"; true -> "0" end,
    SMatchMessage = if MatchMessage -> "1"; true -> "0" end,
    SMatchPresenceIn = if MatchPresenceIn -> "1"; true -> "0" end,
    SMatchPresenceOut = if MatchPresenceOut -> "1"; true -> "0" end,
    ["'", SType, "', "
     "'", SValue, "', "
     "'", SAction, "', "
     "'", SOrder, "', "
     "'", SMatchAll, "', "
     "'", SMatchIQ, "', "
     "'", SMatchMessage, "', "
     "'", SMatchPresenceIn, "', "
     "'", SMatchPresenceOut, "'"].


sql_get_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      ["select name from privacy_default_list "
       "where username='", Username, "';"]).

sql_get_default_privacy_list_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query_t(
      ["select name from privacy_default_list "
       "where username='", Username, "';"]).

sql_get_privacy_list_names(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      ["select name from privacy_list "
       "where username='", Username, "';"]).

sql_get_privacy_list_names_t(LUser) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query_t(
      ["select name from privacy_list "
       "where username='", Username, "';"]).

sql_get_privacy_list_id(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query(
      LServer,
      ["select id from privacy_list "
       "where username='", Username, "' and name='", SName, "';"]).

sql_get_privacy_list_id_t(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["select id from privacy_list "
       "where username='", Username, "' and name='", SName, "';"]).

sql_get_privacy_list_data(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query(
      LServer,
      ["select t, value, action, ord, match_all, match_iq, "
       "match_message, match_presence_in, match_presence_out "
       "from privacy_list_data "
       "where id = (select id from privacy_list where "
       "            username='", Username, "' and name='", SName, "') "
       "order by ord;"]).

sql_get_privacy_list_data_by_id(ID, LServer) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select t, value, action, ord, match_all, match_iq, "
       "match_message, match_presence_in, match_presence_out "
       "from privacy_list_data "
       "where id='", ID, "' order by ord;"]).

sql_set_default_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["delete from privacy_default_list "
       "      where username='", Username, "';"]),
    ejabberd_odbc:sql_query_t(
      ["insert into privacy_default_list(username, name) "
       "values ('", Username, "', '", SName, "');"]).

sql_unset_default_privacy_list(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from privacy_default_list "
       "      where username='", Username, "';"]).

sql_remove_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["delete from privacy_list "
       "where username='", Username, "' and name='", SName, "';"]).

sql_add_privacy_list(LUser, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    ejabberd_odbc:sql_query_t(
      ["insert into privacy_list(username, name) "
       "values ('", Username, "', '", SName, "');"]).

sql_set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t(
      ["delete from privacy_list_data "
       "where id='", ID, "';"]),
    lists:foreach(fun(Items) ->
			  ejabberd_odbc:sql_query_t(
			    ["insert into privacy_list_data("
			     "id, t, value, action, ord, match_all, match_iq, "
			     "match_message, match_presence_in, "
			     "match_presence_out "
			     ") "
		 	     "values ('", ID, "', ", Items, ");"])
		  end, RItems).

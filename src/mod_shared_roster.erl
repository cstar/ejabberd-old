%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_shared_roster).
-author('alexey@sevcom.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 webadmin_menu/2, webadmin_page/3,
	 get_user_roster/2,
	 get_subscription_lists/3,
	 get_jid_info/4,
	 process_item/2,
	 in_subscription/6,
	 out_subscription/4,
	 list_groups/1,
	 create_group/2,
	 create_group/3,
	 delete_group/2,
	 get_group_opts/2,
	 set_group_opts/3,
	 get_group_users/2,
	 get_group_explicit_users/2,
	 add_user_to_group/3,
	 remove_user_from_group/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(sr_group, {group_host, opts}).
-record(sr_user, {us, group_host}).

start(Host, _Opts) ->
    mnesia:create_table(sr_group,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, sr_group)}]),
    mnesia:create_table(sr_user,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, sr_user)}]),
    mnesia:add_table_index(sr_user, group_host),
    ejabberd_hooks:add(webadmin_menu_host, Host,
		       ?MODULE, webadmin_menu, 70),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(roster_get, Host,
		       ?MODULE, get_user_roster, 70),
    ejabberd_hooks:add(roster_in_subscription, Host,
        	       ?MODULE, in_subscription, 30),
    ejabberd_hooks:add(roster_out_subscription, Host,
        	       ?MODULE, out_subscription, 30),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:add(roster_get_jid_info, Host,
        	       ?MODULE, get_jid_info, 70),
    ejabberd_hooks:add(roster_process_item, Host,
        	       ?MODULE, process_item, 50).
    %ejabberd_hooks:add(remove_user, Host,
    %    	       ?MODULE, remove_user, 50),

stop(Host) ->
    ejabberd_hooks:delete(webadmin_menu_host, Host,
			  ?MODULE, webadmin_menu, 70),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(roster_get, Host,
			  ?MODULE, get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, Host,
        		  ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, Host,
        		  ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
        		  ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
        		  ?MODULE, get_jid_info, 70),
    ejabberd_hooks:delete(roster_process_item, Host,
			  ?MODULE, process_item, 50).
    %ejabberd_hooks:delete(remove_user, Host,
    %    		  ?MODULE, remove_user, 50),


get_user_roster(Items, US) ->
    {U, S} = US,
    DisplayedGroups = get_user_displayed_groups(US),
    %% Get shared roster users in all groups and remove self: 
    SRUsers = 
	lists:foldl(
	  fun(Group, Acc1) ->
		  lists:foldl(
		    fun(User, Acc2) ->
			    if User == US -> Acc2;
			       true -> dict:append(User, 
						   get_group_name(S, Group),
						   Acc2)
			    end
		    end, Acc1, get_group_users(S, Group))
	  end, dict:new(), DisplayedGroups),

    %% If partially subscribed users are also in shared roster, show them as
    %% totally subscribed:
    {NewItems1, SRUsersRest} =
	lists:mapfoldl(
	  fun(Item, SRUsers1) ->
		  {_, _, {U1, S1, _}} = Item#roster.usj,
		  US1 = {U1, S1},
		  case dict:find(US1, SRUsers1) of
		      {ok, _GroupNames} ->
			  {Item#roster{subscription = both, ask = none},
			   dict:erase(US1, SRUsers1)};
		      error ->
			  {Item, SRUsers1}
		  end
	  end, SRUsers, Items),
    
    %% Export items in roster format:
    SRItems = [#roster{usj = {U, S, {U1, S1, ""}},
		       us = US,
		       jid = {U1, S1, ""},
		       name = "",
		       subscription = both,
		       ask = none,
		       groups = GroupNames} ||
		  {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

%% This function in use to rewrite the roster entries when moving or renaming
%% them in the user contact list.
process_item(RosterItem, Host) ->
    USFrom = RosterItem#roster.us,
    {User,Server,_Resource} = RosterItem#roster.jid,
    USTo = {User,Server},
    DisplayedGroups = get_user_displayed_groups(USFrom),
    CommonGroups = lists:filter(fun(Group) ->
					is_user_in_group(USTo, Group, Host)
				end, DisplayedGroups),
    case CommonGroups of
	[] -> RosterItem;
	%% Roster item cannot be removed: We simply reset the original groups:
	_ when RosterItem#roster.subscription == remove ->
	    GroupNames = lists:map(fun(Group) ->
					   get_group_name(Host, Group)
				   end, CommonGroups),
	    RosterItem#roster{subscription = both, ask = none,
			      groups=[GroupNames]};
	_ -> RosterItem#roster{subscription = both, ask = none}
    end.

get_subscription_lists({F, T}, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    get_group_users(LServer, Group)
	    end, DisplayedGroups)),
    SRJIDs = [{U1, S1, ""} || {U1, S1} <- SRUsers],
    {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}.

get_jid_info({Subscription, Groups}, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jlib:jid_tolower(JID),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers = 
	lists:foldl(
	  fun(Group, Acc1) ->
		  lists:foldl(
		    fun(User1, Acc2) ->
			    dict:append(
			      User1, get_group_name(LServer, Group), Acc2)
		    end, Acc1, get_group_users(LServer, Group))
	  end, dict:new(), DisplayedGroups),
    case dict:find(US1, SRUsers) of
	{ok, GroupNames} ->
	    NewGroups = if
			    Groups == [] -> GroupNames;
			    true -> Groups
			end,
	    {both, NewGroups};
	error ->
	    {Subscription, Groups}
    end.

in_subscription(Acc, User, Server, JID, Type, _Reason) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, false).

process_subscription(Direction, User, Server, JID, _Type, Acc) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups(US),
    SRUsers =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    get_group_users(LServer, Group)
	    end, DisplayedGroups)),
    case lists:member(US1, SRUsers) of
	true ->
	    case Direction of
		in ->
		    {stop, false};
		out ->
		    stop
	    end;
	false ->
	    Acc
    end.

list_groups(Host) ->
    mnesia:dirty_select(
      sr_group,
      [{#sr_group{group_host = {'$1', '$2'},
		  _ = '_'},
	[{'==', '$2', Host}],
	['$1']}]).

create_group(Host, Group) ->
    create_group(Host, Group, []).

create_group(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

delete_group(Host, Group) ->
    GroupHost = {Group, Host},
    F = fun() ->
		%% Delete the group ...
		mnesia:delete({sr_group, GroupHost}),
		%% ... and its users
		Users = mnesia:index_read(sr_user, GroupHost, #sr_user.group_host),
		lists:foreach(fun(UserEntry) ->
				      mnesia:delete_object(UserEntry)
			      end, Users)
	end,
    mnesia:transaction(F).

get_group_opts(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end.

set_group_opts(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

get_user_groups(US) ->
    Host = element(2, US),
    case catch mnesia:dirty_read(sr_user, US) of
	Rs when is_list(Rs) ->
	    [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
	_ ->
	    []
    end ++ get_all_users_groups(Host).

is_group_enabled(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    not lists:member(disabled, Opts);
	_ ->
	    false
    end.

get_group_opt(Host, Group, Opt, Default) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    case lists:keysearch(Opt, 1, Opts) of
		{value, {_, Val}} ->
		    Val;
		false ->
		    Default
	    end;
	_ ->
	    false
    end.

get_group_users(Host, Group) ->
    case get_group_opt(Host, Group, all_users, false) of
	true ->
	    ejabberd_auth:get_vh_registered_users(Host);
	false ->
	    []
    end ++ get_group_explicit_users(Host, Group).

get_group_explicit_users(Host, Group) ->
    case catch mnesia:dirty_index_read(
		 sr_user, {Group, Host}, #sr_user.group_host) of
	Rs when is_list(Rs) ->
	    [R#sr_user.us || R <- Rs];
	_ ->
	    []
    end.

get_group_name(Host, Group) ->
    get_group_opt(Host, Group, name, Group).

get_all_users_groups(Host) ->
    lists:filter(
      fun(Group) -> get_group_opt(Host, Group, all_users, false) end,
      list_groups(Host)).

get_user_displayed_groups(US) ->
    Host = element(2, US),
    DisplayedGroups1 =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    case is_group_enabled(Host, Group) of
			true ->
			    get_group_opt(Host, Group, displayed_groups, []);
			false ->
			    []
		    end
	    end, get_user_groups(US))),
    [Group || Group <- DisplayedGroups1, is_group_enabled(Host, Group)].

is_user_in_group(US, Group, Host) ->
    case catch mnesia:dirty_match_object(
		 #sr_user{us=US, group_host={Group, Host}}) of
	[] -> false;
	_  -> true
    end.

add_user_to_group(Host, US, Group) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

remove_user_from_group(Host, US, Group) ->
    GroupHost = {Group, Host},
    R = #sr_user{us = US, group_host = GroupHost},
    F = fun() ->
		mnesia:delete_object(R)
	end,
    mnesia:transaction(F).


%%---------------------
%% Web Admin
%%---------------------

webadmin_menu(Acc, _Host) ->
    [{"shared-roster", "Shared Roster"} | Acc].

webadmin_page(_, Host,
	      #request{us = US,
		       path = ["shared-roster"],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = list_shared_roster_groups(Host, Query, Lang),
    {stop, Res};

webadmin_page(_, Host,
	      #request{us = US,
		       path = ["shared-roster", Group],
		       q = Query,
		       lang = Lang} = Request) ->
    Res = shared_roster_group(Host, Group, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

list_shared_roster_groups(Host, Query, Lang) ->
    Res = list_sr_groups_parse_query(Host, Query),
    SRGroups = mod_shared_roster:list_groups(Host),
    FGroups =
	?XAE("table", [],
	     [?XE("tbody",
		  lists:map(
		    fun(Group) ->
			    ?XE("tr",
				[?XE("td", [?INPUT("checkbox", "selected",
						   Group)]),
				 ?XE("td", [?AC(Group ++ "/", Group)])
				]
			       )
		    end, lists:sort(SRGroups)) ++
		  [?XE("tr",
		       [?X("td"),
			?XE("td", [?INPUT("text", "namenew", "")]),
			?XE("td", [?INPUTT("submit", "addnew", "Add New")])
		       ]
		      )]
		 )]),
    [?XC("h1", ?T("Shared Roster Groups"))] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [FGroups,
	       ?BR,
	       ?INPUTT("submit", "delete", "Delete Selected")
	      ])
	].

list_sr_groups_parse_query(Host, Query) ->
    case lists:keysearch("addnew", 1, Query) of
	{value, _} ->
	    list_sr_groups_parse_addnew(Host, Query);
	_ ->
	    case lists:keysearch("delete", 1, Query) of
		{value, _} ->
		    list_sr_groups_parse_delete(Host, Query);
		_ ->
		    nothing
	    end
    end.

list_sr_groups_parse_addnew(Host, Query) ->
    case lists:keysearch("namenew", 1, Query) of
	{value, {_, Group}} when Group /= "" ->
	    mod_shared_roster:create_group(Host, Group),
	    ok;
	_ ->
	    error
    end.

list_sr_groups_parse_delete(Host, Query) ->
    SRGroups = mod_shared_roster:list_groups(Host),
    lists:foreach(
      fun(Group) ->
	      case lists:member({"selected", Group}, Query) of
		  true ->
		      mod_shared_roster:delete_group(Host, Group);
		  _ ->
		      ok
	      end
      end, SRGroups),
    ok.


shared_roster_group(Host, Group, Query, Lang) ->
    Res = shared_roster_group_parse_query(Host, Group, Query),
    GroupOpts = mod_shared_roster:get_group_opts(Host, Group),
    Name = get_opt(GroupOpts, name, ""),
    Description = get_opt(GroupOpts, description, ""),
    AllUsers = get_opt(GroupOpts, all_users, false),
    Disabled = false,
    DisplayedGroups = get_opt(GroupOpts, displayed_groups, []),
    Members = mod_shared_roster:get_group_explicit_users(Host, Group),
    FMembers =
	if
	    AllUsers ->
		"@all@\n";
	    true ->
		[]
	end ++ [[us_to_list(Member), $\n] || Member <- Members],
    FDisplayedGroups = [[DG, $\n] || DG <- DisplayedGroups],
    FGroup =
	?XAE("table", [],
	     [?XE("tbody",
		  [?XE("tr",
		       [?XCT("td", "Name:"),
			?XE("td", [?INPUT("text", "name", Name)])
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "Description:"),
			?XE("td", [?XAC("textarea", [{"name", "description"},
						     {"rows", "3"},
						     {"cols", "20"}],
					Description)])
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "Members:"),
			?XE("td", [?XAC("textarea", [{"name", "members"},
						     {"rows", "3"},
						     {"cols", "20"}],
					FMembers)])
		       ]
		      ),
		   ?XE("tr",
		       [?XCT("td", "Displayed Groups:"),
			?XE("td", [?XAC("textarea", [{"name", "dispgroups"},
						     {"rows", "3"},
						     {"cols", "20"}],
					FDisplayedGroups)])
		       ]
		      )]
		 )]),
    [?XC("h1", ?T("Shared Roster Groups"))] ++
	[?XC("h2", ?T("Group ") ++ Group)] ++
	case Res of
	    ok -> [?CT("Submitted"), ?P];
	    error -> [?CT("Bad format"), ?P];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [FGroup,
	       ?BR,
	       ?INPUTT("submit", "submit", "Submit")
	      ])
	].

shared_roster_group_parse_query(Host, Group, Query) ->
    case lists:keysearch("submit", 1, Query) of
	{value, _} ->
	    {value, {_, Name}} = lists:keysearch("name", 1, Query),
	    {value, {_, Description}} = lists:keysearch("description", 1, Query),
	    {value, {_, SMembers}} = lists:keysearch("members", 1, Query),
	    {value, {_, SDispGroups}} = lists:keysearch("dispgroups", 1, Query),
	    NameOpt =
		if
		    Name == "" -> [];
		    true -> [{name, Name}]
		end,
	    DescriptionOpt =
		if
		    Description == "" -> [];
		    true -> [{description, Description}]
		end,
	    DispGroups = string:tokens(SDispGroups, "\r\n"),
	    DispGroupsOpt =
		if
		    DispGroups == [] -> [];
		    true -> [{displayed_groups, DispGroups}]
		end,

	    OldMembers = mod_shared_roster:get_group_explicit_users(
			   Host, Group),
	    SJIDs = string:tokens(SMembers, ", \r\n"),
	    NewMembers =
		lists:foldl(
		  fun(_SJID, error) -> error;
		     (SJID, USs) ->
			  case SJID of
			      "@all@" ->
				  USs;
			      _ ->
				  case jlib:string_to_jid(SJID) of
				      JID when is_record(JID, jid) ->
					  [{JID#jid.luser, JID#jid.lserver} | USs];
				      error ->
					  error
				  end
			  end
		  end, [], SJIDs),
	    AllUsersOpt =
		case lists:member("@all@", SJIDs) of
		    true -> [{all_users, true}];
		    false -> []
		end,

	    mod_shared_roster:set_group_opts(
	      Host, Group,
	      NameOpt ++ DispGroupsOpt ++ DescriptionOpt ++ AllUsersOpt),

	    if
		NewMembers == error -> error;
		true ->
		    AddedMembers = NewMembers -- OldMembers,
		    RemovedMembers = OldMembers -- NewMembers,
		    lists:foreach(
		      fun(US) ->
			      mod_shared_roster:remove_user_from_group(
				Host, US, Group)
		      end, RemovedMembers),
		    lists:foreach(
		      fun(US) ->
			      mod_shared_roster:add_user_to_group(
				Host, US, Group)
		      end, AddedMembers),
		    ok
	    end;
	_ ->
	    nothing
    end.

get_opt(Opts, Opt, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

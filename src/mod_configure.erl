%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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

%%% Implements most of XEP-0133: Service Administration Version 1.1
%%% (2005-08-19)

-module(mod_configure).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 get_local_identity/5,
	 get_local_features/5,
	 get_local_items/5,
	 adhoc_local_items/4,
	 adhoc_local_commands/4,
	 get_sm_identity/5,
	 get_sm_features/5,
	 get_sm_items/5,
	 adhoc_sm_items/4,
	 adhoc_sm_commands/4]).


-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("adhoc.hrl").

-define(T(Lang, Text), translate:translate(Lang, Text)).

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).

-define(NS_ADMIN_s,        "http://jabber.org/protocol/admin").

start(Host, _Opts) ->
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_local_items, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, get_local_identity, 50),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE, adhoc_local_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:add(adhoc_sm_items, Host, ?MODULE, adhoc_sm_items, 50),
    ejabberd_hooks:add(adhoc_sm_commands, Host, ?MODULE, adhoc_sm_commands, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(adhoc_sm_commands, Host, ?MODULE, adhoc_sm_commands, 50),
    ejabberd_hooks:delete(adhoc_sm_items, Host, ?MODULE, adhoc_sm_items, 50),
    ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE, adhoc_local_items, 50),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, get_local_identity, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_local_items, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_ADHOC_s),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ADHOC_s).

%%%-----------------------------------------------------------------------

-define(INFO_IDENTITY(Category, Type, Name, Lang),
	[#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
	  [#xmlattr{name = 'category', value = Category},
	   #xmlattr{name = 'type', value = Type},
	   #xmlattr{name = 'name', value = ?T(Lang, Name)}]}]).

-define(INFO_COMMAND(Name, Lang),
	?INFO_IDENTITY("automation", "command-node", Name, Lang)).

-define(NODEJID(To, Name, Node),
	#xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
	 [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_string(To)},
	  #xmlattr{name = 'name', value = ?T(Lang, Name)},
	  #xmlattr{name = 'node', value = Node}]}).

-define(NODE(Name, Node),
	#xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
	 [#xmlattr{name = 'jid', value = Server},
	  #xmlattr{name = 'name', value = ?T(Lang, Name)},
	  #xmlattr{name = 'node', value = Node}]}).

-define(NS_ADMINX(Sub), ?NS_ADMIN_s++"#"++Sub).
-define(NS_ADMINL(Sub), ["http:","jabber.org","protocol","admin", Sub]).
tokenize(Node) -> string:tokens(Node, "/#").

get_sm_identity(Acc, _From, _To, Node, Lang) ->
    case Node of
	"config" ->
	    ?INFO_COMMAND("Configuration", Lang);
	_ ->
	    Acc
    end.

get_local_identity(Acc, _From, _To, Node, Lang) ->
    LNode = tokenize(Node),
    case LNode of
	["running nodes", ENode] ->
	    ?INFO_IDENTITY("ejabberd", "node", ENode, Lang);
	["running nodes", _ENode, "DB"] ->
	    ?INFO_COMMAND("Database", Lang);
	["running nodes", _ENode, "modules", "start"] ->
	    ?INFO_COMMAND("Start Modules", Lang);
	["running nodes", _ENode, "modules", "stop"] ->
	    ?INFO_COMMAND("Stop Modules", Lang);
	["running nodes", _ENode, "backup", "backup"] ->
	    ?INFO_COMMAND("Backup", Lang);
	["running nodes", _ENode, "backup", "restore"] ->
	    ?INFO_COMMAND("Restore", Lang);
	["running nodes", _ENode, "backup", "textfile"] ->
	    ?INFO_COMMAND("Dump to Text File", Lang);
	["running nodes", _ENode, "import", "file"] ->
	    ?INFO_COMMAND("Import File", Lang);
	["running nodes", _ENode, "import", "dir"] ->
	    ?INFO_COMMAND("Import Directory", Lang);
	["running nodes", _ENode, "restart"] ->
	    ?INFO_COMMAND("Restart Service", Lang);
	["running nodes", _ENode, "shutdown"] ->
	    ?INFO_COMMAND("Shut Down Service", Lang);
	?NS_ADMINL("add-user") ->
	    ?INFO_COMMAND("Add User", Lang);
	?NS_ADMINL("delete-user") ->
	    ?INFO_COMMAND("Delete User", Lang);
	?NS_ADMINL("end-user-session") ->
	    ?INFO_COMMAND("End User Session", Lang);
	?NS_ADMINL("get-user-password") ->
	    ?INFO_COMMAND("Get User Password", Lang);
	?NS_ADMINL("change-user-password") ->
	    ?INFO_COMMAND("Change User Password", Lang);
	?NS_ADMINL("get-user-lastlogin") ->
	    ?INFO_COMMAND("Get User Last Login Time", Lang);
	?NS_ADMINL("user-stats") ->
	    ?INFO_COMMAND("Get User Statistics", Lang);
	?NS_ADMINL("get-registered-users-num") ->
	    ?INFO_COMMAND("Get Number of Registered Users", Lang);
	?NS_ADMINL("get-online-users-num") ->
	    ?INFO_COMMAND("Get Number of Online Users", Lang);
	["config", "acls"] ->
	    ?INFO_COMMAND("Access Control Lists", Lang);
	["config", "access"] ->
	    ?INFO_COMMAND("Access Rules", Lang);
	_ ->
	    Acc
    end.

%%%-----------------------------------------------------------------------

-define(INFO_RESULT(Allow, Feats),
	case Allow of
	    deny ->
		{error, 'forbidden'};
	    allow ->
		{result, Feats}
	end).

get_sm_features(Acc, From, #jid{ldomain = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Allow = acl:match_rule(LServer, configure, From),
	    case Node of
		"config" ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		_ ->
		    Acc
	    end
    end.

get_local_features(Acc, From, #jid{ldomain = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    LNode = tokenize(Node),
	    Allow = acl:match_rule(LServer, configure, From),
	    case LNode of
		["config"] ->
		    ?INFO_RESULT(Allow, []);
		["user"] ->
		    ?INFO_RESULT(Allow, []);
		["online users"] ->
		    ?INFO_RESULT(Allow, []);
		["all users"] ->
		    ?INFO_RESULT(Allow, []);
		["all users", [$@ | _]] ->
		    ?INFO_RESULT(Allow, []);
		["outgoing s2s" | _] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes"] ->
		    ?INFO_RESULT(Allow, []);
		["stopped nodes"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode] ->
		    ?INFO_RESULT(Allow, [?NS_STATS_s]);
		["running nodes", _ENode, "DB"] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["running nodes", _ENode, "modules"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode, "modules", _] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["running nodes", _ENode, "backup"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode, "backup", _] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["running nodes", _ENode, "import"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode, "import", _] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["running nodes", _ENode, "restart"] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["running nodes", _ENode, "shutdown"] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["config", _] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		["http:" | _] ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		_ ->
		    Acc
	    end
    end.

%%%-----------------------------------------------------------------------

adhoc_sm_items(Acc, From, #jid{ldomain = LServer} = To, Lang) ->
    case acl:match_rule(LServer, configure, From) of
	allow ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    Nodes = [#xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
		      [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_string(To)},
		       #xmlattr{name = 'name', value = ?T(Lang, "Configuration")},
		       #xmlattr{name = 'node', value = "config"}]}],
	    {result, Items ++ Nodes};
	_ ->
	    Acc
    end.

%%%-----------------------------------------------------------------------

get_sm_items(Acc, From,
	     #jid{node = User, domain = Server, ldomain = LServer} = To,
	     Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    case {acl:match_rule(LServer, configure, From), Node} of
		{allow, ""} ->
		    Nodes = [?NODEJID(To, "Configuration", "config"),
			     ?NODEJID(To, "User Management", "user")],
		    {result, Items ++ Nodes ++ get_user_resources(User, Server)};
		{allow, "config"} ->
		    {result, []};
		{_, "config"} ->
		    {error, 'forbidden'};
		_ ->
		    Acc
	    end
    end.

get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun(R) ->
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
		       [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_string(User, Server, R)},
			#xmlattr{name = 'name', value = User}]}
	      end, lists:sort(Rs)).

%%%-----------------------------------------------------------------------

adhoc_local_items(Acc, From, #jid{ldomain = LServer, domain = Server} = To,
		  Lang) ->
    case acl:match_rule(LServer, configure, From) of
	allow ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    %% Recursively get all configure commands
	    Nodes = recursively_get_local_items(LServer, "", Server, Lang),
	    Nodes1 = lists:filter(
		       fun(N) ->
			       Nd = exmpp_xml:get_attribute(N, 'node', ""),
			       F = get_local_features([], From, To, Nd, Lang),
			       case F of
				   {result, [?NS_ADHOC_s]} ->
				       true;
				   _ ->
				       false
			       end
		       end, Nodes),
	    {result, Items ++ Nodes1};
	_ ->
	    Acc
    end.

recursively_get_local_items(_LServer, "online users", _Server, _Lang) ->
    [];

recursively_get_local_items(_LServer, "all users", _Server, _Lang) ->
    [];

recursively_get_local_items(LServer, Node, Server, Lang) ->
    LNode = tokenize(Node),
    Items = case get_local_items(LServer, LNode, Server, Lang) of
		{result, Res} ->
		    Res;
		{error, _Error} ->
		    []
	    end,
    Nodes = lists:flatten(
	      lists:map(
		fun(N) ->
			S = exmpp_xml:get_attribute(N, 'jid', ""),
			Nd = exmpp_xml:get_attribute(N, 'node', ""),
			if (S /= Server) or (Nd == "") ->
				[];
			   true ->
				[N, recursively_get_local_items(
				      LServer, Nd, Server, Lang)]
			end
		end, Items)),
    Nodes.

%%%-----------------------------------------------------------------------

-define(ITEMS_RESULT(Allow, LNode, Fallback),
	case Allow of
	    deny ->
		Fallback;
	    allow ->
		case get_local_items(LServer, LNode,
				     exmpp_jid:jid_to_string(To), Lang) of
		    {result, Res} ->
			{result, Res};
		    {error, Error} ->
			{error, Error}
		end
	end).

get_local_items(Acc, From, #jid{ldomain = LServer} = To, "", Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    Allow = acl:match_rule(LServer, configure, From),
	    case Allow of
		deny ->
		    {result, Items};
		allow ->
		    case get_local_items(LServer, [],
					 exmpp_jid:jid_to_string(To), Lang) of
			{result, Res} ->
			    {result, Items ++ Res};
			{error, _Error} ->
			    {result, Items}
		    end
	    end
    end;

get_local_items(Acc, From, #jid{ldomain = LServer} = To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    LNode = tokenize(Node),
	    Allow = acl:match_rule(LServer, configure, From),
	    case LNode of
		["config"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["user"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["online users"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["all users"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["all users", [$@ | _]] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["outgoing s2s" | _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["stopped nodes"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "DB"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "modules"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "modules", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "backup"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "backup", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "import"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "import", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "restart"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["running nodes", _ENode, "shutdown"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		["config", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		?NS_ADMINL(_) ->
		    ?ITEMS_RESULT(Allow, LNode, {error, 'forbidden'});
		_ ->
		    Acc
	    end
    end.

%%%-----------------------------------------------------------------------

get_local_items(_Host, [], Server, Lang) ->
    {result,
     [?NODE("Configuration",            "config"),
      ?NODE("User Management",          "user"),
      ?NODE("Online Users",             "online users"),
      ?NODE("All Users",                "all users"),
      ?NODE("Outgoing s2s Connections", "outgoing s2s"),
      ?NODE("Running Nodes",            "running nodes"),
      ?NODE("Stopped Nodes",            "stopped nodes")
     ]};

get_local_items(_Host, ["config"], Server, Lang) ->
    {result,
     [?NODE("Access Control Lists", "config/acls"),
      ?NODE("Access Rules",         "config/access")
     ]};

get_local_items(_Host, ["config", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["user"], Server, Lang) ->
    {result,
     [?NODE("Add User",            ?NS_ADMINX("add-user")),
      ?NODE("Delete User",         ?NS_ADMINX("delete-user")),
      ?NODE("End User Session",    ?NS_ADMINX("end-user-session")),
      ?NODE("Get User Password",   ?NS_ADMINX("get-user-password")),
      ?NODE("Change User Password",?NS_ADMINX("change-user-password")),
      ?NODE("Get User Last Login Time",   ?NS_ADMINX("get-user-lastlogin")),
      ?NODE("Get User Statistics",   ?NS_ADMINX("user-stats")),
      ?NODE("Get Number of Registered Users",?NS_ADMINX("get-registered-users-num")),
      ?NODE("Get Number of Online Users",?NS_ADMINX("get-online-users-num"))
     ]};

get_local_items(_Host, ["http:" | _], _Server, _Lang) ->
    {result, []};

get_local_items(Host, ["online users"], _Server, _Lang) ->
    {result, get_online_vh_users(Host)};

get_local_items(Host, ["all users"], _Server, _Lang) ->
    {result, get_all_vh_users(Host)};

get_local_items(Host, ["all users", [$@ | Diap]], _Server, _Lang) ->
    case catch ejabberd_auth:get_vh_registered_users(Host) of
	{'EXIT', _Reason} ->
            {error, 'internal-server-error'};
	Users ->
	    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	    case catch begin
			   {ok, [S1, S2]} = regexp:split(Diap, "-"),
			   N1 = list_to_integer(S1),
			   N2 = list_to_integer(S2),
			   Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
			   lists:map(fun({S, U}) ->
					     #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
					      [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_string(U, S)},
					       #xmlattr{name = 'name', value = exmpp_jid:jid_to_string(U, S)}]}
				     end, Sub)
		       end of
			       {'EXIT', _Reason} ->
			 {error, 'not-acceptable'};
		       Res ->
			 {result, Res}
		 end
    end;

get_local_items(Host, ["outgoing s2s"], _Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang)};

get_local_items(Host, ["outgoing s2s", To], _Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang, To)};

get_local_items(_Host, ["running nodes"], Server, Lang) ->
    {result, get_running_nodes(Server, Lang)};

get_local_items(_Host, ["stopped nodes"], _Server, Lang) ->
    {result, get_stopped_nodes(Lang)};

get_local_items(_Host, ["running nodes", ENode], Server, Lang) ->
    {result,
     [?NODE("Database", "running nodes/" ++ ENode ++ "/DB"),
      ?NODE("Modules", "running nodes/" ++ ENode ++ "/modules"),
      ?NODE("Backup Management", "running nodes/" ++ ENode ++ "/backup"),
      ?NODE("Import Users From jabberd 1.4 Spool Files",
	    "running nodes/" ++ ENode ++ "/import"),
      ?NODE("Restart Service", "running nodes/" ++ ENode ++ "/restart"),
      ?NODE("Shut Down Service", "running nodes/" ++ ENode ++ "/shutdown")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "DB"], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", ENode, "modules"], Server, Lang) ->
    {result,
     [?NODE("Start Modules", "running nodes/" ++ ENode ++ "/modules/start"),
      ?NODE("Stop Modules",  "running nodes/" ++ ENode ++ "/modules/stop")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "modules", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", ENode, "backup"], Server, Lang) ->
    {result,
     [?NODE("Backup", "running nodes/" ++ ENode ++ "/backup/backup"),
      ?NODE("Restore", "running nodes/" ++ ENode ++ "/backup/restore"),
      ?NODE("Dump to Text File",
	    "running nodes/" ++ ENode ++ "/backup/textfile")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "backup", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", ENode, "import"], Server, Lang) ->
    {result,
     [?NODE("Import File", "running nodes/" ++ ENode ++ "/import/file"),
      ?NODE("Import Directory",  "running nodes/" ++ ENode ++ "/import/dir")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "import", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", _ENode, "restart"], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", _ENode, "shutdown"], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, _, _Server, _Lang) ->
    {error, 'item-not-found'}.


get_online_vh_users(Host) ->
    case catch ejabberd_sm:get_vh_session_list(Host) of
	{'EXIT', _Reason} ->
	    [];
	USRs ->
	    SURs = lists:sort([{S, U, R} || {U, S, R} <- USRs]),
	    lists:map(fun({S, U, R}) ->
			      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
			       [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_string(U, S, R)},
				#xmlattr{name = 'name', value = exmpp_jid:jid_to_string(U, S)}]}
		      end, SURs)
    end.

get_all_vh_users(Host) ->
    case catch ejabberd_auth:get_vh_registered_users(Host) of
	{'EXIT', _Reason} ->
	    [];
	Users ->
	    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	    case length(SUsers) of
		N when N =< 100 ->
		    lists:map(fun({S, U}) ->
				      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
				       [#xmlattr{name = 'jid', value = exmpp_jid:jid_to_string(U, S)},
					#xmlattr{name = 'name', value = exmpp_jid:jid_to_string(U, S)}]}
			      end, SUsers);
		N ->
		    NParts = trunc(math:sqrt(N * 0.618)) + 1,
		    M = trunc(N / NParts) + 1,
		    lists:map(fun(K) ->
				      L = K + M - 1,
				      Node =
					  "@" ++ integer_to_list(K) ++
					  "-" ++ integer_to_list(L),
				      {FS, FU} = lists:nth(K, SUsers),
				      {LS, LU} =
					  if L < N -> lists:nth(L, SUsers);
					     true -> lists:last(SUsers)
					  end,
				      Name = 
					  FU ++ "@" ++ FS ++
					  " -- " ++
					  LU ++ "@" ++ LS,
				      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
				       [#xmlattr{name = 'jid', value = Host},
					#xmlattr{name = 'node', value = "all users/" ++ Node},
					#xmlattr{name = 'name', value = Name}]}
			      end, lists:seq(1, N, M))
	    end
    end.

get_outgoing_s2s(Host, Lang) ->
    case catch ejabberd_s2s:dirty_get_connections() of
	{'EXIT', _Reason} ->
	    [];
	Connections ->
	    DotHost = "." ++ Host,
	    TConns = [TH || {FH, TH} <- Connections,
			    Host == FH orelse lists:suffix(DotHost, FH)],
	    lists:map(
	      fun(T) ->
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
		       [#xmlattr{name = 'jid', value = Host},
			#xmlattr{name = 'node', value = "outgoing s2s/" ++ T},
			#xmlattr{name = 'name', value =
			 lists:flatten(
			   io_lib:format(
			     ?T(Lang, "To ~s"), [T]))}]}
	      end, lists:usort(TConns))
    end.

get_outgoing_s2s(Host, Lang, To) ->
    case catch ejabberd_s2s:dirty_get_connections() of
	{'EXIT', _Reason} ->
	    [];
	Connections ->
	    lists:map(
	      fun({F, _T}) ->
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
		       [#xmlattr{name = 'jid', value = Host},
			#xmlattr{name = 'node', value = "outgoing s2s/" ++ To ++ "/" ++ F},
			#xmlattr{name = 'name', value =
			 lists:flatten(
			   io_lib:format(
			     ?T(Lang, "From ~s"), [F]))}]}
	      end, lists:keysort(1, lists:filter(fun(E) ->
							 element(2, E) == To
						 end, Connections)))
    end.


get_running_nodes(Server, _Lang) ->
    case catch mnesia:system_info(running_db_nodes) of
	{'EXIT', _Reason} ->
	    [];
	DBNodes ->
	    lists:map(
	      fun(N) ->
		      S = atom_to_list(N),
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
		       [#xmlattr{name = 'jid', value = Server},
			#xmlattr{name = 'node', value = "running nodes/" ++ S},
			#xmlattr{name = 'name', value = S}]}
	      end, lists:sort(DBNodes))
    end.

get_stopped_nodes(_Lang) ->
    case catch (lists:usort(mnesia:system_info(db_nodes) ++
			    mnesia:system_info(extra_db_nodes)) --
		mnesia:system_info(running_db_nodes)) of
	{'EXIT', _Reason} ->
	    [];
	DBNodes ->
	    lists:map(
	      fun(N) ->
		      S = atom_to_list(N),
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
		       [#xmlattr{name = 'jid', value = ?MYNAME},
			#xmlattr{name = 'node', value = "stopped nodes/" ++ S},
			#xmlattr{name = 'name', value = S}]}
	      end, lists:sort(DBNodes))
    end.

%%-------------------------------------------------------------------------

-define(COMMANDS_RESULT(Allow, From, To, Request),
	case Allow of
	    deny ->
		{error, 'forbidden'};
	    allow ->
		adhoc_local_commands(From, To, Request)
	end).

adhoc_local_commands(Acc, From, #jid{ldomain = LServer} = To,
		     #adhoc_request{node = Node} = Request) ->
    LNode = tokenize(Node),
    Allow = acl:match_rule(LServer, configure, From),
    case LNode of
	["running nodes", _ENode, "DB"] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "modules", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "backup", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "import", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "restart"] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "shutdown"] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["config", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	?NS_ADMINL(_) ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	_ ->
	    Acc
    end.

adhoc_local_commands(From, #jid{ldomain = LServer} = _To,
		     #adhoc_request{lang = Lang,
				    node = Node,
				    sessionid = SessionID,
				    action = Action,
				    xdata = XData} = Request) ->
    LNode = tokenize(Node),
    %% If the "action" attribute is not present, it is
    %% understood as "execute".  If there was no <actions/>
    %% element in the first response (which there isn't in our
    %% case), "execute" and "complete" are equivalent.
    ActionIsExecute = lists:member(Action,
				   ["", "execute", "complete"]),
    if	Action == "cancel" ->
	    %% User cancels request
	    adhoc:produce_response(
	      Request, 
	      #adhoc_response{status = canceled});
	XData == false, ActionIsExecute ->
	    %% User requests form
	    case get_form(LServer, LNode, Lang) of
		{result, Form} ->
		    adhoc:produce_response(
		      Request,
		      #adhoc_response{status = executing,
				      elements = Form});
		{result, Status, Form} ->
		    adhoc:produce_response(
		      Request,
		      #adhoc_response{status = Status,
				      elements = Form});
		{error, Error} ->
		    {error, Error}
	    end;
	XData /= false, ActionIsExecute ->
	    %% User returns form.
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    {error, 'bad-request'};
		Fields ->
		    case catch set_form(From, LServer, LNode, Lang, Fields) of
			{result, Res} ->
			    adhoc:produce_response(
			      #adhoc_response{lang = Lang,
			                      node = Node,
					      sessionid = SessionID,
					      elements = Res,
					      status = completed});
			{'EXIT', _} ->
			    {error, 'bad-request'};
			{error, Error} ->
			    {error, Error}
		    end
	    end;
	true ->
	    {error, 'bad-request'}
    end.


-define(TVFIELD(Type, Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'type', value = Type},
			       #xmlattr{name = 'var', value = Var}],
	 children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Val)}]}]}).
-define(HFIELD(), ?TVFIELD("hidden", "FORM_TYPE", ?NS_ADMIN_s)).

-define(TLFIELD(Type, Label, Var),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'type', value = Type},
			       #xmlattr{name = 'label', value = ?T(Lang, Label)},
			       #xmlattr{name = 'var', value = Var}]}).

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'type', value = Type},
			       #xmlattr{name = 'label', value = ?T(Lang, Label)},
			       #xmlattr{name = 'var', value = Var}],
	 children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Val)}]}]}).

-define(XMFIELD(Type, Label, Var, Vals),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'type', value = Type},
			       #xmlattr{name = 'label', value = ?T(Lang, Label)},
			       #xmlattr{name = 'var', value = Var}],
	 children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Val)}]} || Val <- Vals]}).

-define(TABLEFIELD(Table, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'type', value = "list-single"},
			       #xmlattr{name = 'label', value = atom_to_list(Table)},
			       #xmlattr{name = 'var', value = atom_to_list(Table)}],
	 children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(atom_to_list(Val))}]},
	  #xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs = [#xmlattr{name = 'label', value =
				   ?T(Lang, "RAM copy")}],
	   children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"ram_copies">>}]}]},
	  #xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs = [#xmlattr{name = 'label', value =
				   ?T(Lang,
				      "RAM and disc copy")}],
	   children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"disc_copies">>}]}]},
	  #xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs = [#xmlattr{name = 'label', value =
				   ?T(Lang,
				      "Disc only copy")}],
	   children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"disc_only_copies">>}]}]},
	  #xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs = [#xmlattr{name = 'label', value =
				   ?T(Lang, "Remote copy")}],
	   children = [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"unknown">>}]}]}
	 ]}).



get_form(_Host, ["running nodes", ENode, "DB"], Lang) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case rpc:call(Node, mnesia, system_info, [tables]) of
		{badrpc, _Reason} ->
		    {error, 'internal-server-error'};
		Tables ->
		    STables = lists:sort(Tables),
		    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
			       [?HFIELD(),
				#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
			         [#xmlcdata{cdata =
				   list_to_binary(?T(
				      Lang, "Database Tables Configuration at ") ++
				   ENode)}]},
			        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
			         [#xmlcdata{cdata =
				   list_to_binary(?T(
				      Lang, "Choose storage type of tables"))}]} |
			        lists:map(
				  fun(Table) ->
					  case rpc:call(Node,
						        mnesia,
						        table_info,
						        [Table, storage_type]) of
					      {badrpc, _} ->
						  ?TABLEFIELD(Table, unknown);
					      Type ->
						  ?TABLEFIELD(Table, Type)
					  end
				  end, STables)
			       ]}]}
	    end
    end;

get_form(Host, ["running nodes", ENode, "modules", "stop"], Lang) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case rpc:call(Node, gen_mod, loaded_modules, [Host]) of
		{badrpc, _Reason} ->
		    {error, 'internal-server-error'};
		Modules ->
		    SModules = lists:sort(Modules),
		    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
			       [?HFIELD(),
				#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
			         [#xmlcdata{cdata =
				   list_to_binary(?T(
				      Lang, "Stop Modules at ") ++ ENode)}]},
			        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
			         [#xmlcdata{cdata =
				   list_to_binary(?T(
				      Lang, "Choose modules to stop"))}]} |
			        lists:map(fun(M) ->
						  S = atom_to_list(M),
						  ?XFIELD("boolean", S, S, "0")
					  end, SModules)
			       ]}]}
	    end
    end;

get_form(_Host, ["running nodes", ENode, "modules", "start"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Start Modules at ") ++ ENode)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
	         [#xmlcdata{cdata =
	           list_to_binary(?T(
		      Lang, "Enter list of {Module, [Options]}"))}]},
		?XFIELD("text-multi", "List of modules to start", "modules", "[].")
	       ]}]};

get_form(_Host, ["running nodes", ENode, "backup", "backup"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Backup to File at ") ++ ENode)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
	         [#xmlcdata{cdata =
	           list_to_binary(?T(
		      Lang, "Enter path to backup file"))}]},
		?XFIELD("text-single", "Path to File", "path", "")
	       ]}]};

get_form(_Host, ["running nodes", ENode, "backup", "restore"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Restore Backup from File at ") ++ ENode)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
	         [#xmlcdata{cdata =
	           list_to_binary(?T(
		      Lang, "Enter path to backup file"))}]},
		?XFIELD("text-single", "Path to File", "path", "")
	       ]}]};

get_form(_Host, ["running nodes", ENode, "backup", "textfile"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Dump Backup to Text File at ") ++ ENode)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
	         [#xmlcdata{cdata =
	           list_to_binary(?T(
		      Lang, "Enter path to text file"))}]},
		?XFIELD("text-single", "Path to File", "path", "")
	       ]}]};

get_form(_Host, ["running nodes", ENode, "import", "file"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Import User from File at ") ++ ENode)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
	         [#xmlcdata{cdata =
	           list_to_binary(?T(
		      Lang, "Enter path to jabberd1.4 spool file"))}]},
		?XFIELD("text-single", "Path to File", "path", "")
	       ]}]};

get_form(_Host, ["running nodes", ENode, "import", "dir"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Import Users from Dir at ") ++ ENode)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'instructions', children =
	         [#xmlcdata{cdata =
	           list_to_binary(?T(
		      Lang, "Enter path to jabberd1.4 spool dir"))}]},
		?XFIELD("text-single", "Path to Dir", "path", "")
	       ]}]};

get_form(_Host, ["running nodes", _ENode, "restart"], Lang) ->
    Make_option = 
	fun(LabelNum, LabelUnit, Value)->
		#xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs =
		 [#xmlattr{name = 'label', value = LabelNum ++ ?T(Lang, LabelUnit)}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Value)}]}]}
	end,
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Restart Service"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "list-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Time delay")},
		  #xmlattr{name = 'var', value = "delay"}], children =
		 [Make_option("", "immediately", "1"),
		  Make_option("15 ", "seconds", "15"),
		  Make_option("30 ", "seconds", "30"),
		  Make_option("60 ", "seconds", "60"),
		  Make_option("90 ", "seconds", "90"),
		  Make_option("2 ", "minutes", "120"),
		  Make_option("3 ", "minutes", "180"),
		  Make_option("4 ", "minutes", "240"),
		  Make_option("5 ", "minutes", "300"),
		  Make_option("10 ", "minutes", "600"),
		  Make_option("15 ", "minutes", "900"),
		  Make_option("30 ", "minutes", "1800"),
		  #xmlel{ns = ?NS_DATA_FORMS, name = 'required'}
		 ]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "fixed"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Send announcement to all online users on all hosts")}]},
		#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'var', value = "subject"},
		  #xmlattr{name = 'type', value = "text-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Subject")}]},
		#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'var', value = "announcement"},
		  #xmlattr{name = 'type', value = "text-multi"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Message body")}]}
	       ]}]};

get_form(_Host, ["running nodes", _ENode, "shutdown"], Lang) ->
    Make_option = 
	fun(LabelNum, LabelUnit, Value)->
		#xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs =
		 [#xmlattr{name = 'label', value = LabelNum ++ ?T(Lang, LabelUnit)}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Value)}]}]}
	end,
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Shut Down Service"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "list-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Time delay")},
		  #xmlattr{name = 'var', value = "delay"}], children =
		 [Make_option("", "immediately", "1"),
		  Make_option("15 ", "seconds", "15"),
		  Make_option("30 ", "seconds", "30"),
		  Make_option("60 ", "seconds", "60"),
		  Make_option("90 ", "seconds", "90"),
		  Make_option("2 ", "minutes", "120"),
		  Make_option("3 ", "minutes", "180"),
		  Make_option("4 ", "minutes", "240"),
		  Make_option("5 ", "minutes", "300"),
		  Make_option("10 ", "minutes", "600"),
		  Make_option("15 ", "minutes", "900"),
		  Make_option("30 ", "minutes", "1800"),
		  #xmlel{ns = ?NS_DATA_FORMS, name = 'required'}
		 ]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "fixed"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Send announcement to all online users on all hosts")}]},
		#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'var', value = "subject"},
		  #xmlattr{name = 'type', value = "text-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Subject")}]},
		#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'var', value = "announcement"},
		  #xmlattr{name = 'type', value = "text-multi"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Message body")}]}
	       ]}]};

get_form(Host, ["config", "acls"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Access Control List Configuration"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'type', value = "text-multi"},
				       #xmlattr{name = 'label', value =
				        ?T(
					   Lang, "Access control lists")},
				       #xmlattr{name = 'var', value = "acls"}],
	         children = lists:map(fun(S) ->
				   #xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(S)}]}
			   end,
			   string:tokens(
			     lists:flatten(
			       io_lib:format(
				 "~p.",
				 [ets:select(acl,
					     [{{acl, {'$1', '$2'}, '$3'},
					       [{'==', '$2', Host}],
					       [{{acl, '$1', '$3'}}]}])
			         ])),
			     "\n"))
	        }
	       ]}]};

get_form(Host, ["config", "access"], Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Access Configuration"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =[#xmlattr{name = 'type', value = "text-multi"},
				       #xmlattr{name = 'label', value =
				        ?T(
					   Lang, "Access rules")},
				       #xmlattr{name = 'var', value = "access"}],
	         children = lists:map(fun(S) ->
				   #xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =[#xmlcdata{cdata = list_to_binary(S)}]}
			   end,
			   string:tokens(
			     lists:flatten(
			       io_lib:format(
			         "~p.",
			         [ets:select(config,
					     [{{config, {access, '$1', '$2'}, '$3'},
					       [{'==', '$2', Host}],
					       [{{access, '$1', '$3'}}]}])
			         ])),
			     "\n"))
	        }
	       ]}]};

get_form(_Host, ?NS_ADMINL("add-user"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Add User"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Jabber ID")},
		  #xmlattr{name = 'var', value = "accountjid"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "text-private"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Password")},
		  #xmlattr{name = 'var', value = "password"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "text-private"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Password Verification")},
		  #xmlattr{name = 'var', value = "password-verify"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("delete-user"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Delete User"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-multi"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Jabber ID")},
		  #xmlattr{name = 'var', value = "accountjids"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("end-user-session"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "End User Session"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-single"},
		  #xmlattr{name = 'label', value = list_to_binary(?T(Lang, "Jabber ID"))},
		  #xmlattr{name = 'var', value = "accountjid"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("get-user-password"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Get User Password"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Jabber ID")},
		  #xmlattr{name = 'var', value = "accountjid"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("change-user-password"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Get User Password"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Jabber ID")},
		  #xmlattr{name = 'var', value = "accountjid"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "text-private"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Password")},
		  #xmlattr{name = 'var', value = "password"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("get-user-lastlogin"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Get User Last Login Time"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name =  'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Jabber ID")},
		  #xmlattr{name = 'var', value = "accountjid"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("user-stats"), Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
		 [#xmlcdata{cdata = list_to_binary(?T(Lang, "Get User Statistics"))}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
		 [#xmlattr{name = 'type', value = "jid-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Jabber ID")},
		  #xmlattr{name = 'var', value = "accountjid"}], children =
		 [#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]}
	       ]}]};

get_form(Host, ?NS_ADMINL("get-registered-users-num"), Lang) ->
    [Num] = io_lib:format("~p", [ejabberd_auth:get_vh_registered_users_number(Host)]),
    {result, completed,
     [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
       [?HFIELD(),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	 [#xmlattr{name = 'type', value = "jid-single"},
	  #xmlattr{name = 'label', value = ?T(Lang, "Number of registered users")},
	  #xmlattr{name = 'var', value = "registeredusersnum"}], children =
	 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Num)}]}]
	}]}]};

get_form(Host, ?NS_ADMINL("get-online-users-num"), Lang) ->
    Num = io_lib:format("~p", [length(ejabberd_sm:get_vh_session_list(Host))]),
    {result, completed,
     [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
       [?HFIELD(),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	 [#xmlattr{name = 'type', value = "jid-single"},
	  #xmlattr{name = 'label', value = ?T(Lang, "Number of online users")},
	  #xmlattr{name = 'var', value = "onlineusersnum"}], children =
	 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Num)}]}]
	}]}]};

get_form(_Host, _, _Lang) ->
    {error, 'service-unavailable'}.



set_form(_From, _Host, ["running nodes", ENode, "DB"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    lists:foreach(
	      fun({SVar, SVals}) ->
		      %% We believe that this is allowed only for good people
		      Table = list_to_atom(SVar),
		      Type = case SVals of
				 ["unknown"] -> unknown;
				 ["ram_copies"] -> ram_copies;
				 ["disc_copies"] -> disc_copies;
				 ["disc_only_copies"] -> disc_only_copies;
				 _ -> false
			     end,
		      if
			  Type == false ->
			      ok;
			  Type == unknown ->
			      mnesia:del_table_copy(Table, Node);
			  true ->
			      case mnesia:add_table_copy(Table, Node, Type) of
				  {aborted, _} ->
				      mnesia:change_table_copy_type(
					Table, Node, Type);
				  _ ->
				      ok
			      end
		      end
	      end, XData),
	    {result, []}
    end;

set_form(_From, Host, ["running nodes", ENode, "modules", "stop"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    lists:foreach(
	      fun({Var, Vals}) ->
		      case Vals of
			  ["1"] ->
			      Module = list_to_atom(Var),
			      rpc:call(Node, gen_mod, stop_module, [Host, Module]);
			  _ ->
			      ok
		      end
	      end, XData),
	    {result, []}
    end;

set_form(_From, Host, ["running nodes", ENode, "modules", "start"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case lists:keysearch("modules", 1, XData) of
		false ->
		    {error, 'bad-request'};
		{value, {_, Strings}} ->
		    String = lists:foldl(fun(S, Res) ->
						 Res ++ S ++ "\n"
					 end, "", Strings),
		    case erl_scan:string(String) of
			{ok, Tokens, _} ->
			    case erl_parse:parse_term(Tokens) of
				{ok, Modules} ->
				    lists:foreach(
				      fun({Module, Args}) ->
					      rpc:call(Node,
						       gen_mod,
						       start_module,
						       [Host, Module, Args])
				      end, Modules),
				    {result, []};
				_ ->
				    {error, 'bad-request'}
			    end;
			_ ->
			    {error, 'bad-request'}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end
    end;


set_form(_From, _Host, ["running nodes", ENode, "backup", "backup"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, 'bad-request'};
		{value, {_, [String]}} ->
		    case rpc:call(Node, mnesia, backup, [String]) of
			{badrpc, _Reason} ->
			    {error, 'internal-server-error'};
			{error, _Reason} ->
			    {error, 'internal-server-error'};
			_ ->
			    {result, []}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end
    end;


set_form(_From, _Host, ["running nodes", ENode, "backup", "restore"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, 'bad-request'};
		{value, {_, [String]}} ->
                    case rpc:call(Node, ejabberd_admin, restore, [String]) of
			{badrpc, _Reason} ->
			    {error, 'internal-server-error'};
			{error, _Reason} ->
			    {error, 'internal-server-error'};
			_ ->
			    {result, []}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end
    end;


set_form(_From, _Host, ["running nodes", ENode, "backup", "textfile"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, 'bad-request'};
		{value, {_, [String]}} ->
		    case rpc:call(Node, ejabberd_ctl, dump_to_textfile, [String]) of
			{badrpc, _Reason} ->
			    {error, 'internal-server-error'};
			{error, _Reason} ->
			    {error, 'internal-server-error'};
			_ ->
			    {result, []}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end
    end;


set_form(_From, _Host, ["running nodes", ENode, "import", "file"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, 'bad-request'};
		{value, {_, [String]}} ->
		    rpc:call(Node, jd2ejd, import_file, [String]),
		    {result, []};
		_ ->
		    {error, 'bad-request'}
	    end
    end;


set_form(_From, _Host, ["running nodes", ENode, "import", "dir"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, 'item-not-found'};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, 'bad-request'};
		{value, {_, [String]}} ->
		    rpc:call(Node, jd2ejd, import_dir, [String]),
		    {result, []};
		_ ->
		    {error, 'bad-request'}
	    end
    end;

set_form(From, Host, ["running nodes", ENode, "restart"], _Lang, XData) ->
    stop_node(From, Host, ENode, restart, XData);

set_form(From, Host, ["running nodes", ENode, "shutdown"], _Lang, XData) ->
    stop_node(From, Host, ENode, stop, XData);

set_form(_From, Host, ["config", "acls"], _Lang, XData) ->
    case lists:keysearch("acls", 1, XData) of
	{value, {_, Strings}} ->
	    String = lists:foldl(fun(S, Res) ->
					 Res ++ S ++ "\n"
				 end, "", Strings),
	    case erl_scan:string(String) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, ACLs} ->
			    case acl:add_list(Host, ACLs, true) of
				ok ->
				    {result, []};
				_ ->
				    {error, 'bad-request'}
			    end;
			_ ->
			    {error, 'bad-request'}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end;

set_form(_From, Host, ["config", "access"], _Lang, XData) ->
    SetAccess =
	fun(Rs) ->
		mnesia:transaction(
		  fun() ->
			  Os = mnesia:select(config,
					     [{{config, {access, '$1', '$2'}, '$3'},
					       [{'==', '$2', Host}],
					       ['$_']}]),
			  lists:foreach(fun(O) ->
						mnesia:delete_object(O)
					end, Os),
			  lists:foreach(
			    fun({access, Name, Rules}) ->
				    mnesia:write({config,
						  {access, Name, Host},
						  Rules})
			    end, Rs)
		  end)
	end,
    case lists:keysearch("access", 1, XData) of
	{value, {_, Strings}} ->
	    String = lists:foldl(fun(S, Res) ->
					 Res ++ S ++ "\n"
				 end, "", Strings),
	    case erl_scan:string(String) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, Rs} ->
			    case SetAccess(Rs) of
				{atomic, _} ->
				    {result, []};
				_ ->
				    {error, 'bad-request'}
			    end;
			_ ->
			    {error, 'bad-request'}
		    end;
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end;

set_form(_From, _Host, ?NS_ADMINL("add-user"), _Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    Password = get_value("password", XData),
    Password = get_value("password-verify", XData),
    AccountJID = exmpp_jid:string_to_jid(AccountString),
    User = AccountJID#jid.lnode,
    Server = AccountJID#jid.ldomain,
    true = lists:member(Server, ?MYHOSTS),
    ejabberd_auth:try_register(User, Server, Password),
    {result, []};

set_form(_From, _Host, ?NS_ADMINL("delete-user"), _Lang, XData) ->
    AccountStringList = get_values("accountjids", XData),
    [_|_] = AccountStringList,
    ASL2 = lists:map(
	     fun(AccountString) ->
		     JID = exmpp_jid:string_to_jid(AccountString),
		     [_|_] = JID#jid.lnode,
		     User = JID#jid.lnode, 
		     Server = JID#jid.ldomain, 
		     true = ejabberd_auth:is_user_exists(User, Server),
		     {User, Server}
	     end,
	     AccountStringList),
    [ejabberd_auth:remove_user(User, Server) || {User, Server} <- ASL2],
    {result, []};

set_form(_From, _Host, ?NS_ADMINL("end-user-session"), _Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = exmpp_jid:string_to_jid(AccountString),
    [_|_] = JID#jid.lnode,
    LUser = JID#jid.lnode, 
    LServer = JID#jid.ldomain, 
    %% Code copied from ejabberd_sm.erl
    case JID#jid.lresource of
	undefined -> 
	    SIDs = mnesia:dirty_select(session,
				       [{#session{sid = '$1', usr = {LUser, LServer, '_'}, _ = '_'}, [], ['$1']}]),
	    [Pid ! replaced || {_, Pid} <- SIDs];
	R -> 
	    [{_, Pid}] = mnesia:dirty_select(session,
					     [{#session{sid = '$1', usr = {LUser, LServer, R}, _ = '_'}, [], ['$1']}]),
	    Pid ! replaced
    end, 
    {result, []};

set_form(_From, _Host, ?NS_ADMINL("get-user-password"), Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = exmpp_jid:string_to_jid(AccountString),
    [_|_] = JID#jid.lnode,
    User = JID#jid.lnode, 
    Server = JID#jid.ldomain, 
    Password = ejabberd_auth:get_password(User, Server),
    true = is_list(Password),
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		?XFIELD("jid-single", "Jabber ID", "accountjid", AccountString),
	        ?XFIELD("text-single", "Password", "password", Password)
	       ]}]};

set_form(_From, _Host, ?NS_ADMINL("change-user-password"), _Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    Password = get_value("password", XData),
    JID = exmpp_jid:string_to_jid(AccountString),
    [_|_] = JID#jid.lnode,
    User = JID#jid.lnode, 
    Server = JID#jid.ldomain, 
    true = ejabberd_auth:is_user_exists(User, Server),
    ejabberd_auth:set_password(User, Server, Password),
    {result, []};

set_form(_From, _Host, ?NS_ADMINL("get-user-lastlogin"), Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = exmpp_jid:string_to_jid(AccountString),
    [_|_] = JID#jid.lnode,
    User = JID#jid.lnode, 
    Server = JID#jid.ldomain, 

    %% Code copied from web/ejabberd_web_admin.erl
    %% TODO: Update time format to XEP-0202: Entity Time
    FLast =
	case ejabberd_sm:get_user_resources(User, Server) of
	    [] ->
		_US = {User, Server},
		case get_last_info(User, Server) of
		    not_found ->
			?T(Lang, "Never");
		    {ok, Timestamp, _Status} ->
			Shift = Timestamp,
			TimeStamp = {Shift div 1000000,
				     Shift rem 1000000,
				     0},
			{{Year, Month, Day}, {Hour, Minute, Second}} =
			    calendar:now_to_local_time(TimeStamp),
			lists:flatten(
			  io_lib:format(
			    "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
			    [Year, Month, Day, Hour, Minute, Second]))
		end;
	    _ ->
		?T(Lang, "Online")
	end,
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs = [#xmlattr{name = 'type', value = "result"}], children =
	       [?HFIELD(),
		?XFIELD("jid-single", "Jabber ID", "accountjid", AccountString),
	        ?XFIELD("text-single", "Last login", "lastlogin", FLast)
	       ]}]};

set_form(_From, _Host, ?NS_ADMINL("user-stats"), Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = exmpp_jid:string_to_jid(AccountString),
    [_|_] = JID#jid.lnode,
    User = JID#jid.lnode, 
    Server = JID#jid.ldomain, 

    Resources = ejabberd_sm:get_user_resources(User, Server),
    IPs1 = [ejabberd_sm:get_user_ip(User, Server, Resource) || Resource <- Resources],
    IPs = [inet_parse:ntoa(IP)++":"++integer_to_list(Port) || {IP, Port} <- IPs1],

    Items = ejabberd_hooks:run_fold(roster_get, Server, [], [{User, Server}]),
    Rostersize = integer_to_list(erlang:length(Items)),

    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		?XFIELD("jid-single", "Jabber ID", "accountjid", AccountString),
	        ?XFIELD("text-single", "Roster size", "rostersize", Rostersize),
	        ?XMFIELD("text-multi", "IP addresses", "ipaddresses", IPs),
	        ?XMFIELD("text-multi", "Resources", "onlineresources", Resources)
	       ]}]};

set_form(_From, _Host, _, _Lang, _XData) ->
    {error, 'service-unavailable'}.

get_value(Field, XData) -> 
    hd(get_values(Field, XData)).
get_values(Field, XData) -> 
    {value, {_, ValueList}} = lists:keysearch(Field, 1, XData),
    ValueList.


search_running_node(SNode) ->
    search_running_node(SNode, mnesia:system_info(running_db_nodes)).

search_running_node(_, []) ->
    false;
search_running_node(SNode, [Node | Nodes]) ->
    case atom_to_list(Node) of
	SNode ->
	    Node;
	_ ->
	    search_running_node(SNode, Nodes)
    end.

stop_node(From, Host, ENode, Action, XData) ->
    Delay = list_to_integer(get_value("delay", XData)),
    Subject = case get_value("subject", XData) of
		  [] -> [];
		  S -> [#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'var', value = "subject"}], children =
			 [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(S)}]}]}]
	      end,
    Announcement = case get_values("announcement", XData) of
		       [] -> [];
		       As -> [#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [#xmlattr{name = 'var', value = "body"}], children =
			       [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = list_to_binary(Line)}]} || Line <- As] }]
		   end,
    case Subject ++ Announcement of
	[] -> ok;
	SubEls ->
	    Request = #adhoc_request{
	      node = ?NS_ADMINX("announce-allhosts"),
	      action = "complete",
	      xdata = #xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
		       [#xmlattr{name = 'type', value = "submit"}], children =
		       SubEls},
	      others= [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
			[#xmlattr{name = 'type', value = "submit"}], children =
			SubEls}]
	     },
	    To = exmpp_jid:make_jid(Host),
	    mod_announce:announce_commands(empty, From, To, Request)
    end,
    Time = timer:seconds(Delay),
    Node = list_to_atom(ENode),
    {ok, _} = timer:apply_after(Time, rpc, call, [Node, init, Action, []]),
    {result, []}.


get_last_info(User, Server) ->
    ML = lists:member(mod_last, gen_mod:loaded_modules(Server)),
    MLO = lists:member(mod_last_odbc, gen_mod:loaded_modules(Server)),
    case {ML, MLO} of
	{true, _} -> mod_last:get_last_info(User, Server);
	{false, true} -> mod_last_odbc:get_last_info(User, Server);
	{false, false} -> not_found
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adhoc_sm_commands(_Acc, From,
		  #jid{node = User, domain = Server, ldomain = LServer} = _To,
		  #adhoc_request{lang = Lang,
				 node = "config",
				 action = Action,
				 xdata = XData} = Request) ->
    case acl:match_rule(LServer, configure, From) of
	deny ->
	    {error, 'forbidden'};
	allow ->
	    %% If the "action" attribute is not present, it is
	    %% understood as "execute".  If there was no <actions/>
	    %% element in the first response (which there isn't in our
	    %% case), "execute" and "complete" are equivalent.
	    ActionIsExecute = lists:member(Action,
					   ["", "execute", "complete"]),
	    if	Action == "cancel" ->
		    %% User cancels request
		    adhoc:produce_response(
		      Request, 
		      #adhoc_response{status = canceled});
		XData == false, ActionIsExecute ->
		    %% User requests form
		    case get_sm_form(User, Server, "config", Lang) of
			{result, Form} ->
			    adhoc:produce_response(
			      Request,
			      #adhoc_response{status = executing,
					      elements = Form});
			{error, Error} ->
			    {error, Error}
		    end;
		XData /= false, ActionIsExecute ->
		    %% User returns form.
		    case jlib:parse_xdata_submit(XData) of
			invalid ->
			    {error, 'bad-request'};
			Fields ->
			    set_sm_form(User, Server, "config", Request, Fields)
		    end;
		true ->
		    {error, 'bad-request'}
	    end
    end;

adhoc_sm_commands(Acc, _From, _To, _Request) ->
    Acc.

get_sm_form(User, Server, "config", Lang) ->
    {result, [#xmlel{ns = ?NS_DATA_FORMS, name = 'x', children =
	       [?HFIELD(),
		#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	         [#xmlcdata{cdata =
		   list_to_binary(?T(
		      Lang, "Administration of ") ++ User)}]},
	        #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	         [#xmlattr{name = 'type', value = "list-single"},
		  #xmlattr{name = 'label', value = ?T(Lang, "Action on user")},
		  #xmlattr{name = 'var', value = "action"}], children =
	         [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"edit">>}]},
		  #xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs =
		   [#xmlattr{name = 'label', value = ?T(Lang, "Edit Properties")}], children =
		   [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"edit">>}]}]},
		  #xmlel{ns = ?NS_DATA_FORMS, name = 'option', attrs =
		   [#xmlattr{name = 'label', value = ?T(Lang, "Remove User")}], children =
		   [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = <<"remove">>}]}]}
	         ]},
	        ?XFIELD("text-private", "Password", "password",
		        ejabberd_auth:get_password_s(User, Server))
	       ]}]};

get_sm_form(_User, _Server, _Node, _Lang) ->
    {error, 'service-unavailable'}.


set_sm_form(User, Server, "config",
	    #adhoc_request{lang = Lang,
	                   node = Node,
			   sessionid = SessionID}, XData) ->
    Response = #adhoc_response{lang = Lang,
                               node = Node,
			       sessionid = SessionID,
			       status = completed},
    case lists:keysearch("action", 1, XData) of
	{value, {_, ["edit"]}} ->
	    case lists:keysearch("password", 1, XData) of
		{value, {_, [Password]}} ->
		    ejabberd_auth:set_password(User, Server, Password),
		    adhoc:produce_response(Response);
		_ ->
		    {error, 'not-acceptable'}
	    end;
	{value, {_, ["remove"]}} ->
	    catch ejabberd_auth:remove_user(User, Server),
	    adhoc:produce_response(Response);
	_ ->
	    {error, 'not-acceptable'}
    end;

set_sm_form(_User, _Server, _Node, _Request, _Fields) ->
    {error, 'service-unavailable'}.




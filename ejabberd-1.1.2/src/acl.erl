%%%----------------------------------------------------------------------
%%% File    : acl.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : ACL support
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(acl).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 to_record/3,
	 add/3,
	 add_list/3,
	 match_rule/3,
	 % for debugging only
	 match_acl/3]).

-include("ejabberd.hrl").

-record(acl, {aclname, aclspec}).

start() ->
    mnesia:create_table(acl,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, acl)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    ok.

to_record(Host, ACLName, ACLSpec) ->
    #acl{aclname = {ACLName, Host}, aclspec = ACLSpec}.

add(Host, ACLName, ACLSpec) ->
    F = fun() ->
		mnesia:write(#acl{aclname = {ACLName, Host},
				  aclspec = ACLSpec})
	end,
    mnesia:transaction(F).

add_list(Host, ACLs, Clear) ->
    F = fun() ->
		if
		    Clear ->
			Ks = mnesia:select(
			       acl, [{{acl, {'$1', Host}, '$2'}, [], ['$1']}]),
			lists:foreach(fun(K) ->
					      mnesia:delete({acl, {K, Host}})
				      end, Ks);
		    true ->
			ok
		end,
		lists:foreach(fun(ACL) ->
				      case ACL of
					  #acl{aclname = ACLName,
					       aclspec = ACLSpec} ->
					      mnesia:write(
						#acl{aclname = {ACLName, Host},
						     aclspec = ACLSpec})
				      end
			      end, ACLs)
	end,
    case mnesia:transaction(F) of
	{atomic, _} ->
	    ok;
	_ ->
	    false
    end.



match_rule(global, Rule, JID) ->
    case Rule of
	all -> allow;
	none -> deny;
	_ ->
	    case ejabberd_config:get_global_option({access, Rule, global}) of
		undefined ->
		    deny;
		GACLs ->
		    match_acls(GACLs, JID, global)
	    end
    end;

match_rule(Host, Rule, JID) ->
    case Rule of
	all -> allow;
	none -> deny;
	_ ->
	    case ejabberd_config:get_global_option({access, Rule, global}) of
		undefined ->
		    case ejabberd_config:get_global_option({access, Rule, Host}) of
			undefined ->
			    deny;
			ACLs ->
			    match_acls(ACLs, JID, Host)
		    end;
		GACLs ->
		    case ejabberd_config:get_global_option({access, Rule, Host}) of
			undefined ->
			    match_acls(GACLs, JID, Host);
			ACLs ->
			    case lists:reverse(GACLs) of
				[{allow, all} | Rest] ->
				    match_acls(
				      lists:reverse(Rest) ++ ACLs ++
				      [{allow, all}],
				      JID, Host);
				_ ->
				    match_acls(GACLs ++ ACLs, JID, Host)
			    end
		    end
	    end
    end.

match_acls([], _, Host) ->
    deny;
match_acls([{Access, ACL} | ACLs], JID, Host) ->
    case match_acl(ACL, JID, Host) of
	true ->
	    Access;
	_ ->
	    match_acls(ACLs, JID, Host)
    end.

match_acl(ACL, JID, Host) ->
    case ACL of
	all -> true;
	none -> false;
	_ ->
	    {User, Server, Resource} = jlib:jid_tolower(JID),
	    lists:any(fun(#acl{aclspec = Spec}) ->
			      case Spec of
				  all ->
				      true;
				  {user, U} ->
				      (U == User)
					  andalso
					    ((Host == Server) orelse
					     ((Host == global) andalso
					      lists:member(Server, ?MYHOSTS)));
				  {user, U, S} ->
				      (U == User) andalso (S == Server);
				  {server, S} ->
				      S == Server;
				  {user_regexp, UR} ->
				      ((Host == Server) orelse
				       ((Host == global) andalso
					lists:member(Server, ?MYHOSTS)))
					  andalso is_regexp_match(User, UR);
				  {user_regexp, UR, S} ->
				      (S == Server) andalso
					  is_regexp_match(User, UR);
				  {server_regexp, SR} ->
				      is_regexp_match(Server, SR);
				  {node_regexp, UR, SR} ->
				      is_regexp_match(Server, SR) andalso
					  is_regexp_match(User, UR);
				  {user_glob, UR} ->
				      ((Host == Server) orelse
				       ((Host == global) andalso
					lists:member(Server, ?MYHOSTS)))
					  andalso
					  is_glob_match(User, UR);
				  {user_glob, UR, S} ->
				      (S == Server) andalso
					  is_glob_match(User, UR);
				  {server_glob, SR} ->
				      is_glob_match(Server, SR);
				  {node_glob, UR, SR} ->
				      is_glob_match(Server, SR) andalso
					  is_glob_match(User, UR);
				  WrongSpec ->
				      ?ERROR_MSG(
					 "Wrong ACL expression: ~p~n"
					 "Check your config file and reload it with the override_acls option enabled",
					 [WrongSpec]),
				      false
			      end
		      end,
		      ets:lookup(acl, {ACL, global}) ++
		      ets:lookup(acl, {ACL, Host}))
    end.

is_regexp_match(String, RegExp) ->
    case regexp:first_match(String, RegExp) of
	nomatch ->
	    false;
	{match, _, _} ->
	    true;
	{error, ErrDesc} ->
	    ?ERROR_MSG(
	       "Wrong regexp ~p in ACL: ~p",
	       [RegExp, lists:flatten(regexp:format_error(ErrDesc))]),
	    false
    end.

is_glob_match(String, Glob) ->
    is_regexp_match(String, regexp:sh_to_awk(Glob)).



%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_register).
-author('alexey@sevcom.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 stream_feature_register/1,
	 unauthenticated_iq_register/3,
	 process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, Host,
 		       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
 		       ?MODULE, unauthenticated_iq_register, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_stream_features, Host,
 			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_REGISTER).


stream_feature_register(Acc) ->
    [{xmlelement, "register",
      [{"xmlns", ?NS_FEATURE_IQREGISTER}], []} | Acc].

unauthenticated_iq_register(_Acc, Server, #iq{xmlns = ?NS_REGISTER} = IQ) ->
    ResIQ = process_iq(jlib:make_jid("", "", ""),
 		       jlib:make_jid("", Server, ""),
 		       IQ),
    Res1 = jlib:replace_from_to(jlib:make_jid("", Server, ""),
 				jlib:make_jid("", "", ""),
 				jlib:iq_to_xml(ResIQ)),
    jlib:remove_attr("to", Res1);

unauthenticated_iq_register(Acc, _Server, _IQ) ->
    Acc.

process_iq(From, To,
	   #iq{type = Type, lang = Lang, sub_el = SubEl, id = ID} = IQ) ->
    case Type of
	set ->
	    UTag = xml:get_subtag(SubEl, "username"),
	    PTag = xml:get_subtag(SubEl, "password"),
	    RTag = xml:get_subtag(SubEl, "remove"),
	    Server = To#jid.lserver,
	    if
		(UTag /= false) and (RTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:remove_user(User, Server),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    if
				PTag /= false ->
				    Password = xml:get_tag_cdata(PTag),
				    case ejabberd_auth:remove_user(User,
								   Server,
								   Password) of
					ok ->
					    IQ#iq{type = result,
						  sub_el = [SubEl]};
					%% TODO FIXME: This piece of
					%% code does not work since
					%% the code have been changed
					%% to allow several auth
					%% modules.  lists:foreach can
					%% only return ok:
					not_allowed ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl, ?ERR_NOT_ALLOWED]};
					not_exists ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl, ?ERR_ITEM_NOT_FOUND]};
					_ ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl,
						   ?ERR_INTERNAL_SERVER_ERROR]}
				    end;
				true ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_BAD_REQUEST]}
			    end
		    end;
		(UTag == false) and (RTag /= false) ->
		    case From of
			#jid{user = User,
			     lserver = Server,
			     resource = Resource} ->
			    ResIQ = #iq{type = result, xmlns = ?NS_REGISTER,
					id = ID,
					sub_el = [SubEl]},
			    ejabberd_router:route(
			      jlib:make_jid(User, Server, Resource),
			      jlib:make_jid(User, Server, Resource),
			      jlib:iq_to_xml(ResIQ)),
			    ejabberd_auth:remove_user(User, Server),
			    ignore;
			_ ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		    end;
		(UTag /= false) and (PTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    Password = xml:get_tag_cdata(PTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:set_password(User, Server, Password),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    case try_register(User, Server, Password) of
				ok ->
				    IQ#iq{type = result, sub_el = [SubEl]};
				{error, Error} ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, Error]}
			    end
		    end;
		true ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	    end;
	get ->
	    IQ#iq{type = result,
		  sub_el = [{xmlelement,
			     "query",
			     [{"xmlns", "jabber:iq:register"}],
			     [{xmlelement, "instructions", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Choose a username and password "
				   "to register with this server")}]},
			      {xmlelement, "username", [], []},
			      {xmlelement, "password", [], []}]}]}
    end.


try_register(User, Server, Password) ->
    case jlib:is_nodename(User) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    JID = jlib:make_jid(User, Server, ""),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    case acl:match_rule(Server, Access, JID) of
		deny ->
		    {error, ?ERR_CONFLICT};
		allow ->
		    case ejabberd_auth:try_register(User, Server, Password) of
			{atomic, ok} ->
			    send_welcome_message(JID),
			    send_registration_notifications(JID),
			    ok;
			{atomic, exists} ->
			    {error, ?ERR_CONFLICT};
			{error, invalid_jid} ->
			    {error, ?ERR_JID_MALFORMED};
			{error, not_allowed} ->
			    {error, ?ERR_NOT_ALLOWED};
			{error, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR}
		    end
	    end
    end.


send_welcome_message(JID) ->
    Host = JID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    ejabberd_router:route(
	      jlib:make_jid("", Host, ""),
	      JID,
	      {xmlelement, "message", [{"type", "normal"}],
	       [{xmlelement, "subject", [], [{xmlcdata, Subj}]},
		{xmlelement, "body", [], [{xmlcdata, Body}]}]});
	_ ->
	    ok
    end.

send_registration_notifications(UJID) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
	[] -> ok;
	JIDs when is_list(JIDs) ->
	    Body = lists:flatten(
		     io_lib:format(
		       "The user '~s' was just created on node ~w.",
		       [jlib:jid_to_string(UJID), node()])),
	    lists:foreach(
	      fun(S) ->
		      case jlib:string_to_jid(S) of
			  error -> ok;
			  JID ->
			      ejabberd_router:route(
				jlib:make_jid("", Host, ""),
				JID,
				{xmlelement, "message", [{"type", "chat"}],
				 [{xmlelement, "body", [],
				   [{xmlcdata, Body}]}]})
		      end
	      end, JIDs);
	_ ->
	    ok
    end.

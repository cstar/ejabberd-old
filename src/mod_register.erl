%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_register).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, init/0, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ok.

init() ->
    ok.

process_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    UTag = xml:get_subtag(SubEl, "username"),
	    PTag = xml:get_subtag(SubEl, "password"),
	    RTag = xml:get_subtag(SubEl, "remove"),
	    Server = ?MYNAME,
	    if
		(UTag /= false) and (RTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:remove_user(User),
			    {iq, ID, result, XMLNS, [SubEl]};
			_ ->
			    if
				PTag /= false ->
				    Password = xml:get_tag_cdata(PTag),
				    case ejabberd_auth:remove_user(User,
								   Password) of
					ok ->
					    {iq, ID, result, XMLNS, [SubEl]};
					not_allowed ->
					    {iq, ID, error, XMLNS,
					     [SubEl, ?ERR_NOT_ALLOWED]};
					not_exists ->
					    {iq, ID, error, XMLNS,
					     [SubEl, {xmlelement,
						      "error",
						      [{"code", "404"}],
						      [{xmlcdata,
							"Not Found"}]}]};
					_ ->
					    {iq, ID, error, XMLNS,
					     [SubEl,
					      ?ERR_INTERNAL_SERVER_ERROR]}
				    end;
				true ->
				    {iq, ID, error, XMLNS,
					     [SubEl, ?ERR_BAD_REQUEST]}
			    end
		    end;
		(UTag == false) and (RTag /= false) ->
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:remove_user(User),
			    {iq, ID, result, XMLNS, [SubEl]};
			_ ->
			    {iq, ID, error, XMLNS, [SubEl, ?ERR_NOT_ALLOWED]}
		    end;
		(UTag /= false) and (PTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    Password = xml:get_tag_cdata(PTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:set_password(User, Password),
			    {iq, ID, result, XMLNS, [SubEl]};
			_ ->
			    case try_register(User, Password) of
				ok ->
				    {iq, ID, result, XMLNS, [SubEl]};
				{error, Error} ->
				    {iq, ID, error, XMLNS,
				     [SubEl, Error]}
			    end
		    end;
		true ->
		    {iq, ID, error, XMLNS,
		     [SubEl, ?ERR_BAD_REQUEST]}
	    end;
	get ->
	    {iq, ID, result, XMLNS, [{xmlelement,
				      "query",
				      [{"xmlns", "jabber:iq:register"}],
				      [{xmlelement, "instructions", [],
					[{xmlcdata,
					  "Choose a username and password "
					  "to register with this server."}]},
				       {xmlelement, "username", [], []},
				       {xmlelement, "password", [], []}]}]}
    end.


try_register(User, Password) ->
    case jlib:is_nodename(User) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    JID = jlib:make_jid(User, ?MYNAME, ""),
	    case acl:match_rule(register, JID) of
		deny ->
		    {error, ?ERR_CONFLICT};
		allow ->
		    case ejabberd_auth:try_register(User, Password) of
			{atomic, ok} ->
			    send_welcome_message(JID),
			    ok;
			{atomic, exists} ->
			    {error, ?ERR_CONFLICT};
			{error, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR}
		    end
	    end
    end.


send_welcome_message(JID) ->
    case ejabberd_config:get_local_option(welcome_message) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    ejabberd_router:route(
	      jlib:make_jid("", ?MYNAME, ""),
	      JID,
	      {xmlelement, "message", [{"type", "normal"}],
	       [{xmlelement, "subject", [], [{xmlcdata, Subj}]},
		{xmlelement, "body", [], [{xmlcdata, Body}]}]});
	_ ->
	    ok
    end.


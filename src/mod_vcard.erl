%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_vcard).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, init/2, stop/0,
	 process_local_iq/3,
	 process_sm_iq/3,
	 reindex_vcards/0,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-define(JUD_MATCHES, 30).

-record(vcard_search, {us,
		       user,     luser,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit
		      }).
-record(vcard, {us, vcard}).


start(Opts) ->
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
				{attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, vcard_search)}]),
    update_tables(),
    mnesia:add_table_index(vcard_search, luser),
    mnesia:add_table_index(vcard_search, lfn),
    mnesia:add_table_index(vcard_search, lfamily),
    mnesia:add_table_index(vcard_search, lgiven),
    mnesia:add_table_index(vcard_search, lmiddle),
    mnesia:add_table_index(vcard_search, lnickname),
    mnesia:add_table_index(vcard_search, lbday),
    mnesia:add_table_index(vcard_search, lctry),
    mnesia:add_table_index(vcard_search, llocality),
    mnesia:add_table_index(vcard_search, lemail),
    mnesia:add_table_index(vcard_search, lorgname),
    mnesia:add_table_index(vcard_search, lorgunit),

    ejabberd_hooks:add(remove_user,
		       ?MODULE, remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    catch mod_disco:register_sm_feature(?NS_VCARD),
    Hosts = gen_mod:get_hosts(Opts, "vjud."),
    Search = gen_mod:get_opt(search, Opts, true),
    register(ejabberd_mod_vcard, spawn(?MODULE, init, [Hosts, Search])).


init(Hosts, Search) ->
    case Search of
	false ->
	    loop(Hosts);
	_ ->
	    ejabberd_router:register_routes(Hosts),
	    loop(Hosts)
    end.

loop(Hosts) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Hosts);
	stop ->
	    catch ejabberd_router:unregister_routes(Hosts),
	    ok;
	_ ->
	    loop(Hosts)
    end.

stop() ->
    ejabberd_hooks:delete(remove_user,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_VCARD),
    catch mod_disco:unregister_sm_feature(?NS_VCARD),
    ejabberd_mod_vcard ! stop,
    ok.

process_local_iq(_From, _To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "vCard",
			     [{"xmlns", ?NS_VCARD}],
			     [{xmlelement, "FN", [],
			       [{xmlcdata, "ejabberd"}]},
			      {xmlelement, "URL", [],
			       [{xmlcdata,
				 "http://ejabberd.jabberstudio.org/"}]},
			      {xmlelement, "DESC", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Erlang Jabber Server\n"
				   "Copyright (c) 2002-2005 Alexey Shchepin")}]},
			      {xmlelement, "BDAY", [],
			       [{xmlcdata, "2002-11-16"}]}
			     ]}]}
    end.


process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    #jid{user = User, lserver = LServer, luser = LUser} = From,
	    case lists:member(LServer, ?MYHOSTS) of
		true ->
		    set_vcard(User, LServer, SubEl),
		    IQ#iq{type = result, sub_el = []};
		false ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	    end;
	get ->
	    #jid{luser = LUser, lserver = LServer} = To,
	    US = {LUser, LServer},
	    F = fun() ->
			mnesia:read({vcard, US})
		end,
	    Els = case mnesia:transaction(F) of
		      {atomic, Rs} ->
			  lists:map(fun(R) ->
					    R#vcard.vcard
				    end, Rs);
		      {aborted, _Reason} ->
			  []
		  end,
	    IQ#iq{type = result, sub_el = Els}
    end.

set_vcard(User, LServer, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
    EMail1   = xml:get_path_s(VCARD, [{elem, "EMAIL"}, {elem, "USERID"},cdata]),
    EMail2   = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),
    EMail = case EMail1 of
		"" ->
		    EMail2;
		_ ->
		    EMail1
	    end,

    LUser     = jlib:nodeprep(User),
    LFN       = stringprep:tolower(FN),
    LFamily   = stringprep:tolower(Family),
    LGiven    = stringprep:tolower(Given),
    LMiddle   = stringprep:tolower(Middle),
    LNickname = stringprep:tolower(Nickname),
    LBDay     = stringprep:tolower(BDay),
    LCTRY     = stringprep:tolower(CTRY),
    LLocality = stringprep:tolower(Locality),
    LEMail    = stringprep:tolower(EMail),
    LOrgName  = stringprep:tolower(OrgName),
    LOrgUnit  = stringprep:tolower(OrgUnit),

    US = {LUser, LServer},

    if
	(LUser     == error) or
	(LFN       == error) or
	(LFamily   == error) or
	(LGiven    == error) or
	(LMiddle   == error) or
	(LNickname == error) or
	(LBDay     == error) or
	(LCTRY     == error) or
	(LLocality == error) or
	(LEMail    == error) or
	(LOrgName  == error) or
	(LOrgUnit  == error) ->
	    {error, badarg};
	true ->
	    F = fun() ->
		mnesia:write(#vcard{us = US, vcard = VCARD}),
		mnesia:write(
		  #vcard_search{us        = US,
				user      = {User, LServer},
				luser     = LUser,
				fn        = FN,       lfn        = LFN,       
				family    = Family,   lfamily    = LFamily,   
				given     = Given,    lgiven     = LGiven,    
				middle    = Middle,   lmiddle    = LMiddle,   
				nickname  = Nickname, lnickname  = LNickname, 
				bday      = BDay,     lbday      = LBDay,     
				ctry      = CTRY,     lctry      = LCTRY,     
				locality  = Locality, llocality  = LLocality, 
				email     = EMail,    lemail     = LEMail,    
				orgname   = OrgName,  lorgname   = LOrgName,  
				orgunit   = OrgUnit,  lorgunit   = LOrgUnit   
			       })
		end,
		mnesia:transaction(F)
    end.

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).


-define(FORM(JID),
	[{xmlelement, "instructions", [],
	  [{xmlcdata, translate:translate(Lang, "You need an x:data capable client to search")}]},
	 {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
	  [{xmlelement, "title", [],
	    [{xmlcdata, translate:translate(Lang, "Search users in ") ++
	      jlib:jid_to_string(JID)}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata, translate:translate(Lang, "Fill in the form to search "
					    "for any matching Jabber User "
					    "(Add * to the end of field to "
					    "match substring)")}]},
	   ?TLFIELD("text-single", "User", "user"),
	   ?TLFIELD("text-single", "Full Name", "fn"),
	   ?TLFIELD("text-single", "Name", "given"),
	   ?TLFIELD("text-single", "Middle Name", "middle"),
	   ?TLFIELD("text-single", "Family Name", "family"),
	   ?TLFIELD("text-single", "Nickname", "nickname"),
	   ?TLFIELD("text-single", "Birthday", "bday"),
	   ?TLFIELD("text-single", "Country", "ctry"),
	   ?TLFIELD("text-single", "City", "locality"),
	   ?TLFIELD("text-single", "email", "email"),
	   ?TLFIELD("text-single", "Organization Name", "orgname"),
	   ?TLFIELD("text-single", "Organization Unit", "orgunit")
	  ]}]).




do_route(From, To, Packet) ->
    #jid{user = User, resource = Resource} = To,
    if
	(User /= "") or (Resource /= "") ->
	    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router ! {route, To, From, Err};
	true ->
	    IQ = jlib:iq_query_info(Packet),
	    case IQ of
		#iq{type = Type, xmlns = ?NS_SEARCH, lang = Lang, sub_el = SubEl} ->
		    case Type of
			set ->
			    XDataEl = find_xdata_el(SubEl),
			    case XDataEl of
				false ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_BAD_REQUEST),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    XData = jlib:parse_xdata_submit(XDataEl),
				    case XData of
					invalid ->
					    Err = jlib:make_error_reply(
						    Packet,
						    ?ERR_BAD_REQUEST),
					    ejabberd_router:route(To, From,
								  Err);
					_ ->
					    ResIQ =
						IQ#iq{
						  type = result,
						  sub_el =
						  [{xmlelement,
						    "query",
						    [{"xmlns", ?NS_SEARCH}],
						    [{xmlelement, "x",
						      [{"xmlns", ?NS_XDATA},
						       {"type", "result"}],
						      search_result(Lang, To, XData)
						     }]}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(ResIQ))
				    end
			    end;
			get ->
			    ResIQ = IQ#iq{type = result,
					  sub_el = [{xmlelement,
						     "query",
						     [{"xmlns", ?NS_SEARCH}],
						     ?FORM(To)
						    }]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_INFO, sub_el = SubEl} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(To, From, Err);
			get ->
			    ResIQ =
				IQ#iq{type = result,
				      sub_el = [{xmlelement,
						 "query",
						 [{"xmlns", ?NS_DISCO_INFO}],
						 [{xmlelement, "identity",
						   [{"category", "directory"},
						    {"type", "user"},
						    {"name",
						     "vCard User Search"}],
						   []},
						  {xmlelement, "feature",
						   [{"var", ?NS_SEARCH}], []},
						  {xmlelement, "feature",
						   [{"var", ?NS_VCARD}], []}
						 ]
						}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_ITEMS, sub_el = SubEl} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(To, From, Err);
			get ->
			    ResIQ = 
				IQ#iq{type = result,
				      sub_el = [{xmlelement,
						 "query",
						 [{"xmlns", ?NS_DISCO_INFO}],
						 []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = get, xmlns = ?NS_VCARD, lang = Lang} ->
		    ResIQ = 
			IQ#iq{type = result,
			      sub_el = [{xmlelement,
					 "vCard",
					 [{"xmlns", ?NS_VCARD}],
					 iq_get_vcard(Lang)}]},
		    ejabberd_router:route(To,
					  From,
					  jlib:iq_to_xml(ResIQ));
		_ ->
		    Err = jlib:make_error_reply(Packet,
						?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_vcard"}]},
     {xmlelement, "URL", [],
      [{xmlcdata,
        "http://ejabberd.jabberstudio.org/"}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(
		    Lang,
		    "ejabberd vCard module\n"
		    "Copyright (c) 2003-2005 Alexey Shchepin")}]}].

find_xdata_el({xmlelement, _Name, _Attrs, SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([{xmlelement, Name, Attrs, SubEls} | Els]) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_XDATA ->
	    {xmlelement, Name, Attrs, SubEls};
	_ ->
	    find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

-define(LFIELD(Label, Var),
	{xmlelement, "field", [{"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

search_result(Lang, JID, Data) ->
    [{xmlelement, "title", [],
      [{xmlcdata, translate:translate(Lang, "Results of search in ") ++
	jlib:jid_to_string(JID)}]},
     {xmlelement, "reported", [],
      [?LFIELD("JID", "jid"),
       ?LFIELD("Full Name", "fn"),
       ?LFIELD("Name", "given"),
       ?LFIELD("Middle Name", "middle"),
       ?LFIELD("Family Name", "family"),
       ?LFIELD("Nickname", "nickname"),
       ?LFIELD("Birthday", "bday"),
       ?LFIELD("Country", "ctry"),
       ?LFIELD("City", "locality"),
       ?LFIELD("email", "email"),
       ?LFIELD("Organization Name", "orgname"),
       ?LFIELD("Organization Unit", "orgunit")
      ]}] ++ lists:map(fun record_to_item/1, search(JID#jid.lserver, Data)).

-define(FIELD(Var, Val),
	{xmlelement, "field", [{"var", Var}],
	 [{xmlelement, "value", [],
	   [{xmlcdata, Val}]}]}).

record_to_item(R) ->
    {User, Server} = R#vcard_search.user,
    {xmlelement, "item", [],
     [
       ?FIELD("jid",      User ++ "@" ++ Server),
       ?FIELD("fn",       R#vcard_search.fn),
       ?FIELD("family",   R#vcard_search.family),
       ?FIELD("given",    R#vcard_search.given),
       ?FIELD("middle",   R#vcard_search.middle),
       ?FIELD("nickname", R#vcard_search.nickname),
       ?FIELD("bday",     R#vcard_search.bday),
       ?FIELD("ctry",     R#vcard_search.ctry),
       ?FIELD("locality", R#vcard_search.locality),
       ?FIELD("email",    R#vcard_search.email),
       ?FIELD("orgname",  R#vcard_search.orgname),
       ?FIELD("orgunit",  R#vcard_search.orgunit)
      ]
     }.


search(LServer, Data) ->
    MatchSpec = make_matchspec(LServer, Data),
    AllowReturnAll = gen_mod:get_module_opt(?MODULE, allow_return_all, false),
    if
	(MatchSpec == #vcard_search{_ = '_'}) and (not AllowReturnAll) ->
	    [];
	true ->
	    case catch mnesia:dirty_select(vcard_search,
					   [{MatchSpec, [], ['$_']}]) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]),
		    [];
		Rs ->
		    case gen_mod:get_module_opt(?MODULE, matches, ?JUD_MATCHES) of
			infinity ->
			    Rs;
			Val when is_integer(Val) and (Val > 0) ->
			    lists:sublist(Rs, Val);
			Val ->
			    ?ERROR_MSG("Illegal option value ~p. "
				       "Default value ~p substituted.",
				       [{matches, Val}, ?JUD_MATCHES]),
			    lists:sublist(Rs, ?JUD_MATCHES)
		    end
	    end
    end.


make_matchspec(LServer, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, LServer),
    Match.

filter_fields([], Match, _LServer) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_list(Val) and (Val /= "") ->
    LVal = stringprep:tolower(Val),
    NewMatch = case SVar of
                   "user" ->
		       case gen_mod:get_module_opt(
			      ?MODULE, search_all_hosts, true) of
			   true ->
			       Match#vcard_search{luser = make_val(LVal)};
			   false ->
			       Host = find_my_host(LServer),
			       Match#vcard_search{us = {make_val(LVal), Host}}
		       end;
                   "fn"       -> Match#vcard_search{lfn       = make_val(LVal)};
                   "family"   -> Match#vcard_search{lfamily   = make_val(LVal)};
                   "given"    -> Match#vcard_search{lgiven    = make_val(LVal)};
                   "middle"   -> Match#vcard_search{lmiddle   = make_val(LVal)};
                   "nickname" -> Match#vcard_search{lnickname = make_val(LVal)};
                   "bday"     -> Match#vcard_search{lbday     = make_val(LVal)};
                   "ctry"     -> Match#vcard_search{lctry     = make_val(LVal)};
                   "locality" -> Match#vcard_search{llocality = make_val(LVal)};
                   "email"    -> Match#vcard_search{lemail    = make_val(LVal)};
                   "orgname"  -> Match#vcard_search{lorgname  = make_val(LVal)};
                   "orgunit"  -> Match#vcard_search{lorgunit  = make_val(LVal)};
		   _          -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Val) ->
    case lists:suffix("*", Val) of
	true ->
	    lists:sublist(Val, length(Val) - 1) ++ '_';
	_ ->
	    Val
    end.

find_my_host(LServer) ->
    Parts = string:tokens(LServer, "."),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) ->
    ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_string(Parts),
    case lists:member(Domain, Hosts) of
	true ->
	    Domain;
	false ->
	    find_my_host(Tail, Hosts)
    end.

parts_to_string(Parts) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, $.] end, Parts)),
		 right, $.).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_vcard_t(R, _) ->
    US = R#vcard.us,
    User  = US,
    VCARD = R#vcard.vcard,

    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
    EMail    = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),

    {LUser, _LServer} = US,
    LFN       = stringprep:tolower(FN),
    LFamily   = stringprep:tolower(Family),
    LGiven    = stringprep:tolower(Given),
    LMiddle   = stringprep:tolower(Middle),
    LNickname = stringprep:tolower(Nickname),
    LBDay     = stringprep:tolower(BDay),
    LCTRY     = stringprep:tolower(CTRY),
    LLocality = stringprep:tolower(Locality),
    LEMail    = stringprep:tolower(EMail),
    LOrgName  = stringprep:tolower(OrgName),
    LOrgUnit  = stringprep:tolower(OrgUnit),

    if
	(LUser     == error) or
	(LFN       == error) or
	(LFamily   == error) or
	(LGiven    == error) or
	(LMiddle   == error) or
	(LNickname == error) or
	(LBDay     == error) or
	(LCTRY     == error) or
	(LLocality == error) or
	(LEMail    == error) or
	(LOrgName  == error) or
	(LOrgUnit  == error) ->
	    {error, badarg};
	true ->
	    mnesia:write(
	      #vcard_search{us        = US,
			    user      = User,     luser      = LUser,     
			    fn        = FN,       lfn        = LFN,       
			    family    = Family,   lfamily    = LFamily,   
			    given     = Given,    lgiven     = LGiven,    
			    middle    = Middle,   lmiddle    = LMiddle,   
			    nickname  = Nickname, lnickname  = LNickname, 
			    bday      = BDay,     lbday      = LBDay,     
			    ctry      = CTRY,     lctry      = LCTRY,     
			    locality  = Locality, llocality  = LLocality, 
			    email     = EMail,    lemail     = LEMail,    
			    orgname   = OrgName,  lorgname   = LOrgName,  
			    orgunit   = OrgUnit,  lorgunit   = LOrgUnit   
			   })
    end.


reindex_vcards() ->
    F = fun() ->
		mnesia:foldl(fun set_vcard_t/2, [], vcard)
	end,
    mnesia:transaction(F).


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({vcard, US}),
		mnesia:delete({vcard_search, US})
	end,
    mnesia:transaction(F).


update_tables() ->
    update_vcard_table(),
    update_vcard_search_table().

update_vcard_table() ->
    Fields = record_info(fields, vcard),
    case mnesia:table_info(vcard, attributes) of
	Fields ->
	    ok;
	[user, vcard] ->
	    ?INFO_MSG("Converting vcard table from "
		      "{user, vcard} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_vcard_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, vcard},
			      {attributes, record_info(fields, vcard)}]),
	    mnesia:transform_table(vcard, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_vcard_tmp_table),
			 mnesia:foldl(
			   fun(#vcard{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_vcard_tmp_table,
				     R#vcard{us = {U, Host}})
			   end, ok, vcard)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(vcard),
	    F2 = fun() ->
			 mnesia:write_lock_table(vcard),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_vcard_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard table", []),
	    mnesia:transform_table(vcard, ignore, Fields)
    end.


update_vcard_search_table() ->
    Fields = record_info(fields, vcard_search),
    case mnesia:table_info(vcard_search, attributes) of
	Fields ->
	    ok;
	[user,     luser,
	 fn,       lfn,
	 family,   lfamily,
	 given,    lgiven,
	 middle,   lmiddle,
	 nickname, lnickname,
	 bday,	   lbday,
	 ctry,	   lctry,
	 locality, llocality,
	 email,	   lemail,
	 orgname,  lorgname,
	 orgunit,  lorgunit] ->
	    ?INFO_MSG("Converting vcard_search table from "
		      "{user, luser, fn, lfn, family, lfamily, given, lgiven, middle, lmiddle, nickname, lnickname, bday, lbday, ctry, lctry, locality, llocality, email, lemail, orgname, lorgname, orgunit, lorgunit} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_vcard_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, vcard_search},
			      {attributes, record_info(fields, vcard_search)}]),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_vcard_tmp_table),
			 mnesia:foldl(
			   fun({vcard_search,
				User,     LUser,     
				FN,       LFN,       
				Family,   LFamily,   
				Given,    LGiven,    
				Middle,   LMiddle,   
				Nickname, LNickname, 
				BDay,     LBDay,     
				CTRY,     LCTRY,     
				Locality, LLocality, 
				EMail,    LEMail,    
				OrgName,  LOrgName,  
				OrgUnit,  LOrgUnit   
			       }, _) ->
				   mnesia:dirty_write(
				     mod_vcard_tmp_table,
				     #vcard_search{
				       us        = {LUser, Host},
				       user      = {User, Host},
				       luser     = LUser,
				       fn        = FN,       lfn        = LFN,
				       family    = Family,   lfamily    = LFamily,
				       given     = Given,    lgiven     = LGiven,
				       middle    = Middle,   lmiddle    = LMiddle,
				       nickname  = Nickname, lnickname  = LNickname,
				       bday      = BDay,     lbday      = LBDay,
				       ctry      = CTRY,     lctry      = LCTRY,
				       locality  = Locality, llocality  = LLocality,
				       email     = EMail,    lemail     = LEMail,
				       orgname   = OrgName,  lorgname   = LOrgName,
				       orgunit   = OrgUnit,  lorgunit   = LOrgUnit
				      })
			   end, ok, vcard_search)
		 end,
	    mnesia:transaction(F1),
	    lists:foreach(fun(I) ->
				  mnesia:del_table_index(
				    vcard_search,
				    element(I, {vcard_search,
						user,     luser,
						fn,       lfn,
						family,   lfamily,
						given,    lgiven,
						middle,   lmiddle,
						nickname, lnickname,
						bday,	   lbday,
						ctry,	   lctry,
						locality, llocality,
						email,	   lemail,
						orgname,  lorgname,
						orgunit,  lorgunit}))
			  end, mnesia:table_info(vcard_search, index)),
	    mnesia:clear_table(vcard_search),
	    mnesia:transform_table(vcard_search, ignore, Fields),
	    F2 = fun() ->
	        	 mnesia:write_lock_table(vcard_search),
	        	 mnesia:foldl(
	        	   fun(R, _) ->
	        		   mnesia:dirty_write(R)
	        	   end, ok, mod_vcard_tmp_table)
	         end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard_search table", []),
	    mnesia:transform_table(vcard_search, ignore, Fields)
    end.


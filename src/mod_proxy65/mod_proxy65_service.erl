%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_service.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 Bytestreams XMPP service.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_proxy65_service).
-author('xram@jabber.ru').

-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	]).

%% API.
-export([start_link/2]).

-include("../ejabberd.hrl").
-include("../jlib.hrl").

-define(PROCNAME, ejabberd_mod_proxy65_service).

-record(state, {
	  myhost,
	  serverhost,
	  name,
	  stream_addr,
	  port,
	  acl
	 }).

%% Unused callbacks.
handle_cast(_Request, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
%%----------------

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([Host, Opts]) ->
    {IP, State} = parse_options(Host, Opts),
    NewOpts = [Host, {ip, IP} | Opts],
    ejabberd_listener:add_listener(State#state.port, mod_proxy65_stream, NewOpts),
    ejabberd_router:register_route(State#state.myhost),
    {ok, State}.

terminate(_Reason, #state{myhost=MyHost, port=Port}) ->
    catch ejabberd_listener:delete_listener(Port),
    ejabberd_router:unregister_route(MyHost),
    ok.

handle_info({route, From, To, {xmlelement, "iq", _, _} = Packet}, State) ->
    IQ = jlib:iq_query_info(Packet),
    case catch process_iq(From, IQ, State) of
	Result when is_record(Result, iq) ->
	    ejabberd_router:route(To, From, jlib:iq_to_xml(Result));
	{'EXIT', Reason} ->
	    ?ERROR_MSG("Error when processing IQ stanza: ~p", [Reason]),
	    Err = jlib:make_error_reply(Packet, ?ERR_INTERNAL_SERVER_ERROR),
	    ejabberd_router:route(To, From, Err);
	_ ->
	    ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%%------------------------
%%% IQ Processing
%%%------------------------

%% disco#info request
process_iq(_, #iq{type = get, xmlns = ?NS_DISCO_INFO} = IQ, #state{name=Name}) ->
    IQ#iq{type = result, sub_el =
	  [{xmlelement, "query", [{"xmlns", ?NS_DISCO_INFO}], iq_disco_info(Name)}]};

%% disco#items request
process_iq(_, #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ, _) ->
    IQ#iq{type = result, sub_el =
	  [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}], []}]};

%% vCard request
process_iq(_, #iq{type = get, xmlns = ?NS_VCARD, lang = Lang} = IQ, _) ->
    IQ#iq{type = result, sub_el =
	  [{xmlelement, "vCard", [{"xmlns", ?NS_VCARD}], iq_vcard(Lang)}]};

%% bytestreams info request
process_iq(JID, #iq{type = get, sub_el = SubEl, xmlns = ?NS_BYTESTREAMS} = IQ,
	   #state{acl = ACL, stream_addr = StreamAddr, serverhost = ServerHost}) ->
    case acl:match_rule(ServerHost, ACL, JID) of
	allow ->
	    StreamHostEl = [{xmlelement, "streamhost", StreamAddr, []}],
	    IQ#iq{type = result, sub_el =
		  [{xmlelement, "query", [{"xmlns", ?NS_BYTESTREAMS}], StreamHostEl}]};
	deny ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
    end;

%% bytestream activation request
process_iq(InitiatorJID, #iq{type = set, sub_el = SubEl, xmlns = ?NS_BYTESTREAMS} = IQ,
	   #state{acl = ACL, serverhost = ServerHost}) ->
    case acl:match_rule(ServerHost, ACL, InitiatorJID) of
	allow ->
	    ActivateEl = xml:get_path_s(SubEl, [{elem, "activate"}]),
	    SID = xml:get_tag_attr_s("sid", SubEl),
	    case catch jlib:string_to_jid(xml:get_tag_cdata(ActivateEl)) of
		TargetJID when is_record(TargetJID, jid), SID /= "",
		               length(SID) =< 128, TargetJID /= InitiatorJID ->
		    Target = jlib:jid_to_string(jlib:jid_tolower(TargetJID)),
		    Initiator = jlib:jid_to_string(jlib:jid_tolower(InitiatorJID)),
		    SHA1 = sha:sha(SID ++ Initiator ++ Target),
		    case mod_proxy65_sm:activate_stream(SHA1, InitiatorJID, TargetJID, ServerHost) of
			ok ->
			    IQ#iq{type = result, sub_el = []};
			false ->
			    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
			limit ->
			    IQ#iq{type = error, sub_el = [SubEl, ?ERR_RESOURCE_CONSTRAINT]};
			conflict ->
			    IQ#iq{type = error, sub_el = [SubEl, ?ERR_CONFLICT]};
			_ ->
			    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
		    end;
		_ ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	    end;
	deny ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
    end;

%% Unknown "set" or "get" request
process_iq(_, #iq{type=Type, sub_el=SubEl} = IQ, _) when Type==get; Type==set ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};

%% IQ "result" or "error".
process_iq(_, _, _) ->
    ok.

%%%-------------------------
%%% Auxiliary functions.
%%%-------------------------
-define(FEATURE(Feat), {xmlelement,"feature",[{"var", Feat}],[]}).

iq_disco_info(Name) ->
    [{xmlelement, "identity",
      [{"category", "proxy"},
       {"type", "bytestreams"},
       {"name", Name}], []},
     ?FEATURE(?NS_DISCO_INFO),
     ?FEATURE(?NS_DISCO_ITEMS),
     ?FEATURE(?NS_VCARD),
     ?FEATURE(?NS_BYTESTREAMS)].

iq_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_proxy65"}]},
     {xmlelement, "URL", [],
      [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(Lang, "ejabberd SOCKS5 Bytestreams module\n"
       "Copyright (c) 2003-2006 Alexey Shchepin")}]}].

parse_options(ServerHost, Opts) ->
    MyHost = gen_mod:get_opt(host, Opts, "proxy." ++ ServerHost),
    Port = gen_mod:get_opt(port, Opts, 7777),
    ACL = gen_mod:get_opt(access, Opts, all),
    Name = gen_mod:get_opt(name, Opts, "SOCKS5 Bytestreams"),
    IP = case gen_mod:get_opt(ip, Opts, none) of
	         none -> get_proxy_or_domainip(ServerHost, MyHost);
	         Addr -> Addr
	     end,
    [_ | StrIP] = lists:append([[$. | integer_to_list(X)] || X <- inet:ip_to_bytes(IP)]),
    StreamAddr = [{"jid", MyHost}, {"host", StrIP}, {"port", integer_to_list(Port)}],
    {IP, #state{myhost      = MyHost,
		serverhost  = ServerHost,
		name        = Name,
		port        = Port,
		stream_addr = StreamAddr, 
		acl         = ACL}}.

%% Return the IP of the proxy host, or if not found, the ip of the xmpp domain
get_proxy_or_domainip(ServerHost, MyHost) ->
    case inet:getaddr(MyHost, inet) of
        {ok, Addr} -> Addr;
        {error, _} ->
            case inet:getaddr(ServerHost) of
                {ok, Addr} -> Addr;
                {error, _} -> {127,0,0,1}
            end
    end.
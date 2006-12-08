%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_c2s).
-author('alexey@sevcom.net').
-update_info({update, 0}).

-behaviour(gen_fsm).

%% External exports
-export([start/2,
	 start_link/2,
	 send_text/2,
	 send_element/2,
	 socket_type/0,
	 get_presence/1]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 wait_for_auth/2,
	 wait_for_feature_request/2,
	 wait_for_bind/2,
	 wait_for_session/2,
	 wait_for_sasl_response/2,
	 session_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(SETS, gb_sets).

-record(state, {socket,
		sockmod,
		streamid,
		sasl_state,
		access,
		shaper,
		zlib = false,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		jid,
		user = "", server = ?MYNAME, resource = "",
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_i = ?SETS:new(),
		pres_last, pres_pri,
		pres_timestamp,
		pres_invis = false,
		privacy_list = none,
		lang}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"id='~s' from='~s'~s~s>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NS_ERR,
	xml:element_to_string(?SERR_INVALID_NAMESPACE)).
-define(INVALID_XML_ERR,
	xml:element_to_string(?SERR_XML_NOT_WELL_FORMED)).
-define(HOST_UNKNOWN_ERR,
	xml:element_to_string(?SERR_HOST_UNKNOWN)).
-define(POLICY_VIOLATION_ERR(Lang, Text),
	xml:element_to_string(?SERRT_POLICY_VIOLATION(Lang, Text))).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_c2s_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_c2s, [SockData, Opts], ?FSMOPTS).

socket_type() ->
    xml_stream.

%% Return Username, Resource and presence information
get_presence(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, {get_presence}, 1000).

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
init([{SockMod, Socket}, Opts]) ->
    Access = case lists:keysearch(access, 1, Opts) of
		 {value, {_, A}} -> A;
		 _ -> all
	     end,
    Shaper = case lists:keysearch(shaper, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    Zlib = lists:member(zlib, Opts),
    StartTLS = lists:member(starttls, Opts),
    StartTLSRequired = lists:member(starttls_required, Opts),
    TLSEnabled = lists:member(tls, Opts),
    TLS = StartTLS orelse StartTLSRequired orelse TLSEnabled,
    TLSOpts = lists:filter(fun({certfile, _}) -> true;
			      (_) -> false
			   end, Opts),
    Socket1 =
	if
	    TLSEnabled ->
		SockMod:starttls(Socket, TLSOpts);
	    true ->
		Socket
	end,
    {ok, wait_for_stream, #state{socket       = Socket1,
				 sockmod      = SockMod,
				 zlib         = Zlib,
				 tls          = TLS,
				 tls_required = StartTLSRequired,
				 tls_enabled  = TLSEnabled,
				 tls_options  = TLSOpts,
				 streamid     = new_id(),
				 access       = Access,
				 shaper       = Shaper}}.


%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    DefaultLang = case ?MYLANG of
		      undefined ->
			  " xml:lang='en'";
		      DL ->
			  " xml:lang='" ++ DL ++ "'"
		  end,
    case xml:get_attr_s("xmlns:stream", Attrs) of
	?NS_STREAM ->
	    Server = jlib:nameprep(xml:get_attr_s("to", Attrs)),
	    case lists:member(Server, ?MYHOSTS) of
		true ->
		    Lang = xml:get_attr_s("xml:lang", Attrs),
		    case xml:get_attr_s("version", Attrs) of
			"1.0" ->
			    Header = io_lib:format(?STREAM_HEADER,
						   [StateData#state.streamid,
						    Server,
						    " version='1.0'",
						    DefaultLang]),
			    send_text(StateData, Header),
			    case StateData#state.authenticated of
				false ->
				    SASLState =
					cyrsasl:server_new(
					  "jabber", Server, "", [],
					  fun(U) ->
						  ejabberd_auth:get_password(
						    U, Server)
					  end,
					  fun(U, P) ->
						  ejabberd_auth:check_password(
						    U, Server, P)
					  end),
				    Mechs = lists:map(
					      fun(S) ->
						      {xmlelement, "mechanism", [],
						       [{xmlcdata, S}]}
					      end, cyrsasl:listmech(Server)),
				    SockMod =
					(StateData#state.sockmod):get_sockmod(
					  StateData#state.socket),
				    Zlib = StateData#state.zlib,
				    CompressFeature =
					case Zlib andalso
					    (SockMod == gen_tcp) of
					    true ->
						[{xmlelement, "compression",
						  [{"xmlns", ?NS_FEATURE_COMPRESS}],
						  [{xmlelement, "method",
						    [], [{xmlcdata, "zlib"}]}]}];
					    _ ->
						[]
					end,
				    TLS = StateData#state.tls,
				    TLSEnabled = StateData#state.tls_enabled,
				    TLSRequired = StateData#state.tls_required,
				    TLSFeature =
					case (TLS == true) andalso
					    (TLSEnabled == false) andalso
					    (SockMod == gen_tcp) of
					    true ->
						case TLSRequired of
						    true ->
							[{xmlelement, "starttls",
							  [{"xmlns", ?NS_TLS}],
							  [{xmlelement, "required",
							    [], []}]}];
						    _ ->
							[{xmlelement, "starttls",
							  [{"xmlns", ?NS_TLS}], []}]
						end;
					    false ->
						[]
					end,
				    send_element(StateData,
						 {xmlelement, "stream:features", [],
						  TLSFeature ++ CompressFeature ++
						  [{xmlelement, "mechanisms",
						    [{"xmlns", ?NS_SASL}],
						    Mechs}] ++
						   ejabberd_hooks:run_fold(
						     c2s_stream_features,
						     Server,
						     [], [])}),
				    {next_state, wait_for_feature_request,
				     StateData#state{server = Server,
						     sasl_state = SASLState,
						     lang = Lang}};
				_ ->
				    case StateData#state.resource of
					"" ->
					    send_element(
					      StateData,
					      {xmlelement, "stream:features", [],
					       [{xmlelement, "bind",
						 [{"xmlns", ?NS_BIND}], []},
						{xmlelement, "session",
						 [{"xmlns", ?NS_SESSION}], []}]}),
					    {next_state, wait_for_bind,
					     StateData#state{server = Server,
							     lang = Lang}};
					_ ->
					    send_element(
					      StateData,
					      {xmlelement, "stream:features", [], []}),
					    {next_state, wait_for_session,
					     StateData#state{server = Server,
							     lang = Lang}}
				    end
			    end;
			_ ->
			    Header = io_lib:format(
				       ?STREAM_HEADER,
				       [StateData#state.streamid, Server, "",
					DefaultLang]),
			    if
				(not StateData#state.tls_enabled) and
				StateData#state.tls_required ->
				    send_text(StateData,
					      Header ++
					      ?POLICY_VIOLATION_ERR(
						 Lang,
						 "Use of STARTTLS required") ++
					      ?STREAM_TRAILER),
				    {stop, normal, StateData};
				true ->
				    send_text(StateData, Header),
				    {next_state, wait_for_auth, 
				     StateData#state{server = Server,
						     lang = Lang}}
			    end
		    end;
		_ ->
		    Header = io_lib:format(
			       ?STREAM_HEADER,
			       [StateData#state.streamid, ?MYNAME, "",
				DefaultLang]),
		    send_text(StateData,
			      Header ++ ?HOST_UNKNOWN_ERR ++ ?STREAM_TRAILER),
		    {stop, normal, StateData}
	    end;
	_ ->
	    Header = io_lib:format(
		       ?STREAM_HEADER,
		       [StateData#state.streamid, ?MYNAME, "", DefaultLang]),
	    send_text(StateData,
		      Header ++ ?INVALID_NS_ERR ++ ?STREAM_TRAILER),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamelement, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream({xmlstreamend, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream({xmlstreamerror, _}, StateData) ->
    Header = io_lib:format(?STREAM_HEADER,
			   ["none", ?MYNAME, " version='1.0'", ""]),
    send_text(StateData,
	      Header ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_auth({xmlstreamelement, El}, StateData) ->
    case is_auth_packet(El) of
	{auth, _ID, get, {U, _, _, _}} ->
	    {xmlelement, Name, Attrs, _Els} = jlib:make_result_iq_reply(El),
	    case U of
		"" ->
		    UCdata = [];
		_ ->
		    UCdata = [{xmlcdata, U}]
	    end,
	    Res = case ejabberd_auth:plain_password_required(
			 StateData#state.server) of
		      false ->
			  {xmlelement, Name, Attrs,
			   [{xmlelement, "query", [{"xmlns", ?NS_AUTH}],
			     [{xmlelement, "username", [], UCdata},
			      {xmlelement, "password", [], []},
			      {xmlelement, "digest", [], []},
			      {xmlelement, "resource", [], []}
			     ]}]};
		      true ->
			  {xmlelement, Name, Attrs,
			   [{xmlelement, "query", [{"xmlns", ?NS_AUTH}],
			     [{xmlelement, "username", [], UCdata},
			      {xmlelement, "password", [], []},
			      {xmlelement, "resource", [], []}
			     ]}]}
		  end,
	    send_element(StateData, Res),
	    {next_state, wait_for_auth, StateData};
	{auth, _ID, set, {_U, _P, _D, ""}} ->
	    Err = jlib:make_error_reply(
		    El,
		    ?ERR_AUTH_NO_RESOURCE_PROVIDED(StateData#state.lang)),
	    send_element(StateData, Err),
	    {next_state, wait_for_auth, StateData};
	{auth, _ID, set, {U, P, D, R}} ->
	    JID = jlib:make_jid(U, StateData#state.server, R),
	    case (JID /= error) andalso
		(acl:match_rule(StateData#state.server,
				StateData#state.access, JID) == allow) of
		true ->
		    case ejabberd_auth:check_password(
			   U, StateData#state.server, P,
			   StateData#state.streamid, D) of
			true ->
			    ?INFO_MSG(
			       "(~w) Accepted legacy authentication for ~s",
			       [StateData#state.socket,
				jlib:jid_to_string(JID)]),
			    SID = {now(), self()},
			    ejabberd_sm:open_session(
			      SID, U, StateData#state.server, R),
			    Res1 = jlib:make_result_iq_reply(El),
			    Res = setelement(4, Res1, []),
			    send_element(StateData, Res),
			    change_shaper(StateData, JID),
			    {Fs, Ts} = ejabberd_hooks:run_fold(
					 roster_get_subscription_lists,
					 StateData#state.server,
					 {[], []},
					 [U, StateData#state.server]),
			    LJID = jlib:jid_tolower(
				     jlib:jid_remove_resource(JID)),
			    Fs1 = [LJID | Fs],
			    Ts1 = [LJID | Ts],
			    PrivList =
				ejabberd_hooks:run_fold(
				  privacy_get_user_list, StateData#state.server,
				  none,
				  [U, StateData#state.server]),
			    {next_state, session_established,
			     StateData#state{user = U,
					     resource = R,
					     jid = JID,
					     sid = SID,
					     pres_f = ?SETS:from_list(Fs1),
					     pres_t = ?SETS:from_list(Ts1),
					     privacy_list = PrivList}};
			_ ->
			    ?INFO_MSG(
			       "(~w) Failed legacy authentication for ~s",
			       [StateData#state.socket,
				jlib:jid_to_string(JID)]),
			    Err = jlib:make_error_reply(
				    El, ?ERR_NOT_AUTHORIZED),
			    send_element(StateData, Err),
			    {next_state, wait_for_auth, StateData}
		    end;
		_ ->
		    if
			JID == error ->
			    ?INFO_MSG(
			       "(~w) Forbidden legacy authentication for "
			       "username '~s' with resource '~s'",
			       [StateData#state.socket, U, R]),
			    Err = jlib:make_error_reply(El, ?ERR_JID_MALFORMED),
			    send_element(StateData, Err),
			    {next_state, wait_for_auth, StateData};
			true ->
			    ?INFO_MSG(
			       "(~w) Forbidden legacy authentication for ~s",
			       [StateData#state.socket,
				jlib:jid_to_string(JID)]),
			    Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
			    send_element(StateData, Err),
			    {next_state, wait_for_auth, StateData}
		    end
	    end;
	_ ->
	    process_unauthenticated_stanza(StateData, El),
	    {next_state, wait_for_auth, StateData}
    end;

wait_for_auth({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    Zlib = StateData#state.zlib,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    TLSRequired = StateData#state.tls_required,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {xml:get_attr_s("xmlns", Attrs), Name} of
	{?NS_SASL, "auth"} when not ((SockMod == gen_tcp) and TLSRequired) ->
	    Mech = xml:get_attr_s("mechanism", Attrs),
	    ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
	    case cyrsasl:server_start(StateData#state.sasl_state,
				      Mech,
				      ClientIn) of
		{ok, Props} ->
		    (StateData#state.sockmod):reset_stream(
		      StateData#state.socket),
		    send_element(StateData,
				 {xmlelement, "success",
				  [{"xmlns", ?NS_SASL}], []}),
		    U = xml:get_attr_s(username, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s",
			      [StateData#state.socket, U]),
		    {next_state, wait_for_stream,
		     StateData#state{streamid = new_id(),
				     authenticated = true,
				     user = U
				    }};
		{continue, ServerOut, NewSASLState} ->
		    send_element(StateData,
				 {xmlelement, "challenge",
				  [{"xmlns", ?NS_SASL}],
				  [{xmlcdata,
				    jlib:encode_base64(ServerOut)}]}),
		    {next_state, wait_for_sasl_response,
		     StateData#state{sasl_state = NewSASLState}};
		{error, Error} ->
		    send_element(StateData,
				 {xmlelement, "failure",
				  [{"xmlns", ?NS_SASL}],
				  [{xmlelement, Error, [], []}]}),
		    {next_state, wait_for_feature_request, StateData}
	    end;
	{?NS_TLS, "starttls"} when TLS == true,
				   TLSEnabled == false,
				   SockMod == gen_tcp ->
	    TLSOpts = case ejabberd_config:get_local_option(
			     {domain_certfile, StateData#state.server}) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1, StateData#state.tls_options)]
		      end,
	    Socket = StateData#state.socket,
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  xml:element_to_string(
			    {xmlelement, "proceed", [{"xmlns", ?NS_TLS}], []})),
	    {next_state, wait_for_stream,
	     StateData#state{socket = TLSSocket,
			     streamid = new_id(),
			     tls_enabled = true
			    }};
	{?NS_COMPRESS, "compress"} when Zlib == true,
					SockMod == gen_tcp ->
	    case xml:get_subtag(El, "method") of
		false ->
		    send_element(StateData,
		                 {xmlelement, "failure",
				  [{"xmlns", ?NS_COMPRESS}],
				  [{xmlelement, "setup-failed", [], []}]}),
		    {next_state, wait_for_feature_request, StateData};
		Method ->
		    case xml:get_tag_cdata(Method) of
			"zlib" ->
			    Socket = StateData#state.socket,
			    ZlibSocket = (StateData#state.sockmod):compress(
					   Socket,
					   xml:element_to_string(
					     {xmlelement, "compressed",
					      [{"xmlns", ?NS_COMPRESS}], []})),
			    {next_state, wait_for_stream,
			     StateData#state{socket = ZlibSocket,
					     streamid = new_id()
					    }};
			_ ->
			    send_element(StateData,
					 {xmlelement, "failure",
					  [{"xmlns", ?NS_COMPRESS}],
					  [{xmlelement, "unsupported-method",
					    [], []}]}),
			    {next_state, wait_for_feature_request, StateData}
		    end
	    end;
	_ ->
	    if
		(SockMod == gen_tcp) and TLSRequired ->
		    Lang = StateData#state.lang,
		    send_text(StateData, ?POLICY_VIOLATION_ERR(
					    Lang,
					    "Use of STARTTLS required") ++
					 ?STREAM_TRAILER),
		    {stop, normal, StateData};
		true ->
		    process_unauthenticated_stanza(StateData, El),
		    {next_state, wait_for_feature_request, StateData}
	    end
    end;

wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_feature_request(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_sasl_response({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    case {xml:get_attr_s("xmlns", Attrs), Name} of
	{?NS_SASL, "response"} ->
	    ClientIn = jlib:decode_base64(xml:get_cdata(Els)),
	    case cyrsasl:server_step(StateData#state.sasl_state,
				     ClientIn) of
		{ok, Props} ->
		    (StateData#state.sockmod):reset_stream(
		      StateData#state.socket),
		    send_element(StateData,
				 {xmlelement, "success",
				  [{"xmlns", ?NS_SASL}], []}),
		    U = xml:get_attr_s(username, Props),
		    ?INFO_MSG("(~w) Accepted authentication for ~s",
			      [StateData#state.socket, U]),
		    {next_state, wait_for_stream,
		     StateData#state{streamid = new_id(),
				     authenticated = true,
				     user = U
				    }};
		{continue, ServerOut, NewSASLState} ->
		    send_element(StateData,
				 {xmlelement, "challenge",
				  [{"xmlns", ?NS_SASL}],
				  [{xmlcdata,
				    jlib:encode_base64(ServerOut)}]}),
		    {next_state, wait_for_sasl_response,
		     StateData#state{sasl_state = NewSASLState}};
		{error, Error} ->
		    send_element(StateData,
				 {xmlelement, "failure",
				  [{"xmlns", ?NS_SASL}],
				  [{xmlelement, Error, [], []}]}),
		    {next_state, wait_for_feature_request, StateData}
	    end;
	_ ->
	    process_unauthenticated_stanza(StateData, El),
	    {next_state, wait_for_feature_request, StateData}
    end;

wait_for_sasl_response({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_sasl_response({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_sasl_response(closed, StateData) ->
    {stop, normal, StateData}.



wait_for_bind({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
	#iq{type = set, xmlns = ?NS_BIND, sub_el = SubEl} = IQ ->
	    U = StateData#state.user,
	    R1 = xml:get_path_s(SubEl, [{elem, "resource"}, cdata]),
	    R = case jlib:resourceprep(R1) of
		    error -> error;
		    "" ->
			lists:concat(
			  [randoms:get_string() | tuple_to_list(now())]);
		    Resource -> Resource
		end,
	    case R of
		error ->
		    Err = jlib:make_error_reply(El, ?ERR_BAD_REQUEST),
		    send_element(StateData, Err),
		    {next_state, wait_for_bind, StateData};
		_ ->
		    JID = jlib:make_jid(U, StateData#state.server, R),
		    Res = IQ#iq{type = result,
				sub_el = [{xmlelement, "bind",
					   [{"xmlns", ?NS_BIND}],
					   [{xmlelement, "jid", [],
					     [{xmlcdata,
					       jlib:jid_to_string(JID)}]}]}]},
		    send_element(StateData, jlib:iq_to_xml(Res)),
		    {next_state, wait_for_session,
		     StateData#state{resource = R, jid = JID}}
	    end;
	_ ->
	    {next_state, wait_for_bind, StateData}
    end;

wait_for_bind({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_bind({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_bind(closed, StateData) ->
    {stop, normal, StateData}.



wait_for_session({xmlstreamelement, El}, StateData) ->
    case jlib:iq_query_info(El) of
	#iq{type = set, xmlns = ?NS_SESSION} ->
	    U = StateData#state.user,
	    R = StateData#state.resource,
	    JID = StateData#state.jid,
	    case acl:match_rule(StateData#state.server,
				StateData#state.access, JID) of
		allow ->
		    ?INFO_MSG("(~w) Opened session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),
		    SID = {now(), self()},
		    ejabberd_sm:open_session(
		      SID, U, StateData#state.server, R),
		    Res = jlib:make_result_iq_reply(El),
		    send_element(StateData, Res),
		    change_shaper(StateData, JID),
		    {Fs, Ts} = ejabberd_hooks:run_fold(
				 roster_get_subscription_lists,
				 StateData#state.server,
				 {[], []},
				 [U, StateData#state.server]),
		    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
		    Fs1 = [LJID | Fs],
		    Ts1 = [LJID | Ts],
		    PrivList =
			ejabberd_hooks:run_fold(
			  privacy_get_user_list, StateData#state.server,
			  none,
			  [U, StateData#state.server]),
		    {next_state, session_established,
		     StateData#state{sid = SID,
				     pres_f = ?SETS:from_list(Fs1),
				     pres_t = ?SETS:from_list(Ts1),
				     privacy_list = PrivList}};
		_ ->
		    ?INFO_MSG("(~w) Forbidden session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),
		    Err = jlib:make_error_reply(El, ?ERR_NOT_ALLOWED),
		    send_element(StateData, Err),
		    {next_state, wait_for_session, StateData}
	    end;
	_ ->
	    {next_state, wait_for_session, StateData}
    end;

wait_for_session({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_session({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_session(closed, StateData) ->
    {stop, normal, StateData}.




session_established({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, _Els} = El,
    User = StateData#state.user,
    Server = StateData#state.server,
    % TODO: check 'from' attribute in stanza
    FromJID = StateData#state.jid,
    To = xml:get_attr_s("to", Attrs),
    ToJID = case To of
		"" ->
		    jlib:make_jid(User, Server, "");
		_ ->
		    jlib:string_to_jid(To)
	    end,
    NewEl1 = jlib:remove_attr("xmlns", El),
    NewEl = case xml:get_attr_s("xml:lang", Attrs) of
		"" ->
		    case StateData#state.lang of
			"" -> NewEl1;
			Lang ->
			    xml:replace_tag_attr("xml:lang", Lang, NewEl1)
		    end;
		_ ->
		    NewEl1
	    end,
    NewState =
	case ToJID of
	    error ->
		case xml:get_attr_s("type", Attrs) of
		    "error" -> StateData;
		    "result" -> StateData;
		    _ ->
			Err = jlib:make_error_reply(NewEl, ?ERR_JID_MALFORMED),
			send_element(StateData, Err),
			StateData
		end;
	    _ ->
		case Name of
		    "presence" ->
			case ToJID of
			    #jid{user = User,
				 server = Server,
				 resource = ""} ->
				?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
				       [FromJID, NewEl, StateData]),
				presence_update(FromJID, NewEl, StateData);
			    _ ->
				presence_track(FromJID, ToJID, NewEl, StateData)
			end;
		    "iq" ->
			case StateData#state.privacy_list of
			    none ->
				ejabberd_router:route(FromJID, ToJID, NewEl),
				StateData;
			    _PrivList ->
				case jlib:iq_query_info(NewEl) of
				    #iq{xmlns = ?NS_PRIVACY} = IQ ->
					process_privacy_iq(
					  FromJID, ToJID, IQ, StateData);
				    _ ->
					ejabberd_hooks:run(
					  user_send_packet,
					  Server,
					  [FromJID, ToJID, NewEl]),
					ejabberd_router:route(
					  FromJID, ToJID, NewEl),
					StateData
				end
			end;
		    "message" ->
			ejabberd_hooks:run(user_send_packet,
					   Server,
					   [FromJID, ToJID, NewEl]),
			ejabberd_router:route(FromJID, ToJID, NewEl),
			StateData;
		    _ ->
			StateData
		end
	end,
    {next_state, session_established, NewState};

session_established({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};

session_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData, ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

session_established(closed, StateData) ->
    {stop, normal, StateData}.



%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
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
handle_sync_event({get_presence}, _From, StateName, StateData) ->
    User = StateData#state.user,
    PresLast = StateData#state.pres_last,

    Show = get_showtag(PresLast),
    Status = get_statustag(PresLast),
    Resource = StateData#state.resource,

    Reply = {User, Resource, Show, Status},
    {reply, Reply, StateName, StateData};

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
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info(replaced, _StateName, StateData) ->
    Lang = StateData#state.lang,
    send_text(StateData,
	      xml:element_to_string(
		?SERRT_CONFLICT(Lang, "Replaced by new connection"))
	      ++ ?STREAM_TRAILER),
    {stop, normal, StateData#state{authenticated = replaced}};
handle_info({route, From, To, Packet}, StateName, StateData) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    {Pass, NewAttrs, NewState} =
	case Name of
	    "presence" ->
		case xml:get_attr_s("type", Attrs) of
		    "probe" ->
			LFrom = jlib:jid_tolower(From),
			LBFrom = jlib:jid_remove_resource(LFrom),
			NewStateData =
			    case ?SETS:is_element(
				    LFrom, StateData#state.pres_a) orelse
				?SETS:is_element(
				   LBFrom, StateData#state.pres_a) of
				true ->
				    StateData;
				false ->
				    case ?SETS:is_element(
					    LFrom, StateData#state.pres_f) of
					true ->
					    A = ?SETS:add_element(
						   LFrom,
						   StateData#state.pres_a),
					    StateData#state{pres_a = A};
					false ->
					    case ?SETS:is_element(
						    LBFrom, StateData#state.pres_f) of
						true ->
						    A = ?SETS:add_element(
							   LBFrom,
							   StateData#state.pres_a),
						    StateData#state{pres_a = A};
						false ->
						    StateData
					    end
				    end
			    end,
			process_presence_probe(From, To, NewStateData),
			{false, Attrs, NewStateData};
		    "error" ->
			NewA = remove_element(jlib:jid_tolower(From),
					      StateData#state.pres_a),
			{true, Attrs, StateData#state{pres_a = NewA}};
		    "invisible" ->
			Attrs1 = lists:keydelete("type", 1, Attrs),
			{true, [{"type", "unavailable"} | Attrs1], StateData};
		    "subscribe" ->
			{true, Attrs, StateData};
		    "subscribed" ->
			{true, Attrs, StateData};
		    "unsubscribe" ->
			{true, Attrs, StateData};
		    "unsubscribed" ->
			{true, Attrs, StateData};
		    _ ->
			case ejabberd_hooks:run_fold(
			       privacy_check_packet, StateData#state.server,
			       allow,
			       [StateData#state.user,
				StateData#state.server,
				StateData#state.privacy_list,
				{From, To, Packet},
				in]) of
			    allow ->
				LFrom = jlib:jid_tolower(From),
				LBFrom = jlib:jid_remove_resource(LFrom),
				case ?SETS:is_element(
					LFrom, StateData#state.pres_a) orelse
				    ?SETS:is_element(
				       LBFrom, StateData#state.pres_a) of
				    true ->
					{true, Attrs, StateData};
				    false ->
					case ?SETS:is_element(
						LFrom, StateData#state.pres_f) of
					    true ->
						A = ?SETS:add_element(
						       LFrom,
						       StateData#state.pres_a),
						{true, Attrs,
						 StateData#state{pres_a = A}};
					    false ->
						case ?SETS:is_element(
							LBFrom, StateData#state.pres_f) of
						    true ->
							A = ?SETS:add_element(
							       LBFrom,
							       StateData#state.pres_a),
							{true, Attrs,
							 StateData#state{pres_a = A}};
						    false ->
							{true, Attrs, StateData}
						end
					end
				end;
			    deny ->
				{false, Attrs, StateData}
			end
		end;
	    "broadcast" ->
		?DEBUG("broadcast~n~p~n", [Els]),
		case Els of
		    [{item, IJID, ISubscription}] ->
			{false, Attrs,
			 roster_change(IJID, ISubscription,
				       StateData)};
		    [{exit, Reason}] ->
			{exit, Attrs, Reason};
		    [{privacy_list, PrivList, PrivListName}] ->
			case ejabberd_hooks:run_fold(
			       privacy_updated_list, StateData#state.server,
			       false,
			       [StateData#state.privacy_list,
				PrivList]) of
			    false ->
				{false, Attrs, StateData};
			    NewPL ->
				PrivPushIQ =
				    #iq{type = set, xmlns = ?NS_PRIVACY,
					id = "push",
					sub_el = [{xmlelement, "query",
						   [{"xmlns", ?NS_PRIVACY}],
						   [{xmlelement, "list",
						     [{"name", PrivListName}],
						     []}]}]},
				PrivPushEl =
				    jlib:replace_from_to(
				      jlib:jid_remove_resource(
					StateData#state.jid),
				      StateData#state.jid,
				      jlib:iq_to_xml(PrivPushIQ)),
				send_element(StateData, PrivPushEl),
				{false, Attrs, StateData#state{privacy_list = NewPL}}
			end;
		    _ ->
			{false, Attrs, StateData}
		end;
	    "iq" ->
		IQ = jlib:iq_query_info(Packet),
		case IQ of
		    #iq{xmlns = ?NS_VCARD} ->
			Host = StateData#state.server,
			case ets:lookup(sm_iqtable, {?NS_VCARD, Host}) of
			    [{_, Module, Function, Opts}] ->
				gen_iq_handler:handle(Host, Module, Function, Opts,
						      From, To, IQ);
			    [] ->
				Err = jlib:make_error_reply(
					Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
				ejabberd_router:route(To, From, Err)
			end,
			{false, Attrs, StateData};
		    #iq{} ->
			case ejabberd_hooks:run_fold(
			       privacy_check_packet, StateData#state.server,
			       allow,
			       [StateData#state.user,
				StateData#state.server,
				StateData#state.privacy_list,
				{From, To, Packet},
				in]) of
			    allow ->
				{true, Attrs, StateData};
			    deny ->
				Err = jlib:make_error_reply(
					Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
				ejabberd_router:route(To, From, Err),
				{false, Attrs, StateData}
			end;
		    _ ->
			{true, Attrs, StateData}
		end;
	    "message" ->
		case ejabberd_hooks:run_fold(
		       privacy_check_packet, StateData#state.server,
		       allow,
		       [StateData#state.user,
			StateData#state.server,
			StateData#state.privacy_list,
			{From, To, Packet},
			in]) of
		    allow ->
			{true, Attrs, StateData};
		    deny ->
			{false, Attrs, StateData}
		end;
	    _ ->
		{true, Attrs, StateData}
	end,
    if
	Pass == exit ->
	    catch send_text(StateData, ?STREAM_TRAILER),
	    {stop, normal, StateData};
	Pass ->
	    Attrs2 = jlib:replace_from_to_attrs(jlib:jid_to_string(From),
						jlib:jid_to_string(To),
						NewAttrs),
	    FixedPacket = {xmlelement, Name, Attrs2, Els},
	    Text = xml:element_to_string(FixedPacket),
	    send_text(StateData, Text),
	    ejabberd_hooks:run(user_receive_packet,
			       StateData#state.server,
			       [StateData#state.jid, From, To, FixedPacket]),
	    {next_state, StateName, NewState};
	true ->
	    {next_state, StateName, NewState}
    end;
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, StateName, StateData) ->
    case StateName of
	session_established ->
	    case StateData#state.authenticated of
		replaced ->
		    ?INFO_MSG("(~w) Replaced session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),
		    From = StateData#state.jid,
		    Packet = {xmlelement, "presence",
			      [{"type", "unavailable"}],
			      [{xmlelement, "status", [],
				[{xmlcdata, "Replaced by new connection"}]}]},
		    ejabberd_sm:close_session_unset_presence(
		      StateData#state.sid,
		      StateData#state.user,
		      StateData#state.server,
		      StateData#state.resource,
		      "Replaced by new connection"),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_a, Packet),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_i, Packet);
		_ ->
		    ?INFO_MSG("(~w) Close session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),

		    EmptySet = ?SETS:new(),
		    case StateData of
			#state{pres_last = undefined,
			       pres_a = EmptySet,
			       pres_i = EmptySet,
			       pres_invis = false} ->
			    ejabberd_sm:close_session(StateData#state.sid,
						      StateData#state.user,
						      StateData#state.server,
						      StateData#state.resource);
			_ ->
			    From = StateData#state.jid,
			    Packet = {xmlelement, "presence",
				      [{"type", "unavailable"}], []},
			    ejabberd_sm:close_session_unset_presence(
			      StateData#state.sid,
			      StateData#state.user,
			      StateData#state.server,
			      StateData#state.resource,
			      ""),
			    presence_broadcast(
			      StateData, From, StateData#state.pres_a, Packet),
			    presence_broadcast(
			      StateData, From, StateData#state.pres_i, Packet)
		    end
	    end;
	_ ->
	    ok
    end,
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

change_shaper(StateData, JID) ->
    Shaper = acl:match_rule(StateData#state.server,
			    StateData#state.shaper, JID),
    (StateData#state.sockmod):change_shaper(StateData#state.socket, Shaper).

send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [lists:flatten(Text)]),
    (StateData#state.sockmod):send(StateData#state.socket, Text).

send_element(StateData, El) ->
    send_text(StateData, xml:element_to_string(El)).


new_id() ->
    randoms:get_string().


is_auth_packet(El) ->
    case jlib:iq_query_info(El) of
	#iq{id = ID, type = Type, xmlns = ?NS_AUTH, sub_el = SubEl} ->
	    {xmlelement, _, _, Els} = SubEl,
	    {auth, ID, Type,
	     get_auth_tags(Els, "", "", "", "")};
	_ ->
	    false
    end.


get_auth_tags([{xmlelement, Name, _Attrs, Els}| L], U, P, D, R) ->
    CData = xml:get_cdata(Els),
    case Name of
	"username" ->
	    get_auth_tags(L, CData, P, D, R);
	"password" ->
	    get_auth_tags(L, U, CData, D, R);
	"digest" ->
	    get_auth_tags(L, U, P, CData, R);
	"resource" ->
	    get_auth_tags(L, U, P, D, CData);
	_ ->
	    get_auth_tags(L, U, P, D, R)
    end;
get_auth_tags([_ | L], U, P, D, R) ->
    get_auth_tags(L, U, P, D, R);
get_auth_tags([], U, P, D, R) ->
    {U, P, D, R}.


process_presence_probe(From, To, StateData) ->
    LFrom = jlib:jid_tolower(From),
    LBFrom = setelement(3, LFrom, ""),
    case StateData#state.pres_last of
	undefined ->
	    ok;
	_ ->
	    Cond1 = (not StateData#state.pres_invis)
		andalso (?SETS:is_element(LFrom, StateData#state.pres_f)
			 orelse
			 ((LFrom /= LBFrom) andalso
			  ?SETS:is_element(LBFrom, StateData#state.pres_f)))
		andalso (not
			 (?SETS:is_element(LFrom, StateData#state.pres_i)
			  orelse
			  ((LFrom /= LBFrom) andalso
			   ?SETS:is_element(LBFrom, StateData#state.pres_i)))),
	    Cond2 = StateData#state.pres_invis
		andalso ?SETS:is_element(LFrom, StateData#state.pres_f)
		andalso ?SETS:is_element(LFrom, StateData#state.pres_a),
	    if
		Cond1 ->
		    Packet = StateData#state.pres_last,
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {To, From, Packet},
			    out]) of
			deny ->
			    ok;
			allow ->
			    ejabberd_router:route(To, From, Packet)
		    end;
		Cond2 ->
		    ejabberd_router:route(To, From,
					  {xmlelement, "presence",
					   [],
					   []});
		true ->
		    ok
	    end
    end.

presence_update(From, Packet, StateData) ->
    {xmlelement, _Name, Attrs, _Els} = Packet,
    case xml:get_attr_s("type", Attrs) of
	"unavailable" ->
	    Status = case xml:get_subtag(Packet, "status") of
			 false ->
			    "";
			 StatusTag ->
			    xml:get_tag_cdata(StatusTag)
		     end,
	    ejabberd_sm:unset_presence(StateData#state.sid,
				       StateData#state.user,
				       StateData#state.server,
				       StateData#state.resource,
				       Status),
	    presence_broadcast(StateData, From, StateData#state.pres_a, Packet),
	    presence_broadcast(StateData, From, StateData#state.pres_i, Packet),
	    StateData#state{pres_last = undefined,
			    pres_a = ?SETS:new(),
			    pres_i = ?SETS:new(),
			    pres_invis = false};
	"invisible" ->
	    NewState =
		if
		    not StateData#state.pres_invis ->
			presence_broadcast(StateData, From,
					   StateData#state.pres_a,
					   Packet),
			presence_broadcast(StateData, From,
					   StateData#state.pres_i,
					   Packet),
			S1 = StateData#state{pres_last = undefined,
					     pres_a = ?SETS:new(),
					     pres_i = ?SETS:new(),
					     pres_invis = true},
			presence_broadcast_first(From, S1, Packet);
		    true ->
			StateData
		end,
	    NewState;
	"error" ->
	    StateData;
	"probe" ->
	    StateData;
	"subscribe" ->
	    StateData;
	"subscribed" ->
	    StateData;
	"unsubscribe" ->
	    StateData;
	"unsubscribed" ->
	    StateData;
	_ ->
	    OldPriority = case StateData#state.pres_last of
			      undefined ->
				  0;
			      OldPresence ->
				  get_priority_from_presence(OldPresence)
			  end,
	    NewPriority = get_priority_from_presence(Packet),
	    update_priority(NewPriority, StateData),
	    FromUnavail = (StateData#state.pres_last == undefined) or
		StateData#state.pres_invis,
	    ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	    NewState =
		if
		    FromUnavail ->
			ejabberd_hooks:run(user_available_hook,
					   StateData#state.server,
					   [StateData#state.jid]),
			if NewPriority >= 0 ->
				resend_offline_messages(StateData),
				resend_subscription_requests(StateData);
			   true ->
				ok
			end,
			presence_broadcast_first(
			  From, StateData#state{pres_last = Packet,
						pres_invis = false
					       }, Packet);
		    true ->
			presence_broadcast_to_trusted(StateData,
						      From,
						      StateData#state.pres_f,
						      StateData#state.pres_a,
						      Packet),
			if OldPriority < 0, NewPriority >= 0 ->
				resend_offline_messages(StateData);
			   true ->
				ok
			end,
			StateData#state{pres_last = Packet,
					pres_invis = false
				       }
		end,
	    NewState
    end.

presence_track(From, To, Packet, StateData) ->
    {xmlelement, _Name, Attrs, _Els} = Packet,
    LTo = jlib:jid_tolower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
    case xml:get_attr_s("type", Attrs) of
	"unavailable" ->
	    ejabberd_router:route(From, To, Packet),
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = remove_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A};
	"invisible" ->
	    ejabberd_router:route(From, To, Packet),
	    I = ?SETS:add_element(LTo, StateData#state.pres_i),
	    A = remove_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A};
	"subscribe" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, subscribe]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"subscribed" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, subscribed]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"unsubscribe" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, unsubscribe]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"unsubscribed" ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, unsubscribed]),
	    ejabberd_router:route(jlib:jid_remove_resource(From), To, Packet),
	    StateData;
	"error" ->
	    ejabberd_router:route(From, To, Packet),
	    StateData;
	"probe" ->
	    ejabberd_router:route(From, To, Packet),
	    StateData;
	_ ->
	    case ejabberd_hooks:run_fold(
		   privacy_check_packet, StateData#state.server,
		   allow,
		   [StateData#state.user,
		    StateData#state.server,
		    StateData#state.privacy_list,
		    {From, To, Packet},
		    out]) of
		deny ->
		    ok;
		allow ->
		    ejabberd_router:route(From, To, Packet)
	    end,
	    I = remove_element(LTo, StateData#state.pres_i),
	    A = ?SETS:add_element(LTo, StateData#state.pres_a),
	    StateData#state{pres_i = I,
			    pres_a = A}
    end.

presence_broadcast(StateData, From, JIDSet, Packet) ->
    lists:foreach(fun(JID) ->
			  FJID = jlib:make_jid(JID),
			  case ejabberd_hooks:run_fold(
				 privacy_check_packet, StateData#state.server,
				 allow,
				 [StateData#state.user,
				  StateData#state.server,
				  StateData#state.privacy_list,
				  {From, FJID, Packet},
				  out]) of
			      deny ->
				  ok;
			      allow ->
				  ejabberd_router:route(From, FJID, Packet)
			  end
		  end, ?SETS:to_list(JIDSet)).

presence_broadcast_to_trusted(StateData, From, T, A, Packet) ->
    lists:foreach(
      fun(JID) ->
	      case ?SETS:is_element(JID, T) of
		  true ->
		      FJID = jlib:make_jid(JID),
		      case ejabberd_hooks:run_fold(
			     privacy_check_packet, StateData#state.server,
			     allow,
			     [StateData#state.user,
			      StateData#state.server,
			      StateData#state.privacy_list,
			      {From, FJID, Packet},
			      out]) of
			  deny ->
			      ok;
			  allow ->
			      ejabberd_router:route(From, FJID, Packet)
		      end;
		  _ ->
		      ok
	      end
      end, ?SETS:to_list(A)).


presence_broadcast_first(From, StateData, Packet) ->
    ?SETS:fold(fun(JID, X) ->
		       ejabberd_router:route(
			 From,
			 jlib:make_jid(JID),
			 {xmlelement, "presence",
			  [{"type", "probe"}],
			  []}),
		       X
	       end,
	       [],
	       StateData#state.pres_t),
    if
	StateData#state.pres_invis ->
	    StateData;
	true ->
	    As = ?SETS:fold(
		    fun(JID, A) ->
			    FJID = jlib:make_jid(JID),
			    case ejabberd_hooks:run_fold(
				   privacy_check_packet, StateData#state.server,
				   allow,
				   [StateData#state.user,
				    StateData#state.server,
				    StateData#state.privacy_list,
				    {From, FJID, Packet},
				    out]) of
				deny ->
				    ok;
				allow ->
				    ejabberd_router:route(From, FJID, Packet)
			    end,
			    ?SETS:add_element(JID, A)
		    end,
		    StateData#state.pres_a,
		    StateData#state.pres_f),
	    StateData#state{pres_a = As}
    end.


remove_element(E, Set) ->
    case ?SETS:is_element(E, Set) of
	true ->
	    ?SETS:del_element(E, Set);
	_ ->
	    Set
    end.


roster_change(IJID, ISubscription, StateData) ->
    LIJID = jlib:jid_tolower(IJID),
    IsFrom = (ISubscription == both) or (ISubscription == from),
    IsTo   = (ISubscription == both) or (ISubscription == to),
    OldIsFrom = ?SETS:is_element(LIJID, StateData#state.pres_f),
    FSet = if
	       IsFrom ->
		   ?SETS:add_element(LIJID, StateData#state.pres_f);
	       true ->
		   remove_element(LIJID, StateData#state.pres_f)
	   end,
    TSet = if
	       IsTo ->
		   ?SETS:add_element(LIJID, StateData#state.pres_t);
	       true ->
		   remove_element(LIJID, StateData#state.pres_t)
	   end,
    case StateData#state.pres_last of
	undefined ->
	    StateData#state{pres_f = FSet, pres_t = TSet};
	P ->
	    ?DEBUG("roster changed for ~p~n", [StateData#state.user]),
	    From = StateData#state.jid,
	    To = jlib:make_jid(IJID),
	    Cond1 = (not StateData#state.pres_invis) and IsFrom
		and (not OldIsFrom),
	    Cond2 = (not IsFrom) and OldIsFrom
		and (?SETS:is_element(LIJID, StateData#state.pres_a) or
		     ?SETS:is_element(LIJID, StateData#state.pres_i)),
	    if
		Cond1 ->
		    ?DEBUG("C1: ~p~n", [LIJID]),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {From, To, P},
			    out]) of
			deny ->
			    ok;
			allow ->
			    ejabberd_router:route(From, To, P)
		    end,
		    A = ?SETS:add_element(LIJID,
					  StateData#state.pres_a),
		    StateData#state{pres_a = A,
				    pres_f = FSet,
				    pres_t = TSet};
		Cond2 ->
		    ?DEBUG("C2: ~p~n", [LIJID]),
		    PU = {xmlelement, "presence",
			  [{"type", "unavailable"}], []},
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {From, To, PU},
			    out]) of
			deny ->
			    ok;
			allow ->
			    ejabberd_router:route(From, To, PU)
		    end,
		    I = remove_element(LIJID,
				       StateData#state.pres_i),
		    A = remove_element(LIJID,
				       StateData#state.pres_a),
		    StateData#state{pres_i = I,
				    pres_a = A,
				    pres_f = FSet,
				    pres_t = TSet};
		true ->
		    StateData#state{pres_f = FSet, pres_t = TSet}
	    end
    end.


update_priority(Pri, StateData) ->
    ejabberd_sm:set_presence(StateData#state.sid,
			     StateData#state.user,
			     StateData#state.server,
			     StateData#state.resource,
			     Pri).

get_priority_from_presence(PresencePacket) ->
    case xml:get_subtag(PresencePacket, "priority") of
	false ->
	    0;
	SubEl ->
	    case catch list_to_integer(xml:get_tag_cdata(SubEl)) of
		P when is_integer(P) ->
		    P;
		_ ->
		    0
	    end
    end.

process_privacy_iq(From, To,
		   #iq{type = Type, sub_el = SubEl} = IQ,
		   StateData) ->
    {Res, NewStateData} =
	case Type of
	    get ->
		R = ejabberd_hooks:run_fold(
		      privacy_iq_get, StateData#state.server,
		      {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
		      [From, To, IQ, StateData#state.privacy_list]),
		{R, StateData};
	    set ->
		case ejabberd_hooks:run_fold(
		       privacy_iq_set, StateData#state.server,
		       {error, ?ERR_FEATURE_NOT_IMPLEMENTED},
		       [From, To, IQ]) of
		    {result, R, NewPrivList} ->
			{{result, R},
			 StateData#state{privacy_list = NewPrivList}};
		    R -> {R, StateData}
		end
	end,
    IQRes =
	case Res of
	    {result, Result} ->
		IQ#iq{type = result, sub_el = Result};
	    {error, Error} ->
		IQ#iq{type = error, sub_el = [SubEl, Error]}
	end,
    ejabberd_router:route(
      To, From, jlib:iq_to_xml(IQRes)),
    NewStateData.


resend_offline_messages(#state{user = User,
			       server = Server,
			       privacy_list = PrivList} = StateData) ->
    case ejabberd_hooks:run_fold(resend_offline_messages_hook,
				 Server,
				 [],
				 [User, Server]) of
	Rs when list(Rs) ->
	    lists:foreach(
	      fun({route,
		   From, To, {xmlelement, Name, Attrs, Els} = Packet}) ->
		      Pass = case ejabberd_hooks:run_fold(
				    privacy_check_packet, Server,
				    allow,
				    [User,
				     Server,
				     PrivList,
				     {From, To, Packet},
				     in]) of
				 allow ->
				     true;
				 deny ->
				     false
			     end,
		      if
			  Pass ->
			      Attrs2 = jlib:replace_from_to_attrs(
					 jlib:jid_to_string(From),
					 jlib:jid_to_string(To),
					 Attrs),
			      send_element(StateData,
					   {xmlelement, Name, Attrs2, Els});
			  true ->
			      ok
		      end
	      end, Rs)
    end.

resend_subscription_requests(#state{user = User,
				    server = Server} = StateData) ->
    PendingSubscriptions = ejabberd_hooks:run_fold(
			     resend_subscription_requests_hook,
			     Server,
			     [],
			     [User, Server]),
    lists:foreach(fun(XMLPacket) ->
			  send_element(StateData,
				       XMLPacket)
		  end,
		  PendingSubscriptions).

get_showtag(undefined) ->
    "unavailable";
get_showtag(Presence) ->
    case xml:get_path_s(Presence, [{elem, "show"}, cdata]) of
	""      -> "available";
	ShowTag -> ShowTag
    end.

get_statustag(undefined) ->
    "";
get_statustag(Presence) ->
    case xml:get_path_s(Presence, [{elem, "status"}, cdata]) of
	ShowTag -> ShowTag
    end.

process_unauthenticated_stanza(StateData, El) ->
    case jlib:iq_query_info(El) of
	#iq{} = IQ ->
	    Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					  StateData#state.server,
					  empty,
					  [StateData#state.server, IQ]),
	    case Res of
		empty ->
		    % The only reasonable IQ's here are auth and register IQ's
		    % They contain secrets, so don't include subelements to response
		    ResIQ = IQ#iq{type = error,
				  sub_el = [?ERR_SERVICE_UNAVAILABLE]},
		    Res1 = jlib:replace_from_to(
			     jlib:make_jid("", StateData#state.server, ""),
			     jlib:make_jid("", "", ""),
			     jlib:iq_to_xml(ResIQ)),
		    send_element(StateData, jlib:remove_attr("to", Res1));
		_ ->                    
		    send_element(StateData, Res)
	    end;
	_ ->
	    % Drop any stanza, which isn't IQ stanza
	    ok
    end.


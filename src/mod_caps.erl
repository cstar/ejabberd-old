%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2007   Process-one
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

-module(mod_caps).
-author('henoch@dtek.chalmers.se').

-behaviour(gen_server).
-behaviour(gen_mod).

-export([read_caps/1,
	 note_caps/3,
	 get_features/2,
	 handle_disco_response/3]).

%% gen_mod callbacks
-export([start/2, start_link/2,
	 stop/1]).

%% gen_server callbacks
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_caps).
-define(DICT, dict).

-record(caps, {node, version, exts}).
-record(caps_features, {node_pair, features}).
-record(state, {host,
		disco_requests = ?DICT:new(),
		feature_queries = []}).

%% read_caps takes a list of XML elements (the child elements of a
%% <presence/> stanza) and returns an opaque value representing the
%% Entity Capabilities contained therein, or the atom nothing if no
%% capabilities are advertised.
read_caps(Els) ->
    read_caps(Els, nothing).
read_caps([{xmlelement, "c", Attrs, _Els} | Tail], Result) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_CAPS ->
	    Node = xml:get_attr_s("node", Attrs),
	    Version = xml:get_attr_s("ver", Attrs),
	    Exts = string:tokens(xml:get_attr_s("ext", Attrs), " "),
	    read_caps(Tail, #caps{node = Node, version = Version, exts = Exts});
	_ ->
	    read_caps(Tail, Result)
    end;
read_caps([{xmlelement, "x", Attrs, _Els} | Tail], Result) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_MUC_USER ->
	    nothing;
	_ ->
	    read_caps(Tail, Result)
    end;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) ->
    Result.

%% note_caps should be called to make the module request disco
%% information.  Host is the host that asks, From is the full JID that
%% sent the caps packet, and Caps is what read_caps returned.
note_caps(Host, From, Caps) ->
    case Caps of
	nothing -> ok;
	_ ->
	    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	    gen_server:cast(Proc, {note_caps, From, Caps})
    end.

%% get_features returns a list of features implied by the given caps
%% record (as extracted by read_caps).  It may block, and may signal a
%% timeout error.
get_features(Host, Caps) ->
    case Caps of
	nothing -> [];
	#caps{} ->
	    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	    gen_server:call(Proc, {get_features, Caps})
    end.

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 transient,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:stop_child(ejabberd_sup, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, _Opts]) ->
    mnesia:create_table(caps_features,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, caps_features)}]),
    mnesia:add_table_copy(caps_features, node(), ram_copies),
    {ok, #state{host = Host}}.

maybe_get_features(#caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
    F = fun() ->
		%% Make sure that we have all nodes we need to know.
		%% If a single one is missing, we wait for more disco
		%% responses.
		lists:foldl(fun(SubNode, Acc) ->
				    case Acc of
					fail -> fail;
					_ ->
					    case mnesia:read({caps_features, {Node, SubNode}}) of
						[] -> fail;
						[#caps_features{features = Features}] -> Features ++ Acc
					    end
				    end
			    end, [], SubNodes)
	end,
    case mnesia:transaction(F) of
	{atomic, fail} ->
	    wait;
	{atomic, Features} ->
	    {ok, Features}
    end.

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.

handle_call({get_features, Caps}, From, State) ->
    case maybe_get_features(Caps) of
	{ok, Features} -> 
	    {reply, Features, State};
	wait ->
	    gen_server:cast(self(), visit_feature_queries),
	    Timeout = timestamp() + 10,
	    FeatureQueries = State#state.feature_queries,
	    NewFeatureQueries = [{From, Caps, Timeout} | FeatureQueries],
	    NewState = State#state{feature_queries = NewFeatureQueries},
	    {noreply, NewState}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({note_caps, From, 
	     #caps{node = Node, version = Version, exts = Exts}}, 
	    #state{host = Host, disco_requests = Requests} = State) ->
    %% XXX: this leads to race conditions where ejabberd will send
    %% lots of caps disco requests.
    SubNodes = [Version | Exts],
    %% Now, find which of these are not already in the database.
    Fun = fun() ->
		  lists:foldl(fun(SubNode, Acc) ->
				      case mnesia:read({caps_features, {Node, SubNode}}) of
					  [] ->
					      [SubNode | Acc];
					  _ ->
					      Acc
				      end
			      end, [], SubNodes)
	  end,
    case mnesia:transaction(Fun) of
	{atomic, Missing} ->
	    %% For each unknown caps "subnode", we send a disco
	    %% request.
	    NewRequests =
		lists:foldl(
		  fun(SubNode, Dict) ->
			  ID = randoms:get_string(),
			  Stanza =
			      {xmlelement, "iq",
			       [{"type", "get"},
				{"id", ID}],
			       [{xmlelement, "query",
				 [{"xmlns", ?NS_DISCO_INFO}],
				 []}]},
			  ejabberd_local:register_iq_response_handler
			    (Host, ID, ?MODULE, handle_disco_response),
			  ejabberd_router:route(jlib:make_jid("", Host, ""), From, Stanza),
			  ?DICT:store(ID, {Node, SubNode}, Dict)
		  end, Requests, Missing),
	    {noreply, State#state{disco_requests = NewRequests}};
	Error ->
	    ?ERROR_MSG("Transaction failed: ~p", [Error]),
	    {noreply, State}
    end;
handle_cast({disco_response, From, _To, 
	     #iq{type = Type, id = ID,
		 sub_el = SubEls}},
	    #state{disco_requests = Requests} = State) ->
    case {Type, SubEls} of
	{result, [{xmlelement, "query", _Attrs, Els}]} ->
	    case ?DICT:find(ID, Requests) of
		{ok, {Node, SubNode}} ->
		    Features =
			lists:flatmap(fun({xmlelement, "feature", FAttrs, _}) ->
					      [xml:get_attr_s("var", FAttrs)];
					 (_) ->
					      []
				      end, Els),
		    mnesia:transaction(
		      fun() ->
			      mnesia:write(#caps_features{node_pair = {Node, SubNode},
							  features = Features})
		      end),
		    gen_server:cast(self(), visit_feature_queries);
		error ->
		    ?ERROR_MSG("ID '~s' matches no query", [ID])
	    end;
	{error, _} ->
	    gen_server:cast(self(), visit_feature_queries),
	    ?ERROR_MSG("Error IQ reponse IQ from ~s: ~p", [jlib:jid_to_string(From), SubEls]);
	{result, _} ->
	    ?ERROR_MSG("Invalid IQ contents from ~s: ~p", [jlib:jid_to_string(From), SubEls]);
	_ ->
	    %% Can't do anything about errors
	    ok
    end,
    NewRequests = ?DICT:erase(ID, Requests),
    {noreply, State#state{disco_requests = NewRequests}};
handle_cast(visit_feature_queries, #state{feature_queries = FeatureQueries} = State) ->
    Timestamp = timestamp(),
    NewFeatureQueries =
	lists:foldl(fun({From, Caps, Timeout}, Acc) ->
			    case maybe_get_features(Caps) of
				wait when Timeout > Timestamp -> [{From, Caps, Timeout} | Acc];
				wait -> Acc;
				{ok, Features} ->
				    gen_server:reply(From, Features),
				    Acc
			    end
		    end, [], FeatureQueries),
    {noreply, State#state{feature_queries = NewFeatureQueries}}.

handle_disco_response(From, To, IQ) ->
    #jid{lserver = Host} = To,
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {disco_response, From, To, IQ}).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

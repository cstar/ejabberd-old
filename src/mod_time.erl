%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_time).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 process_local_iq/3]).

-include("ejabberd.hrl").
-include("namespaces.hrl").




start() ->
    ejabberd_local:register_iq_handler(?NS_TIME,
				       ?MODULE, process_local_iq).



process_local_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS,
	     [SubEl, {xmlelement, "error",
		      [{"code", "405"}],
		      [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    UTC = jlib:timestamp_to_iso(calendar:universal_time()),
	    {iq, ID, result, XMLNS,
	     [{xmlelement, "query",
	       [{"xmlns", ?NS_TIME}],
	       [{xmlelement, "utc", [],
		 [{xmlcdata, UTC}]}
	       ]}]}
    end.



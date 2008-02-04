%%%----------------------------------------------------------------------
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

%-define(ejabberd_debug, true).
%-define(DBGFSM, true).

-define(VERSION, "2.1.0-alpha").

%% ---------------------------------
%% Logging mechanism

-define(DEBUG(Format, Args),
    ejabberd_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    ejabberd_logger:info_msg(?MODULE,?LINE,Format, Args)).
			      
-define(WARNING_MSG(Format, Args),
    ejabberd_logger:warning_msg(?MODULE,?LINE,Format, Args)).
			      
-define(ERROR_MSG(Format, Args),
    ejabberd_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    ejabberd_logger:critical_msg(?MODULE,?LINE,Format, Args)).

-define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
-define(MYNAME, hd(ejabberd_config:get_global_option(hosts))).
-define(S2STIMEOUT, 600000).
-define(MYLANG, ejabberd_config:get_global_option(language)).

-define(MSGS_DIR, "msgs").
-define(CONFIG_PATH, "ejabberd.cfg").
-define(LOG_PATH, "ejabberd.log").

-define(PRIVACY_SUPPORT, true).

-define(EJABBERD_URI, "http://www.process-one.net/en/ejabberd/").

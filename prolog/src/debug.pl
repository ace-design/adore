%%%%
%% This file is part of ADORE [ www.adore-design.org ]
%%
%% Copyright (C) 2008-  Sebastien Mosser
%%
%% ADORE is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% ADORE is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with jSeduite:DataCache; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author      Main Sébastien Mosser          [mosser@polytech.unice.fr]
%%%%

:- dynamic debugSubscription/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available DEBUG channel declaration %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- debug(debug,_,_).
:- debug(def,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rule to activate a channel %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- debug(debug). %% Debugging the debug functionalities ... ^_^
% Subscribe the engine for messages on a given channel
activateDebug(Channel) :- 
	debug(debug,' * Activating debug channel #~w',Channel),
	call(debug(Channel)).
	

%% Activate debug for all declared channel
performDebugSubscription :- findall(X, debugSubscription(X), Channels),
	Channels = [], true, !.

performDebugSubscription :- findall(X, debugSubscription(X), Channels),
	\+ Channels = [],
	debug(debug,' *** Debug Subscription started',_),
	maplist(activateDebug,Channels), 
	debug(debug,' *** done !',_),
	list_debug_topics.


%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debugging framework %%
%%%%%%%%%%%%%%%%%%%%%%%%%

debugLevel(failure,'FAIL').
debugLevel(info,'INFO').
debugLevel(warning,'WARN').

dlog(_,L,_,_) :- \+ debugLevel(L,_), true, !.
dlog(Chan, Lvl, Msg, Args) :- debugLevel(Lvl, LvlMsg), 
	string_concat(LvlMsg,': ',Tmp),
	string_concat(Tmp,Msg, Final),
	debug(Chan,Final, Args).
	

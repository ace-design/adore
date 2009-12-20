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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rule to activate a channel %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- debug(debug). %% Debugging the debug functionalities ... ^_^
	
%% Activate debug for all declared channel
performDebugSubscription :- 
	findall(X, debugSubscription(X), Channels),
	Channels == [], !.

performDebugSubscription :- 
	findall(X, debugSubscription(X), Channels),
	maplist(adore_debug,Channels).

adore_debug(C) :- 
	swritef(Tmp,'% Listening debug channel %w',[C]),
	aw(Tmp), debug(C).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debugging framework %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% Public interface for using debug functionalities
dinfo(Channel,Msg,Args) :- dlog(Channel,info,Msg,Args).
dwarn(Channel,Msg,Args) :- dlog(Channel,warning,Msg,Args).
dfail(Channel,Msg,Args) :- dlog(Channel,failure,Msg,Args), fail.

%% Do not use directly (biohazard or assimilated!)
debugLevel(failure,'FAIL').
debugLevel(info,'INFO').
debugLevel(warning,'WARN').

dlog(_,_,_,_) :- adore_silent(true), true, !.
dlog(_,L,_,_) :- \+ debugLevel(L,_), true, !.
dlog(C,_,_,_) :- \+ debugSubscription(C), true, !.
dlog(Chan, Lvl, Msg, Args) :- 
	debugLevel(Lvl, LvlMsg), 
	string_concat(LvlMsg,': ',Tmp),
	string_concat(Tmp,Msg, Final),
	debug(Chan,Final, Args).
	


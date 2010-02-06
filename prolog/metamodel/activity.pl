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

:- module(activity,[]).

belongsTo(Act,P) :- 
	activity(Act), isContainedBy(Act,P), process(P).


useVariable(Act,Var) :- 
	useVariable(Act,Var,_).
useVariable(Act,Var,in) :- 
	activity(Act), variable(Var), 
	( usesAsInput(Act,Var) | usesAsInput(Act,F), fieldAccess(F,Var,_) ).
useVariable(Act,Var,out) :- 
	activity(Act), variable(Var), 
	( usesAsOutput(Act,Var) | usesAsOutput(Act,F), fieldAccess(F,Var,_) ).

%%%
% Activity Set Predicates
%%%

areInSameProcess(X,Y) :- belongsTo(X,P), belongsTo(Y,P).

isWellFormed(ActSet) :- 
	maplist(activity:belongsTo,ActSet,ProcessSet),
	sort(ProcessSet,[_]).

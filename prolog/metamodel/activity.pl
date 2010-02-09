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

areInSameProcess(X,Y) :- belongsTo(X,P), belongsTo(Y,P).


getPredecessors(A,Preds) :- 
	findall(X,path(X,A),Preds).

getSuccessors(A,Succs) :- 
	findall(X,path(A,X),Succs).

%%%
% Activity Set Predicates
%%%

isWellFormed(ActSet) :- isWellFormed(ActSet,_).
isWellFormed(ActSet,P) :- 
	maplist(activity:belongsTo,ActSet,ProcessSet),
	sort(ProcessSet,[P]).

hasSameKind(ActSet) :- hasSameKind(ActSet,_).
hasSameKind(ActSet,Kind) :- 
	maplist(hasForKind,ActSet,Kinds), sort(Kinds,[Kind]).

filterByKind([],_,[]).
filterByKind([H|T],K,[H|O]) :- 
	hasForKind(H,K), !, filterByKind(T,K,O).
filterByKind([_|T],K,O) :- 
	filterByKind(T,K,O).

areUnifiable(ActSet,Kind,Process) :- 
	isWellFormed(ActSet,Process), hasSameKind(ActSet,Kind).


getFirsts(Block,Activities) :- 
	findall(A,activity:isFirst(Block,A),Tmp), sort(Tmp,Activities).
isFirst(Block,Activity) :- 
	member(Activity,Block), \+ path(_,Activity).
isFirst(Block,Activity) :- 
	member(Activity,Block), path(APrime,Activity), \+ member(APrime,Block).

getLasts(Block,Activities) :- 
	findall(A,activity:isLast(Block,A),Tmp), sort(Tmp,Activities).
isLast(Block,Activity) :- 
	member(Activity,Block),	\+ path(Activity,_).
isLast(Block,Activity) :- 
	member(Activity,Block), path(Activity,APrime),
	\+ member(APrime,Block). 


isBlockInterfaceVariable(Block,V,in) :- 
	isFirstActivity(Block,A), usesElemAsInput(A,V), \+ isConstant(V).
isBlockInterfaceVariable(Block,V,out) :- 
	isLastActivity(Block,A), usesElemAsOutput(A,V).

%% TO DO: harmonize with isBliockInterface predicates.
getBlockInputVariables(Block, Vars) :-
	findall(V,activity:isBlockInputVariable(Block,V),Tmp),
	sort(Tmp,Vars).
isBlockInputVariable(Block,V) :- 
	isFirstActivity(Block,A), usesElemAsInput(A,V), \+ isConstant(V).

getBlockOutputVariable(Block, Vars) :-
	findall(V,activity:isBlockOutputVariable(Block,V),Tmp),
	sort(Tmp,Vars).
isBlockOutputVariable(Block,V) :- 
	isLastActivity(Block,A), usesElemAsOutput(A,V).




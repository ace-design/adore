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

%%%%
%% Process Existence
%%%%

isProcess(P) :- process(P),!.
isProcess(P) :- context(C), contextOutput(C,P),!.


%%%%
%% Relations between activities
%%%%

sameProcess(X,Y) :- isContainedBy(X,P), isContainedBy(Y,P).

%% path/2: path(+A,+B) => a direct path exists between A and B
%path(X,X) :- fail.
path(X,Y) :- sameProcess(X,Y), waitFor(Y,X).
path(X,Y) :- sameProcess(X,Y), isGuardedBy(Y,X,_,_).
path(X,Y) :- sameProcess(X,Y), weakWait(Y,X).
path(X,Y) :- sameProcess(X,Y), onFailure(Y,X,_).

%% existsPath/2: existsPath(+A,+B) => transitive closure for path
existsPath(X,Y) :- path(X,Y).
existsPath(X,Y) :- path(X,Z), existsPath(Z,Y).

%% getPath/3: getPath(+A,+B,-Path) => return activities from A to B
getPath(A,B,[A|O]) :- extractPath(A,B,O). 
extractPath(A,B,[B]) :- path(A,B).
extractPath(A,B,[X|O]) :- path(A,X), extractPath(X,B,O).

%%%%
%% Access to variable
%%%%

%% getVariable/2: getVariable(+V,-R): retrieve the variable associated to V
getVariable(F,V) :- fieldAccess(F,V,_). %% as it can be a field access
getVariable(V,V) :- variable(V).        %% or simply the variable.

%% usesElemAsInput(?A,?V): A uses V (or an associated field) as input
usesElemAsInput(A,V) :- usesAsInput(A,V).
usesElemAsInput(A,V) :- usesAsInput(A,F), fieldAccess(F,V,_).

%% usesElemAsOuutput(?A,?V): A uses V (or an associated field) as output
usesElemAsOutput(A,V) :- usesAsOutput(A,V).
usesElemAsOutput(A,V) :- usesAsOutput(A,F), fieldAccess(F,V,_).

%% usesElem(?A,?V): A uses V as input or output
usesElem(A,V) :- usesElemAsInput(A,V).
usesElem(A,V) :- usesElemAsOutput(A,V).

usedByProcess(P,V) :- 
	isContainedBy(A,P), activity(A), usesElem(A,V).

%%%%
%% Block Handling
%%%%

getFirstActivitiesOfBlock(Block,Activities) :- 
	findall(A,isFirstActivity(Block,A),Tmp), sort(Tmp,Activities).
isFirstActivity(Block,Activity) :- 
	member(Activity,Block), \+ path(_,Activity).
isFirstActivity(Block,Activity) :- 
	member(Activity,Block), path(APrime,Activity), \+ member(APrime,Block).

getLastActivitiesOfBlock(Block,Activities) :- 
	findall(A,isLastActivity(Block,A),Tmp), sort(Tmp,Activities).
isLastActivity(Block,Activity) :- 
	member(Activity,Block),	\+ path(Activity,_).
isLastActivity(Block,Activity) :- 
	member(Activity,Block), path(Activity,APrime),
	\+ member(APrime,Block). 

getBlockInputVariable(Block, Vars) :-
	findall(V,isBlockInputVariable(Block,V),Tmp),
	sort(Tmp,Vars).
isBlockInputVariable(Block,V) :- 
	isFirstActivity(Block,A), usesElemAsInput(A,V), \+ isConstant(V).

getBlockOutputVariable(Block, Vars) :-
	findall(V,isBlockOutputVariable(Block,V),Tmp),
	sort(Tmp,Vars).
isBlockOutputVariable(Block,V) :- 
	isLastActivity(Block,A), usesElemAsOutput(A,V).

isWellFormed(Block,P) :- 
	activityBlock(_,Block,Activities),
	map(isContainedBy,Activities,Processes),
	sort(Processes,[P]),!.

%%%%
%% Process entry and exit points
%%%%

%% isProcessEntryPoint(?P,?A): A is an entry of P (receive|pred)
isProcessEntryPoint(P,A) :- 
	process(P), isContainedBy(A,P), activity(A), hasForKind(A,receive).
isProcessEntryPoint(F,A) :- 
	process(F), isFragment(F), isContainedBy(A,F), activity(A), 
	hasForKind(A,predecessors).

%% isProcessExitPoint(?P,?A): A is an exit of P (reply|throw|succ)
isProcessExitPoint(P,A) :-
	process(P), isContainedBy(A,P), activity(A), hasForKind(A,reply).
isProcessExitPoint(P,A) :-
	process(P), isContainedBy(A,P), activity(A), hasForKind(A,throw).
isProcessExitPoint(F,A) :-
	process(F), isFragment(F), isContainedBy(A,F), activity(A), 
	hasForKind(A,successors).

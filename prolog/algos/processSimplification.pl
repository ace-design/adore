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
:- module( processSimplification, 
	   [ doProcessSimplification/1, delTransitivePaths/2 ]).

%%%%%%
%%% end user interface
%%%%%%

doProcessSimplification(Process) :- 
	dinfo(algo,'doProcessSimplification(~w) ...',[Process]),
	myTimer(processSimplification:buildActions(Process,Actions)),
	myTimer(executeActionSet(Actions)),!,
	length(Actions,LActs),
	dinfo(algo,'... done (~w actions).',[LActs]).

buildActions(Process, Actions) :- 
 	process:exists(Process), process:getActivities(Process,Acts),
	findall(A,processSimplification:simplify(Acts,A),SplActs),
	flatten([ SplActs, delTransitivePaths(Process)], Actions).

simplify(Activities,myRetract(waitFor(X,Y))) :- 
	%% waitFor is absorbed 
	member(X, Activities), member(Y, Activities),
	waitFor(X,Y), (isGuardedBy(X,Y,_,_)| weakWait(X,Y)).


%%% TO DO: check correctness on the CCCMS (ouch ...)
simplify(Activities,myRetract(waitFor(X,Z))) :- 
	%% waitFor is absorbed when guard is defined
	member(X, Activities), member(Y, Activities), member(Z, Activities),
	isGuardedBy(X,Y,_,_), waitFor(X,Z),
	relations:existsControlPath(Z,Y).

simplify(Activities,myRetract(waitFor(X,Y))) :- 
	member(X,Activities), member(Y, Activities),
	relations:getControlPath(Y,X,L), L \= [Y,X].
%% Enf of TODO

simplify(Activities,[myRetract(weakWait(X,Y)),defWaitFor(X,Y)]) :- 
	%% just a SINGLE weak wait <==> normal waitFor
	member(X, Activities), 
	findall(P,weakWait(X,P),[Y]). 

simplify(Activities, Actions) :- %% Use self as invoked service, in a process
	member(X,Activities), activity:belongsTo(X,Process), 
	\+ isFragment(Process), hasForKind(X,invoke),
	hasForService(X,self), hasForSrvName(Process,Service),
	Actions = [ retract(hasForService(X,self)), 
	            setInvokedService(X,Service)].

simplify(Activities, Actions) :- %% Use self as invoked operation, in a process
	member(X,Activities), activity:belongsTo(X,Process), 
	\+ isFragment(Process), hasForKind(X,invoke),
	hasForOperation(X,self), hasForOpName(Process,Operation),
	Actions = [ retract(hasForOperation(X,self)), 
	            setInvokedOperation(X,Operation)].

%%%%%
%% Macro Actions
%%%%%

:- assert(user:isMacroAction(delTransitivePaths,2)).
delTransitivePaths(Process,Actions) :-
	findall(A, processSimplification:delATransitivePath(Process,A),Raw),
	flatten(Raw,Actions), cleanup.

delATransitivePath(Process,Action) :- 
	%% a < b  AND a < ... < b (< : wait, weak or guard) => retract a < b
	process:getActivities(Process,Activities),
 	member(X, Activities), member(Y, Activities),
 	waitFor(Y,X), % old: |weakWait(Y,X)  TODO: is that ALWAYS true?
 	relations:getControlPath(X,Y,L), \+ L = [X,Y], 
	getLazyGuards(X,GuardsX), getLazyGuards(Y,GuardsY),
	GuardsX = GuardsY, % X and Y under the same conditions (only)
 	relations:delAPath(X,Y,Action).

:-dynamic simplifyTmpGuards/2. %% To improve performance!!!!
cleanup :- 
	retractall(simplifyTmpGuards(_,_)).
getLazyGuards(Act,Guards) :- simplifyTmpGuards(Act,Guards),!.
getLazyGuards(Act,Guards) :- 
	activity:getTransitiveGuards(Act,Guards), !,
	assert(simplifyTmpGuards(Act,Guards)).


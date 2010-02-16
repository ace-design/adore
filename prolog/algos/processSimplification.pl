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
	   [ doProcessSimplification/1, delTransitivePaths/2]).

%%%%%%
%%% end user interface
%%%%%%

doProcessSimplification(Process) :- 
%	dinfo(algo,'Running doProcessSimplification(~w)',[Process]),
%	dinfo(algo,'  Computing action set',[]),
	myTimer(processSimplification:buildActions(Process,Actions)),
%	length(Actions,LActions), 
%	dinfo(algo,'  => Result: ~w actions',[LActions]),
%	dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(Actions)),!.
%	dinfo(algo,'doProcessSimplification(~w) ended with success!',[Process]).

buildActions(Process, Actions) :- 
 	process:exists(Process), process:getActivities(Process,Acts),
	findall(A,processSimplification:simplify(Acts,A),SplActs),
	flatten([ SplActs, delTransitivePaths(Process)], Actions).

simplify(Activities,myRetract(waitFor(X,Y))) :- 
	%% waitFor is absorbed 
	member(X, Activities), member(Y, Activities),
	waitFor(X,Y), (isGuardedBy(X,Y,_,_)| weakWait(X,Y)).

simplify(Activities,[myRetract(weakWait(X,Y)),defWaitFor(X,Y)]) :- 
	%% just a SINGLE weak wait <==> normal waitFor
	member(X, Activities), 
	findall(P,weakWait(X,P),[Y]). 

%%%%%
%% Macro Actions
%%%%%

:- assert(user:isMacroAction(delTransitivePaths,2)).
delTransitivePaths(Process,Actions) :-
	findall(A, processSimplification:delATransitivePath(Process,A),Raw),
	flatten(Raw,Actions).

delATransitivePath(Process,Action) :- 
	%% a < b  AND a < ... < b (< : wait, weak or guard) => retract a < b
	process:getActivities(Process,Activities),
 	member(X, Activities), member(Y, Activities),
 	waitFor(Y,X), % old: |weakWait(Y,X)  TODO: is that ALWAYS true?
 	relations:getControlPath(X,Y,L), \+ L = [X,Y], 
 	relations:delAPath(X,Y,Action).

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
%% @author      Main Sebastien Mosser          [mosser@polytech.unice.fr]
%%%%

:- module(relations,[shiftAPath/4]).

%% path/2: path(+A,+B) => a direct path exists between A and B
%path(X,X) :- fail.
path(X,Y) :- activity:sameProcess(X,Y), waitFor(Y,X).
path(X,Y) :- activity:sameProcess(X,Y), isGuardedBy(Y,X,_,_).
path(X,Y) :- activity:sameProcess(X,Y), weakWait(Y,X).
path(X,Y) :- activity:sameProcess(X,Y), onFailure(Y,X,_).

%% controlPath (not error handling).
controlPath(X,Y) :- 
	path(X,Y), \+ onFailure(Y,X,_).

%% existsPath/2: existsPath(+A,+B) => transitive closure for path
existsPath(X,Y) :- path(X,Y).
existsPath(X,Y) :- 
	activity:areInSameProcess(X,Z), activity:areInSameProcess(Z,Y),
	Z \= X, Z \= Y,	path(X,Z), existsPath(Z,Y).

%% getPath/3: getPath(+A,+B,-Path) => return activities from A to B
getPath(A,B,[A|O]) :- extractPath(A,B,O). 
extractPath(A,B,[B]) :- path(A,B).
extractPath(A,B,[X|O]) :- path(A,X), 
	
	extractPath(X,B,O).

%% contreol path (not onFail) (appearently dead code ... weird)
getControlPath(A,B,[A|O]) :- extractControlPath(A,B,O). 
extractControlPath(A,B,[B]) :- controlPath(A,B).
extractControlPath(A,B,[X|O]) :- controlPath(A,X), extractControlPath(X,B,O).


existsControlPath(X,Y) :- controlPath(X,Y).
existsControlPath(X,Y) :- 
	activity:areInSameProcess(X,Z), activity:areInSameProcess(Z,Y),
	Z \= X, Z \= Y, 
%	writef("x := %w, y := %w, z := %w\n",[X,Y,Z]),
	controlPath(X,Z), existsControlPath(Z,Y).

getGuardPath(A,X,[guard(V,C)]) :- 
	activity:sameProcess(X,A), isGuardedBy(X,A,V,C).
getGuardPath(A,X,[]) :- path(A,X), \+ isGuardedBy(X,A,_,_).
getGuardPath(A,X,Guards) :- 
	path(A,Tmp), \+ isGuardedBy(Tmp,A,_,_), getGuardPath(Tmp,X,Guards).
getGuardPath(A,X,[guard(V,C)|Others]) :- 
	path(A,Tmp), isGuardedBy(Tmp,A,V,C), getGuardPath(Tmp,X,Others).

%% deleting a path
delAPath(Act,A) :- delAPath(Act,_,A).
delAPath(Act,X,myRetract(waitFor(Act,X))) :- waitFor(Act,X).
delAPath(Act,X,myRetract(waitFor(X,Act))) :- waitFor(X,Act).
delAPath(Act,X,myRetract(weakWait(Act,X))) :- weakWait(Act,X).
delAPath(Act,X,myRetract(weakWait(X,Act))) :- weakWait(X,Act).
delAPath(Act,X,myRetract(isGuardedBy(Act,X,V,C))) :- isGuardedBy(Act,X,V,C).
delAPath(Act,X,myRetract(isGuardedBy(X,Act,V,C))) :- isGuardedBy(X,Act,V,C).
delAPath(Act,X,myRetract(onFailure(Act,X,E))) :- onFailure(Act,X,E).
delAPath(Act,X,myRetract(onFailure(X,Act,E))) :- onFailure(X,Act,E).

%% shifting a path from an activity to another one
:- assert(user:isMacroAction(shiftAPath,4)).
shiftAPath(Act,NAct,A) :- shiftAPath(Act,NAct,_,A).
shiftAPath(Act,NAct,X,[myRetract(waitFor(Act,X)), defWaitFor(NAct,X)]) :- 
	waitFor(Act,X).
shiftAPath(Act,NAct,X,[myRetract(waitFor(X,Act)), defWaitFor(X,NAct)]) :- 
	waitFor(X,Act).
shiftAPath(Act,NAct,X,[myRetract(weakWait(Act,X)), defWeakWait(NAct,X)]) :- 
	weakWait(Act,X).
shiftAPath(Act,NAct,X,[myRetract(weakWait(X,Act)), defWeakWait(X,NAct)]) :- 
	weakWait(X,Act).
shiftAPath(Act,NAct,X,Actions) :- 
	isGuardedBy(Act,X,V,C),
	Actions = [myRetract(isGuardedBy(Act,X,V,C)),defGuard(NAct,X,V,C)].
shiftAPath(Act,NAct,X,Actions) :- 
	isGuardedBy(X,Act,V,C),
	Actions = [myRetract(isGuardedBy(X,Act,V,C)),defGuard(X,NAct,V,C)].
shiftAPath(Act,NAct,X,[myRetract(onFailure(Act,X,E)),defOnFail(NAct,X,E)]) :- 
	onFailure(Act,X,E).
shiftAPath(Act,NAct,X,[myRetract(onFailure(X,Act,E)),defOnFail(X,NAct,E)]) :- 
	onFailure(X,Act,E).
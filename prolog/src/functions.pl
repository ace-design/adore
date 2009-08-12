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
%% Relations between activities
%%%%

%% path/2: path(+A,+B) => a direct path exists between A and B
path(X,Y) :- waitFor(Y,X).
path(X,Y) :- isGuardedBy(Y,X,_,_).
path(X,Y) :- weakWait(Y,X).
path(X,Y) :- onFailure(Y,X,_).

%% existsPath/2: existsPath(+A,+B) => transitive closure for path
existsPath(X,Y) :- path(X,Y).
existsPath(X,Y) :- path(X,Z), existsPath(Z,Y).


%%%%
%% Access to variable
%%%%

%% getVariable/2: getVariable(+V,-R): retrieve the variable associated to V
getVariable(F,V) :- fieldAccess(F,V,_). %% as it can be a field access
getVariable(V,V) :- variable(V).        %% or simply the variable.


%usesVariable(A,V) :- usesAsInput(A,V).
%usesVariable(A,V) :- usesAsOutput(A,V).


%%%%
%% Block Handling
%%%%

% dataflow(peanoMachine_add_a,L), last(A,L).
lastElement(Block,A) :- 
	activity(A),  member(A,Block), \+ path(A,_).
lastElement(Block,A) :- 
	activity(A), member(A,Block),
	activity(APrime), path(A,APrime), \+ member(APrime,Block).

firstElement(Block,A) :- 
	activity(A),  member(A,Block), \+ path(_,A).
firstElement(Block,A) :- 
	activity(A), member(A,Block),
	activity(APrime), path(APrime,A), \+ member(APrime,Block).

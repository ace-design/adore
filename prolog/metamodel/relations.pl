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

:- module(relations,[]).

%% path/2: path(+A,+B) => a direct path exists between A and B
%path(X,X) :- fail.
path(X,Y) :- activity:sameProcess(X,Y), waitFor(Y,X).
path(X,Y) :- activity:sameProcess(X,Y), isGuardedBy(Y,X,_,_).
path(X,Y) :- activity:sameProcess(X,Y), weakWait(Y,X).
path(X,Y) :- activity:sameProcess(X,Y), onFailure(Y,X,_).

%% existsPath/2: existsPath(+A,+B) => transitive closure for path
existsPath(X,Y) :- path(X,Y).
existsPath(X,Y) :- path(X,Z), existsPath(Z,Y).

%% getPath/3: getPath(+A,+B,-Path) => return activities from A to B
getPath(A,B,[A|O]) :- extractPath(A,B,O). 
extractPath(A,B,[B]) :- path(A,B).
extractPath(A,B,[X|O]) :- path(A,X), extractPath(X,B,O).







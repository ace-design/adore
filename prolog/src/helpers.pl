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

%% concatenate/2: transform a list of strings into a separated string
concatenate(L,C) :- concatenate(L,C,'\n').
concatenate([],'',_).
concatenate([E],R,_) :- swritef(R,'%w',[E]),!.
concatenate([''|T],R,Sep) :- !, concatenate(T,R,Sep).
concatenate([H|T],R,Sep) :- 
	concatenate(T,Tmp,Sep), swritef(R,'%w%w%w',[H,Sep,Tmp]).

%% map/3: Prolog doesn't provides a REAL unary map implementation.
map(_,[],[]).
map(P,[H|T],[R|O]) :-
	call(P,H,R), map(P,T,O).

%% getAbsoluteName: retrieve a named element from a context
getAbsoluteName(_,absoluteReference(S,O,E),R) :-
	swritef(Str,"%w_%w_%w",[S,O,E]),
	string_to_atom(Str,R).
getAbsoluteName(_,absoluteReference(F,E),R) :-
	swritef(Str,"%w_%w",[F,E]),
	string_to_atom(Str,R).
getAbsoluteName(Cxt,inferedReference(E),R) :-
	context(Cxt), contextTarget(Cxt,T),
	swritef(Str,"%w_%w",[T,E]),
	string_to_atom(Str,R).

%% getAbsoluteNames: iterate over a list of name references
getAbsoluteNames(_,[],[]).
getAbsoluteNames(Cxt,[H|T],[R|O]) :- 
	getAbsoluteName(Cxt,H,R), getAbsoluteNames(Cxt,T,O).

%% writeList => display a List on the terminal
writeList([]).
writeList([H|T]) :- 
	write(H), nl, writeList(T).
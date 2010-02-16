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
%% filter/3: Prolog doesn't provide a REAL filter implementation
filter(_,[],[]).
filter(P,[H|T],[H|O]) :- call(P,H), filter(P,T,O).
filter(P,[H|T],O) :- \+ call(P,H), filter(P,T,O).

%% getAbsoluteName: retrieve a named element from a context
getAbsoluteName(_,absoluteReference(S,O,E),R) :-
	swritef(Str,"%w_%w_%w",[S,O,E]),
	string_to_atom(Str,R),!.
getAbsoluteName(_,absoluteReference(F,E),R) :-
	swritef(Str,"%w_%w",[F,E]),
	string_to_atom(Str,R),!.
getAbsoluteName(Cxt,inferedReference(E),R) :-
	context(Cxt), contextTarget(Cxt,T),
	swritef(Str,"%w_%w",[T,E]),
	string_to_atom(Str,R),!.
getAbsoluteName(_,A,A).

%% getAbsoluteNames: iterate over a list of name references
getAbsoluteNames(_,[],[]).
getAbsoluteNames(Cxt,[H|T],[R|O]) :- 
	getAbsoluteName(Cxt,H,R), getAbsoluteNames(Cxt,T,O).

%% writeList => display a List on the terminal
writeList([]).
writeList([H|T]) :- 
	write(H), nl, writeList(T).

%% assertWithoutDuplication
adoreAssert(Fact) :- Fact, !.
adoreAssert(Fact) :- assert(Fact).

%%% Variable label common transformations

genVarLabel(V,Label) :- 
	getVariable(V,Tmp), %% a variable, not an anonymous field access
	getPreviousName(Tmp,OldName), %% The REAL name, not the renamed one
%	\+ Tmp = OldName, 
	suffixToStar(OldName, PrettyName), %% '_star' <-> '*'
	genFields(V,Fields), %% fields as x.y.z
	swritef(Label,"%w%w",[PrettyName,Fields]). %% that's all folks

genFields(I,'') :- \+ fieldAccess(I,_,_), !.
genFields(I,R) :- fieldAccess(I,_,L), showAsPoint(L,R).

showAsPoint([],'').
showAsPoint([H],R) :-  
	suffixToStar(H,V),swritef(R,'.%w',[V]),!.
showAsPoint([H|T],R) :- 
	showAsPoint(T,O),
	swritef(R,".%w%w",[H,O]).

%% suffix a variable name: '_star' -> '*'
suffixToStar(V,R) :- 
	\+ is_list(V), string_to_list(V,L), suffixToStar(L,R).
suffixToStar([],'') :- !.
suffixToStar("_star",'*')  :- !.
suffixToStar([H|T],R) :- 
	string_to_list(C,[H]), suffixToStar(T,O),
	swritef(R,"%w%w",[C,O]).


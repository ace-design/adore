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

%% We leave 'pebbles' after us (cf 'Le petit poucet' and his 'petits cailloux').
:- dynamic pebble/4.

%% Short name for context
:- dynamic adoreContext/2.

declareContext(Ctx,Id) :- 
	gensym(ctx_,Id), assert(adoreContext(Id,Ctx)).

%%%%%% traceRename/4: traceRename(K,O,N,C)
%%  -> K: pick one from  activity|variable|constant
%%  -> O: old name
%%  -> N: new name
%%  -> C: context of renaming (compile, duplication, merge).

traceRename(Kind, Old, New, Context) :-
	assert(pebble(rename(Kind), Old, New, Context)).

traceDerivation(Ctx,Old,New) :- 
	assert(pebble(derivation, Old, New, Ctx)).

traceVanishment(Ctx,Element,Kind) :- 
	assert(pebble(vanishment, Element, Kind, Ctx)).

traceUnification(_,[],_).
traceUnification(Ctx, [Old|T], New) :- 
	assert(pebble(unification,Old,New,Ctx)),
	traceUnification(Ctx,T,New).


traceSubstitution(Old,New) :- 
	assert(pebble(substitution,Old,New,_)).
getSubstitute(Old,New) :- 
	pebble(substitution,Old,New,_).

resolveUnifiedVariable(Act,Var, Var) :- hasForKind(Act,hook),!.
resolveUnifiedVariable(_,Var, Unified) :- getSubstitute(Var,Unified),!.
resolveUnifiedVariable(_,Var,Var).


getPreviousName(New,Old) :- pebble(rename(_),Old,New,_),!.
getPreviousName(New,New). %% i.e. there is no pebble to lead us (no renaming).


getImmediateDerivation(Ctx,Old,New) :- 
	pebble(derivation,Old,New,Ctx).

findRoot(Elem,Root) :- 
	pebble(Ctx,Ancestor,Elem,_), \+ Ctx = rename(_), 
	findRoot(Ancestor,Root),!.
findRoot(Elem,Elem).


findUserRoot(Elem,Root) :- 
	pebble(_,Root,Elem,_), pebble(rename(_),_,Root,_),!.
findUserRoot(Elem,Root) :- 
	pebble(_,Ancestor,Elem,_), findUserRoot(Ancestor,Root),!.
findUserRoot(Elem,Elem).

identifyClone(Act,Process,ClonedAct) :- 
	activity:belongsTo(Act,Origin),
	(  adoreContext(CtxId,clone(Origin,Process)) 
         | adoreContext(CtxId,instantiate(Origin,_,Process)) ),
	getImmediateDerivation(CtxId,Act,ClonedAct).

identifyVarClone(V,Process,Clone) :- 
	variable:belongsTo(V,Origin),
	(  adoreContext(CtxId,clone(Origin,Process)) 
         | adoreContext(CtxId,instantiate(Origin,_,Process)) ),
	getImmediateDerivation(CtxId,V,Clone).
traceListing(Elem) :- 
	findall( pebble(A,B,Elem,Ctx), pebble(A,B,Elem,Ctx), Rights),
	findall( pebble(A,Elem,B,Ctx), pebble(A,Elem,B,Ctx), Lefts),
	flatten([Rights,Lefts],Listing), writeList(Listing).


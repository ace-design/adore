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
:- module(tag, [doTag/1, tag/2, tag/3, clearTags/0]).

:- dynamic tag/2.
:- dynamic tag/3.


%%%%%%
%%% end user interface
%%%%%%

doTag(Process) :- 
	dinfo(algo,'Running doTag(~w)',[Process]),
	myTimer(tag:buildTagList(Process, TagList)),
	adoreAssertAList(TagList),
	dinfo(algo,'doTag(~w) ended with success!',[Process]).

buildTagList(Process,FinalTagList) :- 
	process:exists(Process), 
	findall(Tags,tag:generate(Process,Tags), AllTags),
	flatten(AllTags, RawTags), simplify(RawTags, Simplified), 
	sort(Simplified,FinalTagList).

clearTags :- 
	retractall(tag(_,_)), retractall(tag(_,_,_)).

%%% Internal algorithm

%% Available tags
% ~> tag(control, Act).
% ~> tag(guard, Act, condition(Var, Other), true|false).
% ~> tag(fault, Act, error(Fault, Other)).
%%

%%%%
%% Initial Control-flow (receive -> ...)
%%%%
generate(Process, [tag(control, EntryPoint)|Propagation]) :- 
	activity:belongsTo(EntryPoint, Process), \+ path(_,EntryPoint), 
	findControlSuccessors(EntryPoint, Succs),
	propagate(control,Succs,[],Propagation).

%%%%
%% Guards
%%%%
generate(Process, [tag(guard, Act, Meta)|Propagation]) :- 
	activity:belongsTo(Act,Process), isGuardedBy(Act,Other,Var,Value),
	Meta = condition(Var,Other,Value), findAllSuccessors(Act,Succs), 
	propagate(guard, Succs, Meta, Propagation).

%%%%
%% Faults
%%%%

generate(Process, [tag(fault, Act, Meta)|Propagation]) :- 
	activity:belongsTo(Act,Process), onFailure(Act, FaultyAct, Fault),
	Meta = error(Fault,FaultyAct), findAllSuccessors(Act,Succs), 
	propagate(fault, Succs, Meta, Propagation).


%%%%
%% Simplification rules
%%%%

%simplify(L,L).
simplify(L,Simplified) :- 
 	findall(X,tag:shouldBeSimplified(L,X),Raws), flatten(Raws,ToDelete),
 	removeList(ToDelete, L, Simplified).

%% Fault Flow pouring into regular control-flow
shouldBeSimplified(L, [tag(fault, Act, Meta)]) :- 
	member(tag(control,Act), L), member(tag(fault, Act, Meta), L).
%% Exclusive conditions means no conditions
shouldBeSimplified(L, [OnTrue, OnFalse]) :- 
	member(tag(guard, Act, condition(Var, Test, true)), L),
	member(tag(guard, Act, condition(Var, Test, false)), L),
	OnTrue  = tag(guard, Act, condition(Var, Test, true)),
	OnFalse = tag(guard, Act, condition(Var, Test, false)).

%%%%
%% Algorithm helpers
%%%%

%% does not make any sense outside this algorithm (too restrictive)
restrictedControlPath(A,B) :- ( waitFor(B,A) | weakWait(B,A) ).
restrictedControlPath(A,B) :- 
	( waitFor(O,A) | weakWait(O,A) ), restrictedControlPath(O,B).

findAllSuccessors(Act,Succs) :- 
	findall(O,relations:existsPath(Act,O),Others), sort(Others, Succs).

findControlSuccessors(Act,Succs) :- 
	findall(O,tag:restrictedControlPath(Act,O),Others), sort(Others, Succs).


propagate(_,[],_,[]).
propagate(Flag,[H|T],[],[tag(Flag,H)|Others]) :- 
	propagate(Flag,T,[],Others),!.
propagate(Flag,[H|T],Meta,[tag(Flag,H,Meta)|Others]) :- 
	propagate(Flag,T,Meta,Others).
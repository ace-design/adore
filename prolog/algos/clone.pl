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
:- module(clone, [doClone/2]).

%%%%%%
%%% end user interface
%%%%%%

doClone(Orig,Output) :- 
	buildActions(Orig,Output,Directives),
	executeActionSet(Directives),!.

buildActions(Orig,Output,Dirs) :- 
	process:exists(Orig), \+ process:exists(Output),
	gensym(clone_ctx,CtxId),
	cloneProcess(clone(Orig,CtxId),Orig,Output,Dirs).

%%%%
%% Process
%%%%
cloneProcess(Ctx, In,Out, Actions) :- 
	traceDerivation(Ctx, In, Out),
	cloneAllVariables(Ctx,In, VarActions),
	findall(A,clone:processAdditionalAction(Ctx,In,Out,A),AddProcess),
	cloneAllActivities(Ctx,In, ActActions),
	cloneAllRelations(Ctx,In, RelActions),
	flatten([createProcess(Out),AddProcess,
	         VarActions,ActActions, RelActions],Actions).

processAdditionalAction(_,In,Out,setAsFragment(Out)) :- 
	isFragment(In).
processAdditionalAction(_,In,Out,setService(Out,Srv)) :- 
	hasForSrvName(In,Srv).
processAdditionalAction(_,In,Out,setOperation(Out,Op)) :- 
	hasForOpName(In,Op).

processAdditionalAction(Ctx,In,Out,setAsFragmentParameter(Out,Param)) :-
         hasForParameter(In,Tmp),
	 process:bindsFragmentParameterToVariable(In,Tmp,V),
	 getImmediateDerivation(Ctx,V,Param).
processAdditionalAction(_,In,Out,setAsFragmentParameter(Out,Param)) :-
         hasForParameter(In,Param), 
	 \+ process:bindsFragmentParameterToVariable(In,Param,_).


%%%%
%% Variables
%%%%

cloneAllVariables(Ctx,Process,Actions) :- 
	findall(A,clone:cloneVariable(Ctx,Process,A),Actions).

cloneVariable(Ctx,Process,Actions) :- 
	process:getVariables(Process,Vars), member(V,Vars),
	genVariableId(CloneId), traceDerivation(Ctx,V,CloneId),
	findall(A,clone:varAdditionalAction(V,CloneId,A),AddVar),
	flatten([createVariable(CloneId),AddVar],Actions).

varAdditionalAction(V,Clone,setVariableType(Clone,Type)) :-
	hasForType(V,Type).
varAdditionalAction(V,Clone,setInitValue(Clone,RawVal)) :- 
	hasForInitValue(V,RawVal).
varAdditionalAction(V,Clone,setConstancy(Clone)) :- isConstant(V).
varAdditionalAction(V,Clone,flagAsSet(Clone)) :- isSet(V).

%%%%
%% Activity
%%%%
cloneAllActivities(Ctx,Process,Actions) :- 
	findall(A,clone:cloneActivity(Ctx,Process,A),Actions).

cloneActivity(Ctx,Process,Actions) :- 
	process:getActivities(Process,Acts), member(A,Acts),
	genActivityId(CloneId), traceDerivation(Ctx,A,CloneId),
	findall(Act,clone:actAdditionalAction(Ctx,A,CloneId,Act),AddAct),	
	flatten([createActivity(CloneId),AddAct],Actions).

%% Context independent actions
actAdditionalAction(_,Act,Clone,setActivityKind(Clone,K)) :- 
	hasForKind(Act,K).
actAdditionalAction(_,Act,Clone,setInvokedService(Clone,Srv)) :- 
	hasForService(Act,Srv).
actAdditionalAction(_,Act,Clone,setInvokedOperation(Clone,Op)) :- 
	hasForOperation(Act,Op).
actAdditionalAction(_,Act,Clone,setFunction(Clone,Fct)) :- 
	hasForFunction(Act,Fct).

%% Context dependent actions
actAdditionalAction(Ctx,Act,Clone,setContainment(Clone,Process)) :-
	activity:belongsTo(Act,P), getImmediateDerivation(Ctx,P,Process).
actAdditionalAction(Ctx,Act,Clone,addAsInput(Var,Clone)) :-
	usesAsInput(Act,OldVar), getImmediateDerivation(Ctx,OldVar,Var).
actAdditionalAction(Ctx,Act,Clone,addAsOutput(Var,Clone)) :-
	usesAsOutput(Act,OldVar), getImmediateDerivation(Ctx,OldVar,Var).
actAdditionalAction(Ctx,Act,Clone,setMessageBinding(Clone,Msg,Var)) :-
	usesAsBinding(Act,Msg,OldVar), getImmediateDerivation(Ctx,OldVar,Var).

%%%%
%% Relations
%%%%

cloneAllRelations(Ctx, Process, Actions) :- 
	findall(A,clone:clonePath(Ctx,Process,A),Actions).

clonePath(Ctx, Process, defWaitFor(X,Y)) :- 
	activity:belongsTo(OldX,Process), activity:belongsTo(OldY,Process),
	waitFor(OldX,OldY), 
	getImmediateDerivation(Ctx,OldX,X), getImmediateDerivation(Ctx,OldY,Y).
clonePath(Ctx, Process, defWeakWait(X,Y)) :- 
	activity:belongsTo(OldX,Process), activity:belongsTo(OldY,Process),
	weakWait(OldX,OldY), 
	getImmediateDerivation(Ctx,OldX,X), getImmediateDerivation(Ctx,OldY,Y).
clonePath(Ctx, Process, defGuard(X,Y,V,C)) :- 
	activity:belongsTo(OldX,Process), activity:belongsTo(OldY,Process),
	isGuardedBy(OldX,OldY,OldVar,C), getImmediateDerivation(Ctx,OldVar,V),
	getImmediateDerivation(Ctx,OldX,X), getImmediateDerivation(Ctx,OldY,Y).
clonePath(Ctx, Process, defOnFail(X,Y,E)) :- 
	activity:belongsTo(OldX,Process), activity:belongsTo(OldY,Process),
	onFailure(OldX,OldY,E), 
	getImmediateDerivation(Ctx,OldX,X), getImmediateDerivation(Ctx,OldY,Y).

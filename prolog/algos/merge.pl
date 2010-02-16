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

:- module(merge, [doMerge/2, shiftActivity/3, unifyActivities/3]).

%%%%%%
%% Related publication: 
%%  Sébastien Mosser, Mireille Blay-Fornarino, Michel Riveill. 
%%  "Web Services Orchestration Evolution : 
%%   A Merge Process For Behavioral Evolution" (long paper ) 
%%  in Proceedings of the 2nd European Conference on 
%%     Software Architecture (ECSA'08), Acceptation Rate: 14 %, 
%%  Springer LNCS, Paphos, Cyprus, 29 sep - 1 oct 2008
%%  http://rainbow.polytech.unice.fr/publis/mosser-blay-fornarino-etal:2008b.pdf
%%%%%%

%%
% end user interface
%%

doMerge(Fragments,Output) :- 
	dinfo(algo,'Running doMerge(~w,~w)',[Fragments,Output]),
	dinfo(algo,'  Computing action set',[]),
	myTimer(merge:buildActions(Fragments,Output,Directives)),
	length(Directives, LActions), 
	dinfo(algo,'  => Result: ~w actions',[LActions]),
	dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(Directives)),
	dinfo(algo,'doMerge(~w,~w) ended with success!',[Fragments,Output]).

buildActions(Fragments,Output,Dirs) :- 
	\+ process:exists(Output), 
	declareContext(merge(Fragments,Output),Ctx),
	mergeFragments(Ctx,Fragments,Output,Dirs).


%%%
% Internal algorithm
%%%

mergeFragments(Ctx,Frags,Out,Actions) :- 
	CreateActs = [createProcess(Out), setAsFragment(Out)],
	findall(A,merge:pourFragmentIntoOutput(Frags,Out,A),FragUnionActs),
	findall(A,merge:removeOldFragments(Ctx,Frags,A),FragRemovalActs),
	findFragActsUnification(Ctx,predecessors,Frags,PredsActs),
	findFragActsUnification(Ctx,successors,Frags,SuccActs),
	findFragActsUnification(Ctx,hook,Frags,HookActs),
	findVariableDeclaredUnification(Ctx,Frags,VarUnifActs),
	flatten([CreateActs, FragUnionActs, FragRemovalActs,
	         PredsActs, SuccActs, HookActs,VarUnifActs],Actions).
%%%
% Fragments unions
%%%

pourFragmentIntoOutput(Frags,Output,shiftActivity(A,Output)) :- 
	member(F,Frags), process(F), isFragment(F), activity:belongsTo(A,F).

removeOldFragments(Ctx,Frags,Actions) :- 
	member(F,Frags), traceVanishment(Ctx,F,process), 
	Actions= [ retract(process(F)), retract(isFragment(F))].

%%%
% Activities unification detection
%%%

findFragActsUnification(Ctx,Kind,Fragments,[unifyActivities(Ctx,Filtered)]) :- 
	maplist(process:getActivities,Fragments,Raw), flatten(Raw,Acts),
	activity:filterByKind(Acts,Kind,Filtered).
	
%%%
% Macro Actions
%%%
:- assert(user:isMacroAction(shiftActivity,3)).
shiftActivity(Activity, NewContainer,Actions) :- 
	isContainedBy(Activity,Old),
	Actions = [ retract(isContainedBy(Activity,Old)), 
	            setContainment(Activity,NewContainer) ].

:- assert(user:isMacroAction(unifyActivities,3)).
unifyActivities(Ctx, Activities, Actions) :- 
	activity:areUnifiable(Activities,K,P), genActivityId(Unified), 
	traceUnification(Ctx,Activities,Unified),
	CreateActs = [ createActivity(Unified), setActivityKind(Unified,K),
	               setContainment(Unified,P)],
	findall(A, merge:absorbRelation(Activities,Unified,A), RelActs),
	findall(A, merge:absorbVariables(Activities,Unified,A), VarActs),
	findall(A, merge:delActivities(Activities,A), DelActs),
	flatten([CreateActs, VarActs, RelActs, DelActs],Actions).


%% Absorb relation between "Old \in Olds" and the others, using "New".
absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), waitFor(Old,X), 
	Actions = [retract(waitFor(Old,X)), defWaitFor(New,X)].
absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), waitFor(X,Old), 
	Actions= [retract(waitFor(X,Old)), defWaitFor(X,New)].

absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), weakWait(Old,X), 
	Actions= [retract(weakWait(Old,X)), defWeakWait(New,X)].
absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), weakWait(X,Old), 
	Actions= [retract(weakWait(X,Old)), defWeakWait(X,New)].

absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), isGuardedBy(X,Old,V,C), 
	Actions= [retract(isGuardedBy(X,Old,V,C)), defGuard(X,New,V,C)].
absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), isGuardedBy(Old,X,V,C), 
	Actions= [retract(isGuardedBy(Old,X,V,C)), defGuard(New,X,V,C)].

absorbRelation(Olds,New,Actions) :-
	member(Old,Olds), onFailure(X,Old,E), 
	Actions= [retract(onFailure(X,Old,E)), defOnFail(X,New,E)].
absorbRelation(Olds,New,Actions) :- 
	member(Old,Olds), onFailure(Old,X,E), 
	Actions= [retract(onFailure(Old,X,E)), defOnFail(New,X,E)].

%% Absorb Variables
absorbVariables(Olds,New,Actions) :- 
	member(Old,Olds), usesAsInput(Old,V), 
	Actions = [ retract(usesAsInput(Old,V)), addAsInput(V,New)].
absorbVariables(Olds,New,Actions) :- 
	member(Old,Olds), usesAsOutput(Old,V), 
	Actions = [ retract(usesAsOutput(Old,V)), addAsOutput(V,New)].

%% Delete old activities
delActivities(Olds,Actions) :- 
	member(Old,Olds), 
	activity(Old), hasForKind(Old,K), isContainedBy(Old,P),
	Actions = [ retract(activity(Old)), retract(hasForKind(Old,K)), 
	            retract(isContainedBy(Old,P))].
%%%%
%% Variable unification declared using 'adoreEquivalence' facts
%%%%

findVariableDeclaredUnification(_,Fragments,Actions) :- 
	findall(V,(member(F,Fragments), process:getVariables(F,V)), RawVars),
	flatten(RawVars,Variables), map(findRoot,Variables,RootVariables),
	findall(V,merge:extractVariableUnifications(RootVariables,V),Unifiable),
	computeVariableUnifications(Fragments,Unifiable,Actions).

extractVariableUnifications(RootVariables, RV) :- 
	member(RV,RootVariables), adoreEquivalence(variable,EquivList),
	member([Process, Variable], EquivList), 
	pebble(rename(variable),Variable,RV,compile(Process)).


computeVariableUnifications(_,[],[]) :- !. %% Nothing to unify
computeVariableUnifications(_,[_],[]) :- !. %% a single entity
computeVariableUnifications(Fragments,VarList,Actions) :- %% Here is fun
	member(AVariable,VarList), 
	cloneVariable(AVariable,CloneActs,CloneId),
	findall(Act,buildVarSubstList(Fragments,VarList,CloneId,Act),SubstActs),
	sort(SubstActs, SortedSubstActs),
	flatten([CloneActs,SortedSubstActs], Actions).

buildVarSubstList(Fragments,VarList,CloneId, Action) :- 
	member(V, VarList), variable:belongsTo(V,RealFragment), 
	member(ClonedFragment,Fragments), 
	(  adoreContext(CtxId,clone(RealFragment,ClonedFragment))
         | adoreContext(CtxId,instantiate(RealFragment,_,ClonedFragment)) ),
	getImmediateDerivation(CtxId,V,RealVariable), 
	Action = [substVariable(RealVariable,CloneId)].
	
cloneVariable(V,Actions,CloneId) :- 
	genVariableId(CloneId),
	findall(A,merge:varAdditionalAction(V,CloneId,A),AddVar),
	flatten([createVariable(CloneId),AddVar],Actions).

varAdditionalAction(V,Clone,setVariableType(Clone,Type)) :-
	hasForType(V,Type).
varAdditionalAction(V,Clone,setInitValue(Clone,RawVal)) :- 
	hasForInitValue(V,RawVal).
varAdditionalAction(V,Clone,setConstancy(Clone)) :- isConstant(V).
varAdditionalAction(V,Clone,flagAsSet(Clone)) :- isSet(V).
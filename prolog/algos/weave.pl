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

:- module(weave,[doWeave/1, substVariable/3, delActivity/2, simplifyProcess/2]).

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

doWeave(Directives) :- 
	buildActions(Directives, Actions), executeActionSet(Actions),!.

buildActions(Directives, Actions) :- 
	declareContext(weave(Directives),Ctx),
	findall(A, weave:applyDir(Ctx, Directives,A), Raw),
	flatten(Raw,Actions).

%%%
% Internal algorithm
%%%


applyDir(_,Directives,Actions) :- 
	member(weave(Frag,Block),Directives), 
 	activity:isWellFormed(Block,Output),
	%% Shifting fragment content into the targeted process
	findall(shiftActivity(A,Output), activity:belongsTo(A,Frag), ShiftActs),
	%% Binding Fragment Artifacts: Hook, P & S
	bindsArtifacts(Frag,Block, BindingActs),
	%% Deleting Fragments Artifacts
	delFragmentArtifacts(Frag,DelActs),
	flatten([ShiftActs, BindingActs, DelActs, simplifyProcess(Output)],Actions).
	
	


bindsArtifacts(Frag,Targets,Actions) :- 
	bindsPredecessors(Frag, Targets, PredsActions),
	bindsSuccessors(Frag, Targets, SuccsActions),
	bindsHook(Frag, Targets, HookActions),
	flatten([PredsActions,SuccsActions, HookActions], Raw),
	sort(Raw,Actions).

bindsPredecessors(Frag,Targets,Actions) :- 
	process:getPreds(Frag,Pred), activity:getFirsts(Targets,Firsts),
	maplist(activity:getPredecessors,Firsts,RawPreds), 
	flatten(RawPreds,Preds),
	findall(A,weave:shiftRelation(Pred,Preds,A),Actions).

bindsSuccessors(Frag,Targets,Actions) :- 
	process:getSuccs(Frag,Succ), activity:getLasts(Targets,Lasts),
	maplist(activity:getSuccessors,Lasts,RawSuccs), 
	flatten(RawSuccs,Succs),
	findall(A,weave:shiftRelation(Succ,Succs,A),Actions).

%bindsHook(_,_,[]) :- !.
bindsHook(Frag,Targets,Actions) :- 
	process:getHook(Frag,Hook), 
	unifyVariables(Hook, Targets, VarActs),
	findall(A,weave:adaptRelation(Hook,Targets,A),AdaptActs),
	findall(A,weave:shiftRelation(Hook,Targets,A),ShiftActs),
	flatten([VarActs,ShiftActs,AdaptActs],Actions).


delFragmentArtifacts(Frag, Actions) :- 
	process:getPreds(Frag,Pred), process:getSuccs(Frag,Succ), 
	process:getHook(Frag,Hook), 
	Actions = [delActivity(Pred), delActivity(Succ), delActivity(Hook)].


%%
% Hook variables unification
%%

unifyVariables(Hook,TargetBlock,Substitutions) :- 
	findall(V,activity:useVariable(Hook,V),HookVariables),
	identifyVariableUnification(Hook, TargetBlock, 
	                            HookVariables,Substitutions).

identifyVariableUnification(_,_,[],[]).
identifyVariableUnification(Hook, Block, [Var|Tail],Actions) :- 
	findall(Equiv,variable:findEquivalentInBlock(Hook,Var,Block,Equiv),
	        Candidates),
	validateCandidates(Var,Candidates,Selected),
 	identifyVariableUnification(Hook,Block,Tail,Others),
	flatten([substVariable(Var,Selected),Others],Actions).

validateCandidates(V,[],_) :- 
	dfail(weave,'Cannot unify \'~w\' with existing variable!',[V]),!.
validateCandidates(_,[C],C) :- !.
validateCandidates(V,L,_) :- 
	dfail(weave,
	      'There is multiple candidates to be unified with \'~w\': ~w',
	      [V,L]),!.

%%%
% Macro Actions
%%%

:- assert(user:isMacroAction(delActivity,2)).
delActivity(Old,Actions) :- 
	activity(Old), hasForKind(Old,K), isContainedBy(Old,P),
	RetractActs = [ retract(activity(Old)), retract(hasForKind(Old,K)), 
	                retract(isContainedBy(Old,P))],
	findall(A, weave:delAPath(Old,A), RelActions),
	flatten([RetractActs,RelActions],Actions).

:- assert(user:isMacroAction(substVariable,3)).
substVariable(Old,New, Actions) :- 
	findall( [retract(usesAsInput(Act,Old)), addAsInput(New,Act)],
	         activity:useVariable(Act,Old,in),Inputs),
        findall( [retract(usesAsOutput(Act,Old)), addAsOutput(New,Act)],
	         activity:useVariable(Act,Old,out),Outputs),
	flatten([Inputs,Outputs],Actions).


:- assert(user:isMacroAction(simplifyProcess,2)).
simplifyProcess(P,Actions) :- 
	findall(A, performSimplification(P,A), Actions).

performSimplification(P, Action) :- 
	process:getActivities(P,Activities),
	member(X, Activities), member(Y, Activities),
	path(X,Y), relations:getPath(X,Y,L), \+ L = [X,Y], 
	weave:delAPath(X,Y,Action).

%%%
% Technical Stuff
%%%


%% Deleting Path

delAPath(Act,A) :- delAPath(Act,_,A).

delAPath(Act,X,retract(waitFor(Act,X))) :- waitFor(Act,X).
delAPath(Act,X,retract(waitFor(X,Act))) :- waitFor(X,Act).
delAPath(Act,X,retract(weakWait(Act,X))) :- weakWait(Act,X).
delAPath(Act,X,retract(weakWait(X,Act))) :- weakWait(X,Act).
delAPath(Act,X,retract(isGuardedBy(Act,X,V,C))) :- isGuardedBy(Act,X,V,C).
delAPath(Act,X,retract(isGuardedBy(X,Act,V,C))) :- isGuardedBy(X,Act,V,C).
delAPath(Act,X,retract(onFailure(Act,X,E))) :- onFailure(Act,X,E).
delAPath(Act,X,retract(onFailure(X,Act,E))) :- onFailure(X,Act,E).



shiftRelation(Activity, Block, defWaitFor(X,F)) :- 
	waitFor(X,Activity), activity:isFirst(Block,F).
shiftRelation(Activity, Block, defWeakWait(X,F)) :- 
	weakWait(X,Activity), activity:isFirst(Block,F).
shiftRelation(Activity, Block, defGuard(X,F,C,V)) :- 
	isGuardedBy(X,Activity,C,V), activity:isFirst(Block,F).
shiftRelation(Activity, Block, defOnFail(X,F,E)) :- 
	onFailure(X,Activity,E), activity:isFirst(Block,F).

shiftRelation(Activity, Block, defWaitFor(L,X)) :- 
	waitFor(Activity,X), activity:isLast(Block,L).
shiftRelation(Activity, Block, defWeakWait(L,X)) :- 
	weakWait(Activity,X), activity:isLast(Block,L).
shiftRelation(Activity, Block, defGuard(L,X,C,V)) :- 
	isGuardedBy(Activity,X,C,V), activity:isLast(Block,L).
shiftRelation(Activity, Block, defOnFail(L,X,E)) :- 
	onFailure(Activity,X,E), activity:isLast(Block,L).


%%WARNING: waitFor(a,b) ==> path(b,a) !!!!! 
adaptRelation(Hook, Block, defWaitFor(F,Y)) :- 
	waitFor(Hook,X), hasForKind(X,predecessors), 
	activity:isFirst(Block, F), path(Y,F).
adaptRelation(Hook, Block, defWeakWait(F,Y)) :- 
	weakWait(Hook,X), hasForKind(X,predecessors), 
	activity:isFirst(Block, F), path(Y,F).
adaptRelation(Hook, Block, defGuard(F,Y,V,C)) :- 
	isGuardedBy(Hook,X,V,C), hasForKind(X,predecessors), 
	activity:isFirst(Block, F), path(Y,F).
adaptRelation(Hook, Block, defOnFail(F,Y,E)) :- 
	onFailure(Hook,X,E), hasForKind(X,predecessors),
	activity:isFirst(Block, F), path(Y,F).

adaptRelation(Hook, Block, defWaitFor(Y,L)) :- 
	waitFor(X,Hook), hasForKind(X,successors), 
	activity:isLast(Block, L), path(L,Y).
adaptRelation(Hook, Block, defWeakWait(Y,L)) :- 
	weakWait(X,Hook), hasForKind(X,successors), 
	activity:isLast(Block, L), path(L,Y).
adaptRelation(Hook, Block, defGuard(Y,L,V,C)) :- 
	isGuardedBy(X,Hook,V,C), hasForKind(X,successors), 
	activity:isLast(Block, L), path(L,Y).
adaptRelation(Hook, Block, defOnFail(Y,L,E)) :- 
	onFailure(X,Hook,E), hasForKind(X,successors), write(bar),
	activity:isLast(Block, L), path(L,Y).


%%%
% Old Stuff
%%%








%% %% Relation retract

%% retractRelation(Act,retract(waitFor(Act,X))) :- waitFor(Act,X).
%% retractRelation(Act,retract(waitFor(X,Act))) :- waitFor(X,Act).
%% retractRelation(Act,retract(weakWait(Act,X))) :- weakWait(Act,X).
%% retractRelation(Act,retract(weakWait(X,Act))) :- weakWait(X,Act).
%% retractRelation(Act,retract(isGuardedBy(Act,X,V,C))) :- isGuardedBy(Act,X,V,C).
%% retractRelation(Act,retract(isGuardedBy(X,Act,V,C))) :- isGuardedBy(X,Act,V,C).
%% retractRelation(Act,retract(onFailure(Act,X,E))) :- onFailure(Act,X,E).
%% retractRelation(Act,retract(onFailure(X,Act,E))) :- onFailure(X,Act,E).

%% propagateHookBeforeRelation(Hook, Acts,defWaitFor(Act,A)) :- 
%% 	member(Act,Acts), waitFor(Hook,A).
%% propagateHookBeforeRelation(Hook, Acts,defWeakWait(Act,A)) :- 
%% 	member(Act,Acts), weakWait(Hook,A).
%% propagateHookBeforeRelation(Hook, Acts,defGuard(Act,A,V,C)) :- 
%% 	member(Act,Acts), isGuardedBy(Hook,A,V,C).
%% propagateHookBeforeRelation(Hook, Acts,defOnFail(Act,A,E)) :- 
%% 	member(Act,Acts), onFailure(Hook,A,E).

%% propagateHookAfterRelation(Hook, Acts,defWaitFor(A,Act)) :- 
%% 	member(Act,Acts), waitFor(A,Hook).
%% propagateHookAfterRelation(Hook, Acts,defWeakWait(A,Act)) :- 
%% 	member(Act,Acts), weakWait(A,Hook).
%% propagateHookAfterRelation(Hook, Acts,defGuard(A,Act,V,C)) :- 
%% 	member(Act,Acts), isGuardedBy(A,Hook,V,C).
%% propagateHookAfterRelation(Hook, Acts,defOnFail(A,Act,E)) :-
%% 	member(Act,Acts), onFailure(A,Hook,E).


%% applyDir(_,Directives, Actions) :- 
%% 	member(weave(Frag,Targets),Directives), 
%% 	activity:isWellFormed(Targets,P),
%% 	%% Shifting fragment content into the targeted process
%% 	findall(shiftActivity(A,P), activity:belongsTo(A,Frag), ShiftActs),
%% 	%% Concretizing Preds & Succs
%% 	concretize(Frag, Targets, PredsSuccsActs),
%% 	%% Unifiying Hook
%% 	unifyHook(Frag, Targets, HookActs),
%% 	%% Deleting Fragment artifact (P,S & Hook)
%% 	delFragmentArtifacts(Frag, DelActs),
%%         %% Building final result
%% 	flatten([ ShiftActs, PredsSuccsActs, HookActs, DelActs, 
%% 	          simplifyProcess(P)], Actions).





%%%
% Hook
%%%

%% unifyHook(Frag, Block, Actions) :- 
%% 	process:getHook(Frag,Hook), 
%% 	unifyVariables(Hook, Block, VarActs),
%% 	propagateHookRelations(Hook, Block, RelActs),
%% 	flatten([VarActs,RelActs], Actions).

%propagateHookRelations(_,_,[]) :- !.
%% propagateHookRelations(Hook, Block, Actions) :- 
%% 	activity:getFirsts(Block, Firsts),
%% 	activity:getLasts(Block, Lasts),
%% 	findall( A, weave:propagateHookBeforeRelation(Hook,Firsts,A),
%% 	         FirstsActions),
%% 	findall( A, weave:propagateHookAfterRelation(Hook,Lasts,A), 
%% 		 LastsActions),
%% 	writeList([FirstsActions, LastsActions]),
%% 	flatten([FirstsActions, LastsActions],Tmp),
%% 	sort(Tmp,Actions).




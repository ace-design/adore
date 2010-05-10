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

:- module(weave,[doWeave/1, substVariable/3, delActivity/2]).

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
	dinfo(algo,'Running doWeave(...)',[]),
	dinfo(algo,'  Computing action set',[]),
	myTimer(weave:buildActions(Directives, Actions)), 
	length(Actions,LActions), 
	dinfo(algo,'  => Result: ~w actions',[LActions]),
	dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(Actions)),
	dinfo(algo,'doWeave(...) ended with success!',[]).
	
buildActions(Directives, Actions) :- 
	declareContext(weave(Directives),Ctx),
	findall(A, weave:applyDir(Ctx, Directives,A), Raw),
	flatten([Raw],Actions).

%%%
% Internal algorithm
%%%


applyDir(_,Directives,Actions) :- 
	member(weave(Frag,Block),Directives), 
	dinfo(algo,'  weave(~w,~w)',[Frag,Block]),
 	activity:isWellFormed(Block,Output),
	%% Shifting fragment content into the targeted process
	findall(shiftActivity(A,Output), activity:belongsTo(A,Frag), ShiftActs),
	dinfo(weave,'ShiftActs: ~w' , [ShiftActs]),
	%% Binding Fragment Artifacts: Hook, P & S
	bindsArtifacts(Frag,Block, BindingActs),
	%% Deleting Fragments Artifacts
	delFragmentArtifacts(Frag,DelActs),
	dinfo(weave,'DelActs: ~w' , [DelActs]),
	flatten([ ShiftActs, BindingActs, DelActs], Actions).
	
bindsArtifacts(Frag,Targets,Actions) :- 
	bindsHook(Frag, Targets, HookActions),
	bindsPredecessors(Frag, Targets, PredsActions),
	dinfo(weave,'PredsActions: ~w' , [PredsActions]),
	bindsSuccessors(Frag, Targets, SuccsActions),
	dinfo(weave,'SuccsActions: ~w' , [SuccsActions]),
	flatten([PredsActions,SuccsActions,HookActions], Actions).

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
	dinfo(weave,'VarActs: ~w' , [VarActs]),
	findall(A,weave:shiftRelation(Hook,Targets,A),ShiftActs),
	dinfo(weave,'ShiftRelations: ~w' , [ShiftActs]),	
	findall(A,weave:adaptRelation(Hook,Targets,A),AdaptActs),
	dinfo(weave,'AdaptRelations: ~w' , [AdaptActs]),
	flatten([VarActs, AdaptActs, ShiftActs],Actions).



delFragmentArtifacts(Frag, Actions) :- 
	process:getPreds(Frag,Pred), process:getSuccs(Frag,Succ), 
	process:getHook(Frag,Hook), !,
	Actions = [delActivity(Pred), delActivity(Succ), delActivity(Hook)].


%%
% Hook variables unification
%%

:- dynamic weave_consumed/1.
unifyVariables(Hook,TargetBlock,Substitutions) :- 
	findall(V,activity:useVariable(Hook,V),HookVariables),
	identifyVariableUnification(Hook, TargetBlock, 
	                            HookVariables,Substitutions),
	retractall(weave_consumed(_)).

identifyVariableUnification(_,_,[],[]).
identifyVariableUnification(Hook,Block,[Var|Tail],Actions) :- 
	member(A,Block), activity:belongsTo(A,P), variable:belongsTo(Var,P),!,	
	identifyVariableUnification(Hook,Block,Tail,Actions).
identifyVariableUnification(Hook, Block, [Var|Tail],Actions) :- 
	findall(Equiv,variable:findEquivalentInBlock(Hook,Var,Block,Equiv),
	        RawCandidates),
	sort(RawCandidates,Candidates),
	validateCandidates(Var,Candidates,Selected),
	traceSubstitution(Var,Selected),
 	identifyVariableUnification(Hook,Block,Tail,Others),
	flatten([substVariable(Var,Selected),Others],Actions).

validateCandidates(V,[],V) :- !,true.
        %% TODO: fixme (according to Kompose semantic: enrichment)
	%dfail(weave,'Cannot unify \'~w\' with existing variable!',[V]).

validateCandidates(_,[C],C) :- !.
validateCandidates(V,L,C) :-
	member(C,L), variable:belongsTo(C,P), variableBinding(P,Bindings),
	findUserRoot(V,Target), member([C,Target],Bindings),
	\+ weave_consumed([C,Target]), assert(weave_consumed([C,Target])),!.
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
	RetractActs = [ myRetract(activity(Old)), myRetract(hasForKind(Old,K)), 
	                myRetract(isContainedBy(Old,P))],
	findall(A, relations:delAPath(Old,A), RelActions),
	flatten([RetractActs,RelActions],Actions).

:- assert(user:isMacroAction(substVariable,3)).
substVariable(Old,New, Actions) :- 
	findall(A,doVarSubst(Old,New,A), Raw), flatten(Raw,Actions).

doVarSubst(Old,New,Actions) :- 
	usesAsInput(Act,Old), \+ fieldAccess(Old,_,_), 
	Actions = [retract(usesAsInput(Act,Old)), addAsInput(New,Act)].
doVarSubst(Old,New,Actions) :- 
	usesAsOutput(Act,Old), \+ fieldAccess(Old,_,_), 
	Actions = [retract(usesAsOutput(Act,Old)), addAsOutput(New,Act)].
doVarSubst(Old,New,Actions) :- 
	(usesAsInput(Act,F)| usesAsOutput(Act,F)), fieldAccess(F,Old,L), 
	Actions = [ retract(fieldAccess(F,Old,L)), 
	            createFieldAccess(F,New,L)].
%%%
% Technical Stuff
%%%

shiftRelation(Activity, Block, defWaitFor(X,F)) :- 
	waitFor(X,Activity), activity:isLast(Block,F).
shiftRelation(Activity, Block, defWeakWait(X,F)) :- 
	weakWait(X,Activity), activity:getLasts(Block,[F]).
shiftRelation(Activity, Block, Actions) :- 
	weakWait(X,Activity), activity:getLasts(Block,Lasts),
	length(Lasts,Length), Length > 1, 
	Block = [B|_], activity:belongsTo(B,Process),
	genActivityId(NopId), 
	findall(defWaitFor(NopId,L),member(L,Lasts),ReOrderActs),
	CreateActions=[createActivity(NopId), setActivityKind(NopId,nop), 
	         setContainment(NopId,Process)], 
	flatten([CreateActions, defWeakWait(X,NopId), ReOrderActs],Actions).

shiftRelation(Activity, Block, defGuard(X,F,VUnif,C)) :- 
	isGuardedBy(X,Activity,V,C), resolveUnifiedVariable(X,V,VUnif),
	activity:isLast(Block,F).
shiftRelation(Activity, Block, defOnFail(X,F,E)) :- 
	onFailure(X,Activity,E), activity:isLast(Block,F).

shiftRelation(Activity, Block, defWaitFor(L,X)) :- 
	waitFor(Activity,X), activity:isFirst(Block,L).
shiftRelation(Activity, Block, defWeakWait(L,X)) :- 
	weakWait(Activity,X), activity:isFirst(Block,L).
shiftRelation(Activity, Block, defGuard(L,X,VUnif,C)) :- 
	isGuardedBy(Activity,X,V,C), resolveUnifiedVariable(X,V,VUnif),
	activity:isFirst(Block,L).
shiftRelation(Activity, Block, defOnFail(L,X,E)) :- 
	onFailure(Activity,X,E), activity:isFirst(Block,L).


%%WARNING: waitFor(a,b) ==> path(b,a) !!!!! 
adaptRelation(Hook, Block, defWaitFor(F,Y)) :- 
	waitFor(Hook,X), hasForKind(X,predecessors), 
	activity:isFirst(Block, F), relations:controlPath(Y,F). %% was Path
adaptRelation(Hook, Block, defWeakWait(F,Y)) :- 
	weakWait(Hook,X), hasForKind(X,predecessors), 
	activity:isFirst(Block, F), relations:controlPath(Y,F).%% was Path
adaptRelation(Hook, Block, defGuard(F,Y,VUnif,C)) :- 
	isGuardedBy(Hook,X,V,C), hasForKind(X,predecessors), 
	activity:isFirst(Block, F), relations:controlPath(Y,F),%% was Path
	resolveUnifiedVariable(Y,V,VUnif).
adaptRelation(Hook, Block, defOnFail(F,Y,E)) :- 
	onFailure(Hook,X,E), hasForKind(X,predecessors),
	activity:isFirst(Block, F), relations:controlPath(Y,F).%% was Path

adaptRelation(Hook, Block, defWaitFor(Y,L)) :- 
	waitFor(X,Hook), hasForKind(X,successors), 
	activity:isLast(Block, L), relations:controlPath(L,Y).
adaptRelation(Hook, Block, defWeakWait(Y,L)) :- 
	weakWait(X,Hook), hasForKind(X,successors), 
	activity:isLast(Block, L), relations:controlPath(L,Y).

adaptRelation(Hook, Block, defGuard(Y,L,VUnif,C)) :- 
	isGuardedBy(X,Hook,V,C), hasForKind(X,successors), 
	activity:isLast(Block, L), relations:controlPath(L,Y),
	resolveUnifiedVariable(Y, V,VUnif).
adaptRelation(Hook, Block, defOnFail(Y,L,E)) :- 
	onFailure(X,Hook,E), hasForKind(X,successors),
	activity:isLast(Block, L), relations:controlPath(L,Y).

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
	%% Binding Fragment Artifacts: Hook, P & S
	bindsArtifacts(Frag,Block, BindingActs),
	%% Deleting Fragments Artifacts
	delFragmentArtifacts(Frag,DelActs),
	dinfo(weave,'DelActs: ~w' , [DelActs]),
	flatten([ ShiftActs, BindingActs, DelActs], Actions).
	
bindsArtifacts(Frag,Targets,Actions) :- 
	bindsPredecessors(Frag, Targets, PredsActions),
	dinfo(weave,'Preds: ~w' , [PredsActions]),
	bindsSuccessors(Frag, Targets, SuccsActions),
	dinfo(weave,'Succs: ~w' , [SuccsActions]),
	dinfo(weave,'Hook:' , []),
	bindsHook(Frag, Targets, HookActions),
	flatten([PredsActions,SuccsActions,HookActions], Actions),!.

%%%
% Predecessors
%%%

bindsPredecessors(Frag,Targets,Acts) :- 
        process:getPreds(Frag, PredGhost), activity:getFirsts(Targets, Firsts),
        maplist(activity:getAllPredecessors, Firsts, RawPreds), 
	flatten(RawPreds,RealPreds),
	findall(A,weave:shiftPredRel(PredGhost, RealPreds, Firsts, A),Acts).

shiftPredRel(Ghost, RealPreds, Block, [Add, Del]) :- 
	relations:path(Ghost, FragAct), \+ hasForKind(FragAct, hook),
	member(P, RealPreds), member(A, Block), relations:path(P,A), 
	shiftPath([P,A], [P,FragAct], Add, Del).
%%%
% Successors
%%%

bindsSuccessors(Frag,Targets, Acts) :- 
	process:getSuccs(Frag, SuccGhost), activity:getLasts(Targets, Lasts),
        maplist(activity:getAllSuccessors, Lasts, RawSuccs), 
	flatten(RawSuccs,RealSuccs),
	findall(A,weave:shiftSuccRel(SuccGhost, RealSuccs, Lasts, A),Acts).

shiftSuccRel(Ghost, RealSuccs, RealActs, [Add, Del]) :-  
	member(S, RealSuccs), member(A, RealActs), 
	relations:controlPath(A,S), relations:path(FragmentAct,Ghost),
	\+ hasForKind(FragmentAct, hook),
	shiftPath([A,S], [FragmentAct,S], Add, Del).

shiftSuccRel(Ghost, RealSuccs, RealActs, [Add, Del]) :-  
	member(S, RealSuccs), member(A, RealActs), 
	relations:controlPath(A,S), relations:path(FragmentAct,Ghost),
	\+ hasForKind(FragmentAct, hook),
	shiftPath([FragmentAct,Ghost], [FragmentAct,S],Add, Del).

%%%
% Hook
%%%

bindsHook(Frag, Targets, Actions) :- 
 	process:getHook(Frag,Hook), 
	%% Hook Predecessors & Successors
	bindsHookPreds(Hook, Targets, HookPredsActs),
	dinfo(weave,'  HookPredsActs: ~w' , [HookPredsActs]),
	bindsHookSuccs(Hook, Targets, HookSuccsActs),
	dinfo(weave,'  HookSuccsActs: ~w' , [HookSuccsActs]),
	%% Rebuild Hook direct relations (P -> Hook -> S), if any
	rebuildDirectRelations(Frag, Targets, RebuildActs),
	dinfo(weave,'  RebuildActs: ~w' , [RebuildActs]),
	%% Variable unification
	unifyVariables(Hook, Targets, VarActs),
 	dinfo(weave,'  VarActs: ~w' , [VarActs]),
	%% Pushing variable substitution inside the actions:
	flatten([HookPredsActs,HookSuccsActs,RebuildActs], SrcActs),
	push(VarActs, SrcActs, BuildActs), 
	%% Final Concatenation
	flatten([BuildActs, VarActs], Actions).

%% hook predecessors
bindsHookPreds(Hook, Targets, Acts) :- 
	activity:getFirsts(Targets, RealFirsts),
	findall(A,weave:shiftHookPredRel(Hook, RealFirsts, A),Acts).

shiftHookPredRel(Hook, RealFirsts, Actions) :- 
	relations:path(A,Hook), \+ hasForKind(A, predecessors), 
	member(F, RealFirsts), shiftPath([A,Hook],[A,F],Add, Del),
	Actions = [Add, Del].

%% hook successors
bindsHookSuccs(Hook, Targets, Acts) :- 
	activity:getLasts(Targets, RealLasts),
	findall(A,weave:shiftHookSuccRel(Hook, RealLasts, A),Acts).

shiftHookSuccRel(Hook, RealLasts, Actions) :- 
	relations:path(Hook,A), \+ hasForKind(A, successors), 
	member(L, RealLasts), shiftPath([Hook,A],[L,A],Add, Del), 
	Actions = [Add, Del].

%% hook immediate relation:
rebuildDirectRelations(Frag, Targets, Actions) :- 
	process:getPreds(Frag, PredGhost), process:getSuccs(Frag, SuccGhost),
	process:getHook(Frag, Hook),
	keepPredToHookRel(PredGhost, Hook, Targets, PredToHookActs),
	keepHookToSuccRel(SuccGhost, Hook, Targets, HookToSuccActs),
	flatten([PredToHookActs, HookToSuccActs],Actions).

keepPredToHookRel(PredGhost, Hook, _, []) :- 
	\+ relations:path(PredGhost, Hook), !.
keepPredToHookRel(PredGhost, Hook, Targets,  Actions) :- 
	relations:path(PredGhost, Hook), activity:getFirsts(Targets, Firsts),
	findall(A, (member(F,Firsts), relations:path(X,F), 
	            weave:shiftPath([X,F],[X,F],A,_)), Actions).
keepHookToSuccRel(SuccGhost, Hook, _, []) :- 
	\+ relations:path(Hook, SuccGhost), !.
keepHookToSuccRel(SuccGhost, Hook, Targets,  Actions) :- 
	relations:path(Hook, SuccGhost), activity:getLasts(Targets, Lasts),
	findall(A, (member(L,Lasts), relations:path(L,X), 
	            weave:shiftPath([Hook,SuccGhost],[L,X],A,_)), Actions).

%%
% Hook variables unification & enrichment
%%

unifyVariables(Hook, TargetBlock, Actions) :- 
	findall(S,weave:identifyVariableUnif(Hook,TargetBlock,S), Substs),
	findall(A,weave:enrichTarget(Hook, TargetBlock, Substs, A),Adds),
	flatten([Substs, Adds], Actions).

%% Unification
identifyVariableUnif(Hook, Block, Action) :-  %% regular unification
	activity:useVariable(Hook,V), 
	variable:findEquivalentInBlock(Hook,V, Block, Candidate),
	Action = [substVariable(V, Candidate)].
identifyVariableUnif(Hook, Block, Action) :- %% user-given unification
	activity:useVariable(Hook,V), member(A,Block), 
	variable:belongsTo(A,P), variableBinding(P,Bindings), 
	findUserRoot(V,Target), member([Candidate,Target],Bindings),
	Action = [substVariable(V, Candidate)].

%% enrichment
enrichTarget(Hook, TargetBlock, Substs, Action) :- %% inputs
	usesElemAsInput(Hook,V), \+ member([substVariable(V,_)],Substs),
	activity:getFirsts(TargetBlock,[First]), 
	Action = [addAsInput(V,First)].
enrichTarget(Hook, TargetBlock, Substs, Action) :- %% outputs
	usesElemAsOutput(Hook,V), \+ member([substVariable(V,_)],Substs),
	activity:getLasts(TargetBlock,[Last]), 
	Action = [addAsOutput(V,Last)].

%%%
% Technical Stuff
%%%

shiftPath([OldSrc,OldTgt],[NewSrc,NewTgt],Add,Del) :- 
	waitFor(OldTgt,OldSrc), Del = myRetract(waitFor(OldTgt,OldSrc)), 
	Add = defWaitFor(NewTgt, NewSrc).
shiftPath([OldSrc,OldTgt],[NewSrc,NewTgt],Add,Del) :- 
	weakWait(OldTgt,OldSrc), Del = myRetract(weakWait(OldTgt,OldSrc)), 
	Add = defWeakWait(NewTgt, NewSrc).
shiftPath([OldSrc,OldTgt],[NewSrc,NewTgt],Add,Del) :- %% SAME | hook => OK

 	isGuardedBy(OldTgt, OldSrc, Var, Value), 
	(OldSrc = NewSrc | hasForKind(OldSrc,hook)),
 	Del = myRetract(isGuardedBy(OldTgt, OldSrc, Var, Value)), 
 	Add = defGuard(NewTgt, NewSrc, Var, Value).
shiftPath([OldSrc,OldTgt],[NewSrc,NewTgt],Add,[]) :- 
	OldSrc \= NewSrc, isGuardedBy(OldTgt, OldSrc, _, _), 
	Add = defWaitFor(NewTgt, NewSrc). %% DIFFERENT SOURCE -> hack (waitFor)
shiftPath([OldSrc,OldTgt],[NewSrc,NewTgt],Add,Del) :- 
	onFailure(OldTgt, OldSrc, Fault), 
	Del = myRetract(onFailure(OldTgt, OldSrc, Fault)), 
	Add = defOnFail(NewTgt, NewSrc, Fault).


push([],X,X).
push([substVariable(Old, New)|Tail],Actions,Result) :- 
	propagate(Old,New,Actions, Tmp), !, push(Tail,Tmp, Result).
push([_|Tail],Actions,Result) :- 
	push(Tail,Actions, Result).

propagate(_,_,[],[]).
propagate(Old, New,[defGuard(A,G, Old, V)|Tail],[defGuard(A,G,New,V)|Others]) :-
	!, propagate(Old, New, Tail, Others).
propagate(Old,New,[X|T],[X|O]) :- propagate(Old, New, T, O).

%%% Should also delete the fragment !!!
delFragmentArtifacts(Frag, Actions) :- 
	process:getPreds(Frag,Pred), process:getSuccs(Frag,Succ), 
	process:getHook(Frag,Hook), !,
	Actions = [delActivity(Pred), delActivity(Succ), delActivity(Hook)].


%%%
% Macro Actions (aka delayed actions)
%%%

:- assert(user:isMacroAction(delActivity,2)).
delActivity(Old,Actions) :- 
	activity(Old), hasForKind(Old,K), isContainedBy(Old,P),
	RetractActs = [ myRetract(activity(Old)), myRetract(hasForKind(Old,K)), 
	                myRetract(isContainedBy(Old,P))],
	findall(A, relations:delAPath(Old,A), RelActions),
	findall(retract(usesAsInput(Old,V)),usesAsInput(Old,V),Ins),
	findall(retract(usesAsOutput(Old,V)),usesAsOutput(Old,V),Outs),
	flatten([RetractActs,RelActions,Ins,Outs],Actions).

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
%% doVarSubst in guards ??????














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
:- module(contextMerge, 
	  [doContextMerge/0,buildContextMergeDirectives/1,unifyContext/2]).

%%%%%%
%%% end user interface
%%%%%%

doContextMerge :- 
	buildContextMergeDirectives(Directives),
	executeActionSet(Directives),!.
%%%%%%
%%% Computing context merge
%%%%%%

buildContextMergeDirectives(Directives) :-  
	findall(C, context(C), Contextes),
	map(contextMerge:buildEquivalentContextList,Contextes,RawList),
	sort(RawList,RawWithoutDuplicates),
	removeAtomicContext(RawWithoutDuplicates,FinalList),
	map(contextMerge:contextListToDirectives,FinalList,Directives).

%% context equivalence (same target, and same output (empty or identified).
isEquivalentContext(Id,Other) :- 
	context(Id), context(Other), Id \= Other,
	contextTarget(Id,Target), contextTarget(Other,Target),
	(\+ contextOutput(Id,_), \+ contextOutput(Other,_)
         | contextOutput(Id,Out), contextOutput(Other,Out)).

%% retrieve equivalence list for a given context
buildEquivalentContextList(Ctx,Equivalence) :-
	findall(Eq, isEquivalentContext(Ctx,Eq), Equivalents),
	flatten([Ctx,Equivalents],Raw),
	sort(Raw,Equivalence).

%% transform a contextList into a list of merge directives
contextListToDirectives(L,unifyContext(L)).

%% remove empty list from the contextList
removeAtomicContext(L,R) :- filter(contextMerge:isCompositeContext,L,R).
isCompositeContext(CList) :- length(CList,L), L > 1.

%%%%%%
%%% Performing context merge
%%%%%%
:- assert(user:isMacroAction(unifyContext,2)).

unifyContext(L,Actions) :- 
	gensym(generatedContext_,CtxId),
	Creation = [defCompositionContext(CtxId)],
	extractTargetFromContextList(L,CtxId,Target),
	extractOutputFromContextList(L,CtxId,Output),
	propagateContextUnificationLoop(L,CtxId,Unification),
	flatten([Creation, Target, Output, Unification],Actions).

extractTargetFromContextList([H|_],Ctx,[setCompositionTarget(Ctx,P)]) :-
	contextTarget(H,P).
	
extractOutputFromContextList([H|_],Ctx,R) :- 
	(\+ contextOutput(H,Process), R = [])
        | (contextOutput(H,Process), R = [setContextOutput(Ctx,Process)]).

propagateContextUnificationLoop([],_,[]).
propagateContextUnificationLoop([H|T],NewCtx,[Act|O]) :-
	propagateContextUnification(H,NewCtx, Act),
	propagateContextUnificationLoop(T,NewCtx,O).

propagateContextUnification(Ctx,NewCtx, Actions) :- 
	findall(B,propagateActBlockMerge(Ctx,NewCtx,B),Blocks),
	findall(A,propagateApplyDirMerge(Ctx,NewCtx,A),Applys),
	findall(S,propagateSetifyMerge(Ctx,NewCtx,S),Setifys),
	Deletion = [retract(context(Ctx))], 
	flatten([Blocks,Applys,Setifys,Deletion],Actions).

propagateActBlockMerge(Ctx,NewCtx,Actions) :- 
	activityBlock(Ctx,BlockId,Activities),
	Actions = [retract(activityBlock(Ctx,BlockId,Activities)),
	           defActivityBlock(NewCtx,BlockId,Activities)].

propagateApplyDirMerge(Ctx,NewCtx,Actions) :- 
	applyFragment(ApplyId,Ctx,BlockId,Fragment),
	Actions = [retract(applyFragment(ApplyId,Ctx,BlockId,Fragment)),
	           defApply(ApplyId,NewCtx,BlockId,Fragment)].

propagateSetifyMerge(Ctx,NewCtx,Actions) :- 
	setify(Ctx,VarName),
	Actions = [retract(setify(Ctx,VarName)),
	           defSetify(NewCtx,VarName)].
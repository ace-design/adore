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
:- module(applyRewrite, 
	  [doApplyRewrite/0, buildApplyRewriteActions/1, rewriteApply/2]).

%%%%%%
%%% end user interface
%%%%%%

doApplyRewrite :- 
	buildApplyRewriteActions(Actions),
	executeActionSet(Actions),!.
%%%%%%
%%% Computing apply rewrite
%%%%%%

%% TODO: this implementation does not implement the transitive closure of
%%       context directive rewriting.

buildApplyRewriteActions(Actions) :-   
	findall(I,shouldBeRewritten(I),ApplyList),
	map(applyRewrite:generateRewriteAction,ApplyList,Actions).

shouldBeRewritten(Id) :-
	applyFragment(Id,Ctx,Block,_), 
	context(Ctx), contextTarget(Ctx,Target),
	isWellFormed(Block,Process), isFragment(Process),
	\+ Target = Process.

generateRewriteAction(Id,rewriteApply(Id)).

%%%%%%
%%% Performing apply rewrite
%%%%%%

:- assert(user:isMacroAction(rewriteApply,2)).

rewriteApply(Id,Actions) :- 
	applyFragment(Id,Ctx,Block,Fragment),
	activityBlock(Ctx,Block,Activities),
	isWellFormed(Block,TargetedFragment),
	gensym(rewrittenContext_,NewCtx),
	gensym(anonymous_,Lambda),
	Deletion  = [ retract(activityBlock(Ctx,Block,Activities)),
                      retract(applyFragment(Id,Ctx,Block,Fragment)) ],
	Creation  = [ defCompositionContext(NewCtx), 
	              setCompositionTarget(NewCtx,TargetedFragment),
		      setContextOutput(NewCtx,Lambda),
		      setAsFragment(Lambda)],
	Insertion = [ defActivityBlock(NewCtx,Block,Activities),
	              defApply(Id,NewCtx,Block,Fragment) ],
	propagateRewriteInContext(Id,Ctx,TargetedFragment,Lambda,Propagation),
	flatten([Deletion,Creation, Insertion,Propagation],Actions).

propagateRewriteInContext(Origin,Ctx,F,NewF,ActionsLists) :- 
	findall(X,rewriteInContext(Origin,Ctx,F,NewF,X), ActionsLists).

rewriteInContext(Origin,Ctx,F,NewF,X) :- 
	context(Ctx), applyFragment(Id,Ctx,Block,F), \+ Origin = Id,
	X = [retract(applyFragment(Id,Ctx,Block,F)),
	     defApply(Id,Ctx,Block,NewF)].
	
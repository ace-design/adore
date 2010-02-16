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
:- module(processNormalization, [doProcessNormalization/1]).

%%%%%%
%%% end user interface
%%%%%%

doProcessNormalization(Orig) :- 
	%dinfo(algo,'Running doProcessNormalization(~w)',[Orig]),
	%dinfo(algo,'  Computing action set',[]),
	myTimer(processNormalization:buildActions(Orig,Actions)),
	%length(Actions,LActions), 
	%dinfo(algo,'  => Result: ~w actions',[LActions]),
	%dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(Actions)),!.
	%dinfo(algo,'doProcessNormalization(~w) ended with success!',[Orig]).

buildActions(Orig, Actions) :- 
 	process:exists(Orig), 
	findall(Acts, (activity:belongsTo(A,Orig), 
	               processNormalization:rewriteWeakWait(A,Acts)),Raw),
	flatten(Raw,Actions).


rewriteWeakWait(Activity,Actions) :- 
	activity(Activity), findall(P,weakWait(Activity,P),WeakPreds),
	length(WeakPreds,L), L > 0, 
	activity:belongsTo(Activity,Process),
	genActivityId(AnId),
	CreateActs = [ createActivity(AnId), setActivityKind(AnId,nop), 
	               setContainment(AnId, Process), 
		       defWeakWait(Activity,AnId) ],
	findall( [defWeakWait(AnId,P), retract(weakWait(Activity,P))],
	         member(P,WeakPreds),ReOrderActs),
        findall(A,processNormalization:propagateDataLinks(AnId,WeakPreds,A),
	        PropActs),
	flatten([CreateActs,ReOrderActs, PropActs],Actions).


%propagateDataLinks(_,_,[]).
propagateDataLinks(NopId,WeakPreds,assert(dataLink(Process,Var,NopId))) :- 
	member(Pred,WeakPreds), activity:belongsTo(Pred,Process),
	activity:useVariable(Pred,VarId), getPreviousName(VarId,Var).
propagateDataLinks(NopId,WeakPreds,assert(dataLink(Process,Var,NopId))) :- 
	member(Pred,WeakPreds), activity:belongsTo(Pred,Process),
	getPreviousName(Pred,PredName),	dataLink(Process,Var,PredName).
	


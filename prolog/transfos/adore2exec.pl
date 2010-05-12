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

:- module(adore2exec, [buildExecutionSemantic/1]).

buildExecutionSemantic(FileName) :- 
	genExecForAllProcess(Text),
	swritef(Complete,"/** Adore Execution Semantic **/\n\n%w",[Text]),
	open(FileName,write,Stream), write(Stream,Complete), close(Stream).


%%%%
%% Internal Definition
%%%%

%% Syntaxtic Sugar

genExecForAllProcess(Text) :- 
	findall(T,(process(P),adore2exec:genExecutionSemantic(P,T)), A),
	dumpList(A,Text).

genExecutionSemantic(Process, Text) :-  
	doTag(Process), 
	findall(X, adore2exec:genActFormula(Process,X), FormulaList),
	clearTags, dumpList(FormulaList, FormulaText), 
	swritef(Text, '// %w \n%w\n',[Process, FormulaText]).

genActFormula(Process,Result) :- 
	%% Computation
	activity:belongsTo(Act, Process), buildActFormula(Act, Formula),
	%% Prettify the formula
	findUserRoot(Act,Root), 
	swritef(Result, 'start(%w) => %w', [Root, Formula]).

%%%%
%% Predecessors partition
%%%%

getPredsDispatch(Act, Control, Guards, Faults) :- 
	activity:getPredecessors(Act,RawControls), 
	findall(A,onFailure(Act,A,_), RawFaults), 
	flatten([RawControls,RawFaults],Raws),
	dispatch(Act, Raws, Control, Guards , Faults).

dispatch(_,[],[],[],[]). %% Empty List
dispatch(Root, [H|T], Control, Guards, [[H|SimFaults]|Faults]) :- 
	%% Fault branch pouring into normal activity flow
	tag(fault, H, error(Fault, FaultyAct)),
	\+ tag(fault, Root, error(Fault, FaultyAct)),
	getSimFaults(T, error(Fault, FaultyAct), SimFaults),
	removeList(SimFaults,T,RemovedTail),
	dispatch(Root, RemovedTail, Control, Guards, Faults),!.
dispatch(Root, [H|T], [H|Control], Guards, Faults) :- 
	%% Normal continuation (aka control)
	dispatch(Root, T, Control, Guards, Faults).

getSimFaults(List,Meta,Result) :- 
	findall(A,(member(A,List), tag(fault,A,Meta)), Result),!.


%% dispatch(Root, [H|T], Control, [[[H|Sim],NotSim]|Guards], Faults) :- 
%% 	%% Exclusive condition detected in the Predecessors flags
%% 	tag(guard, H ,condition(Var, Test, Val)), 
%% 	\+ tag(guard, Root, condition(Var, Test, Val)), 
%% 	getSimGuards(T, condition(Var, Test, Val), Sim), invert(Val, NotVal), 
%% 	getSimGuards(T, condition(Var, Test, NotVal), NotSim),
%% 	flatten([Sim,NotSim], ToDelete), 
%% 	removeList(ToDelete,T,RemovedTail),
%% 	dispatch(Root, RemovedTail, Control, Guards, Faults),!.


	
%% extractConditions([],[]).
%% extractConditions([H|Tail], [Conds|Others]) :- 
%% 	findall(Var,tag(guard,H,condition(Var,_,_)),Raw), sort(Raw,Conds),
%% 	extractConditions(Tail,Others).

%% partition([],_,[]).
%% partition(L,[],L).
%% partition(Acts,[[Cond]|Tail],L) :- 
%% 	partitionOncondition(Cond, Acts, Done, Rest),
%% 	removeConditionInList


%% (Hard) Computation

buildActFormula(Act, 'true') :- \+ path(_,Act), !. %% entry point
buildActFormula(Act,Formula) :-  %% normal case
	getPredsDispatch(Act, Control, Guards, Faults), 
	buildCtrlFormula(Act,Control,CtrlForm), 
	buildGuardFormula(Act,Guards,GuardForm),
	buildFaultFormula(Act,Faults,FaultForm),
	buildFinalFormula(CtrlForm,GuardForm, FaultForm, Formula),!.
buildActFormula(_, 'false'). %% unknown case (should never happend)

%%%%
%% Building pretty equations (helpers)
%%%%

buildRelation(Act, Pred, R) :- 
	waitFor(Act,Pred), findUserRoot(Pred,Root), 
	swritef(R,'end(%w)',[Root]).
buildRelation(Act, Pred, R) :- 
	isGuardedBy(Act, Pred, Var, Val), findUserRoot(Pred,PredRoot),
	( tag(guard, Act, condition(Var, Pred, Val)), findUserRoot(Var,VarRoot),
	  swritef(R,'(end(%w) & %w(%w))',[PredRoot,Val,VarRoot])
	| \+ tag(guard, Act, condition(Var, Pred, Val)), 
	  swritef(R,'end(%w)',[PredRoot])).
buildRelation(Act, Pred, R) :- 
	onFailure(Act, Pred, Fault), findUserRoot(Pred,Root),
	swritef(R,'fail(%w,%w)',[Root,Fault]).


buildCtrlFormula(_,[],'').
buildCtrlFormula(Root,L,Result) :- 
	mapOperation('&', Root, L, Result).

buildGuardFormula(_,[],'').
buildGuardFormula(Root,[[OnValList,OnNotValList]],Result) :-
	mapOperation('&', Root, OnValList, OnValForm),
	mapOperation('&', Root, OnNotValList, OnNotValForm),
	swritef(Result,'((%w) | (%w))',[OnValForm,OnNotValForm]), !.
buildGuardFormula(Root,[[OnValList,OnNotValList]|Tail],Result) :-
	mapOperation('&', Root, OnValList, OnValForm),
	mapOperation('&', Root, OnNotValList, OnNotValForm),
	buildGuardFormula(Root,Tail,Others),
	swritef(Result,'((%w) | (%w)) & %w',[OnValForm,OnNotValForm,Others]).

buildFaultFormula(_,[],'').
buildFaultFormula(Root,[List],Result) :- 
	mapOperation('&',Root,List,Result), !.
buildFaultFormula(Root,[List|Tail],Result) :- 
	mapOperation('&',Root,List, This),
	buildFaultFormula(Root,Tail,Others), 
	swritef(Result,'%w & %w',[This, Others]).


buildFinalFormula(Control, '', '', Control) :- !.
buildFinalFormula('', Guards, '', Guards) :- !.
buildFinalFormula('', '', Faults, Faults) :- !.
buildFinalFormula(Control, Guards, '', Result) :- 
	swritef(Result,'%w & %w',[Control, Guards]), !.
buildFinalFormula(Control, '', Faults, Result) :- 
	swritef(Result,'(%w) | (%w)',[Control, Faults]), !.
buildFinalFormula(Control, Guards, Faults, Result) :- 
	swritef(Result,'(%w & %w) | (%w)',[Control, Guards, Faults]).
	

%%%%
%% Helper:
%%%%	

mapOperation(_,_,[],'').
mapOperation(_, Root, [Act], Result) :- 
	buildRelation(Root,Act,Result), !.
mapOperation(Op, Root, [Act|Tail], Result) :- 
	buildRelation(Root,Act, This), mapOperation(Op, Root, Tail, Others),
	swritef(Result,'%w %w %w',[This, Op, Others]), !.

invert(true,false).
invert(false,true).

dumpList([],'') :- !.
dumpList([H|T], Text) :- 
	dumpList(T,TailText), swritef(Text, "%w\n%w", [H,TailText]).
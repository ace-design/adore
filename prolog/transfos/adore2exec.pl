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
	findall(T,(process:exists(P),adore2exec:genExecutionSemantic(P,T)), A),
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

getPredsPartition(Act, Control, Guards, Faults) :- 
	activity:getPredecessors(Act,Raws), 
	partition(Act, Raws, Control, Guards , Faults).

partition(_,[],[],[],[]). %% Empty List
partition(Root, [H|T], Control, [[[H|Sim],NotSim]|Guards], Faults) :- 
	%% Exclusive condition detected in the Predecessors flags
	tag(guard, H ,condition(Var, Test, Val)), 
	\+ tag(guard, Root, condition(Var, Test, Val)), 
	getSimGuards(T, condition(Var, Test, Val), Sim), invert(Val, NotVal), 
	getSimGuards(T, condition(Var, Test, NotVal), NotSim),
	flatten([Sim,NotSim], ToDelete), 
	removeList(ToDelete,T,RemovedTail),
	partition(Root, RemovedTail, Control, Guards, Faults),!.
partition(Root, [H|T], Control, Guards, [H|Faults]) :- 
	%% Fault branch pouring into normal activity flow
	tag(fault, H, error(Fault, FaultyAct)),
	\+ tag(fault, Root, error(Fault, FaultyAct)),
	partition(Root, T, Control, Guards, Faults),!.
partition(Root, [H|T], [H|Control], Guards, Faults) :- 
	%% Normal continuation (aka control)
	partition(Root, T, Control, Guards, Faults).

getSimGuards(List,Meta,Result) :- 
	findall(A,(member(A,List), tag(guard,A,Meta)),Result),!.

%% (Hard) Computation

buildActFormula(Act, 'true') :- \+ path(_,Act), !. %% entry point
buildActFormula(Act,Formula) :-  %% normal case
	getPredsPartition(Act, Control, Guards, Faults), 
	buildCtrlFormula(Act,Control,CtrlForm), 
	buildGuardFormula(Act,Guards,GuardForm),
	buildFaultFormula(Act,Faults,FaultForm),
	swritef(Formula, '%w , %w , %w',  [CtrlForm, GuardForm, FaultForm]).
buildActFormula(_, 'false'). %% unknown case (should never happend)


buildCtrlFormula(_,_,'').
%% buildCtrlFormula(Act,[H],R) :- buildRelation(Act,H,R),!.
%% buildCtrlFormula(Act,[H|T],R) :- 
%% 	buildRelation(Act,H,This), buildCtrlFormula(Act,T,Others), 
%% 	swritef(R,'%w & %w',[This,Others]).

buildGuardFormula(_,_,'').
%% buildGuardFormula(Act,[ExcList],R) :- 
%% 	buildExclusiveList(Act,ExcList,R),!.
%% buildGuardFormula(Act,[H|T],R) :- 
%% 	buildExclusiveList(Act,H, This), buildGuardFormula(Act,T,Others),
%% 	swritef(R,'(%w) & %w',[This,Others]).

buildFaultFormula(_,_,'').
%% buildFaultformula(Act,[H],R) :- 
%% 	buildRelation(Act,H,R),!.
%% buildFaultformula(Act,[H|T],R) :- 
%% 	buildRelation(Act,H, This), buildFaultFormula(Act,T,Others), 
%% 	swritef(R, '%w | %w', [This,Others]).


%% buildExclusiveList(_,[],'').
%% buildExclusiveList(Act,[H],R) :- buildRelation(Act,H,R),!.
%% buildExclusiveList(Act,[H|T],R) :- 
%% 	buildRelation(Act,H,R), buildExclusiveList(Act,T,Others), 
%% 	swritef(R,'%w | %w',

buildRelation(Act, Pred, R) :- 
	waitFor(Act,Pred), findUserRoot(Pred,Root), 
	swritef(R,'end(%w)',[Root]).
buildRelation(Act, Pred, R) :- 
	isGuardedBy(Act, Pred, Var, Val), 
	findUserRoot(Pred,PredRoot), findUserRoot(Var,VarRoot),
	swritef(R,'(end(%w) & %w(%w))',[PredRoot,Val,VarRoot]).
buildRelation(Act, Pred, R) :- 
	onFailure(Act, Pred, Fault), findUserRoot(Pred,Root),
	swritef(R,'fail(%w,%w)',[Root,Fault]).
	




%%%%
%% Building pretty equations (helpers)
%%%%



%%%%
%% Helper:
%%%%

invert(true,false).
invert(false,true).

dumpList([],'') :- !.
dumpList([H|T], Text) :- 
	dumpList(T,TailText), swritef(Text, "%w\n%w", [H,TailText]).
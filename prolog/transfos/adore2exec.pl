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

%%%%%%%%%%%%%%%%%%%%%%%
%%% Syntactic Sugar %%%
%%%%%%%%%%%%%%%%%%%%%%%

genExecForAllProcess(Text) :- 
	findall(T,(process:exists(P),adore2exec:genExecutionSemantic(P,T)), A),
	dumpList(A,Text).

genExecutionSemantic(Process, Text) :-  
	doTag(Process), 
	findall(X, adore2exec:genActFormula(Process,X), FormulaList),
	clearTags, dumpList(FormulaList, FormulaText), 
	swritef(Text, '// %w \n%w\n',[Process, FormulaText]).


genActFormula(Process,Result) :- 
	activity:belongsTo(Act, Process), buildActFormula(Act, Formula),
	findUserRoot(Act,Root), 
	swritef(Result, 'start(%w) => %w', [Root, Formula]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Formula Building %%%
%%%%%%%%%%%%%%%%%%%%%%%%

buildActFormula(Act, Formula) :-
	flowDispatch(Act, Ctrl, Alts),
	Formula = ''.

%%%%%%%%%%%%%%%%%%%%%
%%% Flow Dispatch %%%
%%%%%%%%%%%%%%%%%%%%%

flowDispatch(Act, Control, Alternatives) :- 
	%% retrieving all the predecessors
	activity:getPredecessors(Act,RawControls), 
 	findall(A, onFailure(Act,A,_), RawFaults), 
	flatten([RawControls,RawFaults], Raws),
	%% retrieving the weak wait flow (part of Alternatives)
	findall(W,weakWait(Act,W),Weaks), removeList(Weaks, Raws, WithoutWeak),
	%% retrieving exceptional flow (part of Alternatives)
	findall(E,( tag(fault, E, error(Fault, FaultyAct)),
	            \+ tag(fault, Act, error(Fault, FaultyAct))), Exceptions),
	flatten([Weaks, Exceptions], Alternatives),
	%% Retrieving the control-flow (the rest).
	removeList(Exceptions, WithoutWeak, Control).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Control Partition (exclusivity induced by guards) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getConditions(Activities, Conditions) :- 
	findall(C, (member(A,Activities), getActConditionTags(A,C)), Raws),
	sort(Raws, WithoutDuplicates), 
	sortByLength(WithoutDuplicates, Conditions).

getActConditionTags(Act, Tags) :- 
	findall(C, tag(guard, Act, condition(C,_,_)), Raws),
	sort(Raws, Tags).

partition(Acts, Var, OnVar, OnNotVar, Rest) :- 
	findall(A,
	        ( member(A,Acts),tag(guard, A, condition(Var,_,true)) ),
		OnVar),
	findall(A,
	        ( member(A,Acts),tag(guard, A, condition(Var,_,false)) ),
		OnNotVar),
	flatten([OnVar, OnNotVar], Guarded), removeList(Guarded, Acts, Rest).

%%%%%%%%%%%%%%%%%%%%%
%%% Formula Model %%%
%%%%%%%%%%%%%%%%%%%%%
%% formula(and|or,Formula1, Formula2).

%% formula simplification
simplify(V,V) :- atom(V), !.
simplify([V],V) :- atom(V), !.
simplify(formula(_, Form, []),R) :- simplify(Form, R),!.
simplify(formula(_, [], Form),R) :- simplify(Form, R),!.
simplify(formula(Op, Form1, Form2), formula(Op, R1, R2)) :- 
	simplify(Form1, R1), simplify(Form2, R2).

%% formula display
%% TODO


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

dumpList([],'') :- !.
dumpList([H|T], Text) :- 
	dumpList(T,TailText), swritef(Text, "%w\n%w", [H,TailText]).

sortByLength([],[]).
sortByLength([H|T],R) :- 
	member(L,T), length(L,Other), length(H,Mine), Mine > Other, !,
	append(T,[H],Tmp), sortByLength(Tmp,R).
sortByLength([H|T], [H|R]) :- sortByLength(T,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OLD CODE, DO NOT EDIT !!!!! %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% %%%%
%% %% Predecessors partition
%% %%%%

%% getPredsPartition(Act, Control, Guards, Faults) :- 
%% 	activity:getPredecessors(Act,RawControls), 
%% 	findall(A,onFailure(Act,A,_), RawFaults), 
%% 	flatten([RawControls,RawFaults],Raws),
%% 	partition(Act, Raws, Control, Guards , Faults).

%% partition(_,[],[],[],[]). %% Empty List
%% partition(Root, [H|T], Control, [[[H|Sim],NotSim]|Guards], Faults) :- 
%% 	%% Exclusive condition detected in the Predecessors flags
%% 	tag(guard, H ,condition(Var, Test, Val)), 
%% 	\+ tag(guard, Root, condition(Var, Test, Val)), 
%% 	getSimGuards(T, condition(Var, Test, Val), Sim), invert(Val, NotVal), 
%% 	getSimGuards(T, condition(Var, Test, NotVal), NotSim),
%% 	flatten([Sim,NotSim], ToDelete), 
%% 	removeList(ToDelete,T,RemovedTail),
%% 	partition(Root, RemovedTail, Control, Guards, Faults),!.
%% partition(Root, [H|T], Control, Guards, [[H|SimFaults]|Faults]) :- 
%% 	%% Fault branch pouring into normal activity flow
%% 	tag(fault, H, error(Fault, FaultyAct)),
%% 	\+ tag(fault, Root, error(Fault, FaultyAct)),
%% 	getSimFaults(T, error(Fault, FaultyAct), SimFaults),
%% 	removeList(SimFaults,T,RemovedTail),
%% 	partition(Root, RemovedTail, Control, Guards, Faults),!.
%% partition(Root, [H|T], [H|Control], Guards, Faults) :- 
%% 	%% Normal continuation (aka control)
%% 	partition(Root, T, Control, Guards, Faults).

%% getSimGuards(List,Meta,Result) :- 
%% 	findall(A,(member(A,List), tag(guard,A,Meta)),Result),!.

%% getSimFaults(List,Meta,Result) :- 
%% 	findall(A,(member(A,List), tag(fault,A,Meta)), Result),!.

%% %% (Hard) Computation

%% buildActFormula(Act, 'true') :- \+ path(_,Act), !. %% entry point
%% buildActFormula(Act,Formula) :-  %% normal case
%% 	getPredsPartition(Act, Control, Guards, Faults), 
%% 	buildCtrlFormula(Act,Control,CtrlForm), 
%% 	buildGuardFormula(Act,Guards,GuardForm),
%% 	buildFaultFormula(Act,Faults,FaultForm),
%% 	buildFinalFormula(CtrlForm,GuardForm, FaultForm, Formula),!.
%% buildActFormula(_, 'false'). %% unknown case (should never happend)

%% %%%%
%% %% Building pretty equations (helpers)
%% %%%%

%% buildRelation(Act, Pred, R) :- 
%% 	waitFor(Act,Pred), findUserRoot(Pred,Root), 
%% 	swritef(R,'end(%w)',[Root]).
%% buildRelation(Act, Pred, R) :- 
%% 	isGuardedBy(Act, Pred, Var, Val), 
%% 	findUserRoot(Pred,PredRoot), findUserRoot(Var,VarRoot),
%% 	swritef(R,'(end(%w) & %w(%w))',[PredRoot,Val,VarRoot]).
%% buildRelation(Act, Pred, R) :- 
%% 	onFailure(Act, Pred, Fault), findUserRoot(Pred,Root),
%% 	swritef(R,'fail(%w,%w)',[Root,Fault]).


%% buildCtrlFormula(_,[],'').
%% buildCtrlFormula(Root,L,Result) :- 
%% 	mapOperation('&', Root, L, Result).

%% buildGuardFormula(_,[],'').
%% buildGuardFormula(Root,[[OnValList,OnNotValList]],Result) :-
%% 	mapOperation('&', Root, OnValList, OnValForm),
%% 	mapOperation('&', Root, OnNotValList, OnNotValForm),
%% 	swritef(Result,'((%w) | (%w))',[OnValForm,OnNotValForm]), !.
%% buildGuardFormula(Root,[[OnValList,OnNotValList]|Tail],Result) :-
%% 	mapOperation('&', Root, OnValList, OnValForm),
%% 	mapOperation('&', Root, OnNotValList, OnNotValForm),
%% 	buildGuardFormula(Root,Tail,Others),
%% 	swritef(Result,'((%w) | (%w)) & %w',[OnValForm,OnNotValForm,Others]).

%% buildFaultFormula(_,[],'').
%% buildFaultFormula(Root,[List],Result) :- 
%% 	mapOperation('&',Root,List,Result), !.
%% buildFaultFormula(Root,[List|Tail],Result) :- 
%% 	mapOperation('&',Root,List, This),
%% 	buildFaultFormula(Root,Tail,Others), 
%% 	swritef(Result,'%w & %w',[This, Others]).


%% buildFinalFormula(Control, '', '', Control) :- !.
%% buildFinalFormula(Control, Guards, '', Result) :- 
%% 	swritef(Result,'%w & %w',[Control, Guards]), !.
%% buildFinalFormula(Control, '', Faults, Result) :- 
%% 	swritef(Result,'(%w) | (%w)',[Control, Faults]), !.
%% buildFinalFormula(Control, Guards, Faults, Result) :- 
%% 	swritef(Result,'(%w & %w) | (%w)',[Control, Guards, Faults]).
	

%% %%%%
%% %% Helper:
%% %%%%	

%% mapOperation(_,_,[],'').
%% mapOperation(_, Root, [Act], Result) :- 
%% 	buildRelation(Root,Act,Result), !.
%% mapOperation(Op, Root, [Act|Tail], Result) :- 
%% 	buildRelation(Root,Act, This), mapOperation(Op, Root, Tail, Others),
%% 	swritef(Result,'%w %w %w',[This, Op, Others]), !.

%% invert(true,false).
%% invert(false,true).


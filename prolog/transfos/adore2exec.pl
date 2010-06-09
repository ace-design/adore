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

%%%%%%%%%%%%%%%%%%%%%%%
%%% Syntactic Sugar %%%
%%%%%%%%%%%%%%%%%%%%%%%

genExecForAllProcess(Text) :- 
	findall(T,(process(P),adore2exec:genExecutionSemantic(P,T)), A),
	dumpList(A,Text).

genExecutionSemantic(Process, '') :- 
	process(Process), process:getActivities(Process,[]),!.
genExecutionSemantic(Process, Text) :-  
	doTag(Process), 
	findall(X, adore2exec:genActFormula(Process,X), FormulaList),
	clearTags, dumpList(FormulaList, FormulaText), 
	swritef(Text, '// %w \n%w\n',[Process, FormulaText]).

genActFormula(Process,Result) :- 
	activity:belongsTo(Act, Process), 
	buildActFormula(Act, Formula),
	buildActName(Act,Root), display(Act, Formula, FinalFormula),
	swritef(Result, 'start(%w) => %w', [Root, FinalFormula]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Formula Building %%%
%%%%%%%%%%%%%%%%%%%%%%%%

buildActFormula(Act, 'true') :-	isEntryPoint(Act),!.
buildActFormula(Act, Formula) :-
	flowDispatch(Act, Ctrl, Weaks, Faults), 
	partitionByBranch(Act, Faults, Ctrl, Branches),
	buildBranchesFormulas(Act, Branches, BranchesFormulas),
	%% Weak Flow Formula
	build(or, Weaks, WeakFormula),
	Raw = formula(and, BranchesFormulas, WeakFormula),
	simplify(Raw, Formula).

%% Branch Partition:
partitionByBranch(_,[],Ctrl,[branch([],Ctrl)]). %% no faults partition
partitionByBranch(Act, [H|T], Ctrl, [Branch|Others]) :-
	extractFaultBranch(Act, H, Ctrl, BranchActs),
	removeList(BranchActs, Ctrl, Nexts), 
	partitionByBranch(Act,T,Nexts,Others), 
	Branch = branch(H,BranchActs).

extractFaultBranch(Root, FaultPartition, CtrlPreds, Branch) :- 
	getFaultPartitionOrigin(Root,FaultPartition, O),
	findall(A, (member(A,CtrlPreds), relations:existsPath(O,A)), Branch).

getFaultPartitionOrigin(Root,Partition, Origin) :- 
	member(F,Partition),!, 
	(tag(fault,F,error(_,Origin)) | onFailure(Root,Origin,_), F = Origin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Branch Formula Building %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buildBranchesFormulas(_, [],[]).
buildBranchesFormulas(Act,[branch(Faults,Ctrl)|Others], Formula) :-
	%% Control Flow Formula:
	getConditions(Ctrl, Conds), 
	buildControlFormula(Act, Ctrl, Conds, CtrlFormula),
	%% Exceptional Flow Formula:
	buildFaultsFormula([Faults], FaultFormula), 
	%% Current Formula: Ctrl|Fault
	Raw = formula(or, CtrlFormula, FaultFormula),
	simplify(Raw,CurrentForm),
	%% Rest
	buildBranchesFormulas(Act, Others, OthersFormula),
	Formula = formula(and, CurrentForm, OthersFormula).

%% Control
buildControlFormula(_,[Act], _,Act) :- activity(Act),!. 
buildControlFormula(_,Activities, [], Formula) :- 
	build(and, Activities, Formula).
buildControlFormula(Root, Activities, [[]|Tail], Formula) :- 
	buildControlFormula(Root, Activities,Tail, Formula).
buildControlFormula(Root, Activities, [[Cond|Others]|Tail], Formula) :- 
	partition(Root, Activities, Cond, OnCond, OnNotCond, Rest),
	pushOut(Cond, Tail, NewTail), Nexts = [Others|NewTail],
	buildControlFormula(Root, OnCond, Nexts, OnCondForm),
	buildControlFormula(Root, OnNotCond, Nexts, OnNotCondForm),
	buildControlFormula(Root, Rest, Nexts, RestForm),
	RawForm = formula(and, formula(or,OnCondForm,OnNotCondForm),RestForm),
	simplify(RawForm, Formula).
	
%% Exceptions
buildFaultsFormula([],[]).
buildFaultsFormula([H|T],Formula) :- 
	build(and,H,Conjunction), buildFaultsFormula(T,Others),
	Raw = formula(or, Conjunction, Others),	simplify(Raw,Formula).
	
%% build a relationship according to a given root (end, fail, condition?)
buildRelation(Act, Pred, R) :- 
	(waitFor(Act,Pred)|weakWait(Act,Pred)), buildActName(Pred,Root), 
	swritef(R,'end(%w)',[Root]).
buildRelation(Act, Pred, R) :- 
 	isGuardedBy(Act, Pred, Var, Val), buildActName(Pred,PredRoot),
	getPreviousName(Var,VarRoot), 
	swritef(R,'(end(%w) & %w(%w))', [PredRoot,Val,VarRoot]).
buildRelation(Act, Pred, R) :- 
	onFailure(Act, Pred, Fault), buildActName(Pred,Root),
	swritef(R,'fail(%w,%w)',[Root,Fault]).

buildActName(A,'^') :- activity(A), hasForKind(A,predecessors),!.
buildActName(A,'$') :- activity(A), hasForKind(A,successors),!.
buildActName(A,N) :- getPreviousName(A,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Flow Dispatch & Entry Point %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractAllPredecessors(Act,Preds) :- 
	activity:getPredecessors(Act,RawControls), 
 	findall(A, onFailure(Act,A,_), RawFaults), 
	flatten([RawControls,RawFaults], Preds).

extractException(Root, Acts, Exc) :- 
 	member(Exc, Acts), onFailure(Root, Exc, _).
extractException(Root, Acts, Exc) :- 
	member(Exc, Acts), tag(fault, Exc, error(Fault, FaultyAct)),
	\+ tag(fault, Root, error(Fault, FaultyAct)).
extractException(_, Acts, Exc) :- 
	member(Exc, Acts), member(Exc2, Acts),
	tag(fault, Exc, error(Fault1, FaultyAct)),
	tag(fault, Exc2, error(Fault2, FaultyAct)), Fault1 \= Fault2.
	
flowDispatch(Act, Control, Weaks, PartitionnedExceptions) :- 
	extractAllPredecessors(Act,Raws),
	%% retrieving the weak wait flow 
	findall(W,weakWait(Act,W),Weaks), 
	removeList(Weaks, Raws, WithoutWeak),
	%% retrieving exceptional flow 
	findall(E,extractException(Act,WithoutWeak,E), RawExceptions),
	sort(RawExceptions,Exceptions),
	faultPartition(Act,Exceptions, PartitionnedExceptions),
	%% Interpolating the control-flow (everything else).
	removeList(Exceptions, WithoutWeak, Control),!.

isEntryPoint(Act) :- activity(Act), \+ relations:path(_,Act).

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

partition(Root, Acts, Var, OnVar, OnNotVar, Rest) :- 
	findall(A,
	        adore2exec:belongsToPartition(Root, Acts, Var, true, A),
	        %( member(A,Acts), tag(guard, A, condition(Var,_,true)) ),
		OnVar),
	findall(A,
	        adore2exec:belongsToPartition(Root, Acts, Var, false, A),
	        % ( member(A,Acts), tag(guard, A, condition(Var,_,false)) ),
		OnNotVar),
	flatten([OnVar, OnNotVar], Guarded), removeList(Guarded, Acts, Rest).

belongsToPartition(Root, Acts, Var, Value, A) :- 
        member(A,Acts), isGuardedBy(Root, A, Var, Value).
belongsToPartition(_, Acts, Var, Value, A) :- 
	member(A,Acts), tag(guard, A, condition(Var,_,Value)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception Partition (exclusivity induced by Faults) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

faultPartition(_,[],[]).
faultPartition(Root,[H|T],[Currents|Others]) :- 
	onFailure(Root, H,_), Currents = [H],
	faultPartition(Root, T,Others).
faultPartition(Root,[H|T],[Currents|Others]) :- 
	tag(fault, H, error(Fault, Source)), 
	findall(S, (member(S,T), tag(fault, S, error(Fault, Source))), Sims),
	sort([H|Sims],Currents), removeList(Currents,T,WithoutSims), 
	faultPartition(Root,WithoutSims,Others).

	

%%%%%%%%%%%%%%%%%%%%%
%%% Formula Model %%%
%%%%%%%%%%%%%%%%%%%%%

%% 'Grammar': formula(and|or,SubFormula1, SubFormula2) | Atom.

%% formula simplification (term rewriting):
rewrite(_,[],F,F) :- !.
rewrite(_,F,[],F) :- !.
rewrite(Op,F1,F2,formula(Op,F1,F2)).

simplify(V,V) :- atom(V), !.
simplify([V],V) :- atom(V), !.
simplify(formula(Op, Form1, Form2), Result) :- 
	simplify(Form1, R1), simplify(Form2, R2), rewrite(Op,R1,R2,Result).

%% formula builder:
build(_,[],[]).
build(Op,[H|T],formula(Op,H,R)) :- build(Op,T,R).

%% formula display, according to a root activity:
display(_, 'true', 'true').
display(Root, Activity, Result) :- 
	activity(Activity), !, buildRelation(Root, Activity, Result).
display(Root, formula(Op,SF1,SF2), R) :- 
	display(Root, SF1, D1),	display(Root, SF2, D2), operator(Op,Dop),
	addParenthesis(SF1,D1,PD1), addParenthesis(SF2,D2,PD2), 
	swritef(R,'%w %w %w',[PD1,Dop,PD2]).

%% add parenthesis, only when needed
addParenthesis(formula(_,_,_), Text, P) :- !, swritef(P,'(%w)',[Text]).
addParenthesis(_,Text,Text).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

dumpList([],'').
dumpList([''|T], Text) :- dumpList(T,Text),!.
dumpList([H|T], Text) :- 
	dumpList(T,TailText), swritef(Text, "%w\n%w", [H,TailText]).

sortByLength([],[]).
sortByLength([H|T],R) :- 
	member(L,T), length(L,Other), length(H,Mine), Mine > Other, !,
	append(T,[H],Tmp), sortByLength(Tmp,R).
sortByLength([H|T], [H|R]) :- sortByLength(T,R).

operator(and,'&').
operator(or, '|').

pushOut(_,[],[]).
pushOut(Cond, [Head|Tail], [NewHead|NewTail]) :- 
	remove(Cond, Head, NewHead), pushOut(Cond, Tail, NewTail).

invert(true, false).
invert(false, true).

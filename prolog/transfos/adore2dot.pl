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
:- module(adore2dot,[adore2dot/2, adore2dotfile/2]).

%%%%
%% Main interface with the transformation
%%%%
adore2dotfile(P,F) :- 
	adore2dot(P,Code), 
	open(F,write,Stream), write(Stream,Code), close(Stream).

%% adore2dot(+Process, -Code): generate DOT Code associated with Process
adore2dot(P,R) :-
	process(P), genCore(P,Core),
	swritef(R,'digraph %w {\n  fontname=Courier;\n  node [shape=record];\n  edge [fontname=Courier];\n%w } \n',[P,Core]),!.


%%%%
%% Graph Label (pretty name printed as graph legend)
%%%%

genGraphLabel(P,R) :- 
	isFragment(P), 
	genFragLabel(P,Label),
	swritef(R,'label="Fragment %w"',[Label]).
genGraphLabel(P,R) :- 
	hasForSrvName(P,S),
	hasForOpName(P,O),
	swritef(R,'label="Orchestration %w::%w"',[S,O]).

genFragLabel(P,P) :- findall(X,hasForParameter(P,X),[]).
genFragLabel(P,R) :- 
	findall(X,hasForParameter(P,X),L),
	genFragParamList(L,Params),
	swritef(R,"%w<%w>",[P,Params]).

genFragParamList([],'').
genFragParamList([H],R) :- swritef(R,'%w',[H]), !. 
genFragParamList([H|T],R) :- 
	genFragParamList(T,Others),
	swritef(R,'%w, %w',[H,Others]).

%%%%
%% Graph Core (aka label, nodes and edges)
%%%%
genCore(P,R) :- 
	genGraphLabel(P,L),
	genClusters(P,Clusters),
	genActivities(P,Acts),
	genOrders(P,Orders),
	concatenate([L,Clusters,Acts,Orders],R).

%%%%
%% Clusters (aka iterations)
%%%%

identifyClusters(Process,Clusters) :- 
	findall(C,isClusterOf(Process,C),List),
	sort(List,Clusters).

isClusterOf(P,C) :- 
	activity(A), isContainedBy(A,P), iteratesOver(A,C).

genClusters(Process,Code) :- 
	identifyClusters(Process,Clusters),
	map(adore2dot:genCluster,Clusters, List),
	concatenate(List,Code).

genCluster(ClusterId,Code) :- 
	genClusterActivities(ClusterId,Acts),
	genClusterLabel(ClusterId,Fin,Fout),
	swritef(Legend, 'legend_%w [style=filled, fillcolor=lightgrey,label="{%w|%w}"] ;',[ClusterId,Fin,Fout]),
	swritef(Code,'    subgraph cluster_%w {\n label=""; \n%w    %w\n}\n',[ClusterId, Acts, Legend]).

genClusterActivities(ClusterId,Code) :- 
	findall(X,genClusterActivity(ClusterId,X),List),
	concatenate(List,Code).
genClusterActivity(ClusterId,Code) :- 
	activity(Act), iteratesOver(Act,ClusterId), 
	drawActivity(Act,Code).

genClusterLabel(ClusterId,LabelIn,LabelOut) :- 
	policy(ClusterId,Fin,Fout),
	genFormula(Fin,LabelIn),
	genFormula(Fout,LabelOut).

genFormula(forall(V,Vstar),Label) :- 
	genVarLabel(V,Vlabel),
	genVarLabel(Vstar,VstarLabel),
	swritef(Label,  "forall %w in   %w", [Vlabel,VstarLabel]).

genFormula(append(V,Vstar),Label) :- 
	genVarLabel(V,Vlabel),
	genVarLabel(Vstar,VstarLabel),
	swritef(Label,  "append %w into %w", [Vlabel,VstarLabel]).

genFormula(none,'--').

%%%%
%% Activities:
%%%%
genActivities(P,C) :- 
	findall(X, genActivity(P,X), List),
	concatenate(List,C).
genActivity(P,C) :- 
	isContainedBy(Act,P), activity(Act), \+ iteratesOver(Act,_),
	drawActivity(Act,C).

drawActivity(A,C) :- 
	hasForKind(A,predecessors), 
	swritef(C,'  %w [label="P", shape=doublecircle];',[A]), !.
drawActivity(A,C) :- 
	hasForKind(A,successors), 
	swritef(C,'  %w [label="S", shape=doublecircle];',[A]), !.
drawActivity(A,C) :- 
	getPreviousName(A,Id),
	genOutputs(A,Outs),
	genLabel(A,Label),
	genInputs(A,Ins), genActColor(A, Color), 
	swritef(C,'  %w [label="%w|%w%w%w"%w];',[A,Id,Outs,Label,Ins,Color]).

genActColor(Act,'') :- hasForKind(Act,hook),!.
genActColor(Act,Color) :- 
	findRoot(Act,Root), activity:belongsTo(Root,P), 
	getProcessColor(P,Color).

getProcessColor(P,Color) :- 
	hasForColor(P,ColorName),!,
	swritef(Color,',style=filled,fillcolor="%w"',[ColorName]).
getProcessColor(_,'').

	

genOutputs(A,'') :- 
	findall(V,usesAsOutput(A,V),[]),!.
genOutputs(A,R) :- 
	findall(V,usesAsOutput(A,V),Outs),
	genVarList(A,Outs,Tmp),
	swritef(R,'(%w) := ',[Tmp]).

genInputs(A,'()') :- 
	findall(V,usesAsInput(A,V),[]),!.
genInputs(A,R) :- 
	findall(V,usesAsInput(A,V),Ins),
	genVarList(A,Ins,Tmp),
	swritef(R,'(%w)',[Tmp]).

genLabel(A,R) :- 
	hasForKind(A,invoke), !,
	hasForService(A,S), hasForOperation(A,O),
	swritef(R,"%w::%w",[S,O]).
genLabel(A,R) :- 
	hasForKind(A,assign), 
	hasForFunction(A,R),!.
genLabel(A,'') :- 
	hasForKind(A,assign).
genLabel(A,K) :- hasForKind(A,K). 

%%%%
%% Variables
%%%%

genVarList(_,[],'').
genVarList(A,[H],R) :- 
	genVar(A,H,R),!.
genVarList(A,[H|T],R) :- 
	genVar(A,H,V),
	genVarList(A,T,Others),
	swritef(R,"%w,%w",[V,Others]).

genVar(A,V,R) :- 
	usesAsBinding(A,V,Part),
	isConstant(V), hasForInitValue(V,Val),
	swritef(R,'%w: \'%w\'',[Part,Val]), !.
genVar(A,V,R) :- 
	usesAsBinding(A,V,Part),
	genVarLabel(V,Label),
	swritef(R,'%w: %w',[Part,Label]), !.
genVar(_,V,R) :- 
	isConstant(V), hasForInitValue(V,Val),
	swritef(R,'\'%w\'',[Val]),!.
genVar(_,V,R) :- 
	genVarLabel(V,R).

%% A var label is it's user readable name


%%%%
%% Orders: 
%%%%
genOrders(P,C) :- 
	findall(X,genOrder(P,X), List),
	concatenate(List,C).
genOrder(P,C) :- 
	isContainedBy(Left,P), isContainedBy(Rght,P), 
	\+ Left == Rght, 
	drawOrder(Left,Rght,C).

drawOrder(L,R,C) :- 
	waitFor(R,L),
	swritef(C,'  %w -> %w ;',[L,R]).
drawOrder(L,R,C) :- 
	weakWait(R,L),
	swritef(C,'  %w -> %w [style=dashed,arrowhead=odot];',[L,R]).
drawOrder(L,R,C) :- 
	isGuardedBy(R,L,V,true), getPreviousName(V,Label),
	swritef(C,'  %w -> %w [label="%w"];',[L,R,Label]).
drawOrder(L,R,C) :- 
	isGuardedBy(R,L,V,false), getPreviousName(V,Label),
	swritef(C,'  %w -> %w [label="!%w"];',[L,R,Label]).
drawOrder(L,R,C) :- 
	onFailure(R,L,E),
	swritef(C,'  %w -> %w [color="red",label="%w"];',[L,R,E]).




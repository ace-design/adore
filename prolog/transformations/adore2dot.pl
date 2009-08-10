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

%%%%
%% Main Process
%%%%

adore2dot(P,R) :- 
	process(P), 
	adore2dot_genCore(P,Core),
	swritef(R,'digraph %w {\n  fontname=Courier;\n  node [shape=record];\n  edge [fontname=Courier];\n%w } \n',[P,Core]),!.

adore2dot_genGraphLabel(P,R) :- 
	isFragment(P), 
	swritef(R,'label="Fragment %w"',[P]).
adore2dot_genGraphLabel(P,R) :- 
	hasForSrvName(P,S),
	hasForOpName(P,O),
	swritef(R,'label="Orchestration %w::%w"',[S,O]).

adore2dot_genCore(P,R) :- 
	adore2dot_genGraphLabel(P,L),
	adore2dot_genActivities(P,Acts),
	adore2dot_genOrders(P,Orders),
	concatenate([L,Acts,Orders],R).

%%%%
%% Activities:
%%%%
adore2dot_genActivities(P,C) :- 
	findall(X, adore2dot_genActivity(P,X), List),
	concatenate(List,C).
adore2dot_genActivity(P,C) :- 
	isContainedBy(Act,P), activity(Act),
	adore2dot_drawActivity(Act,C).

adore2dot_drawActivity(A,C) :- 
	hasForKind(A,predecessors), 
	swritef(C,'  %w [label="P", shape=doublecircle];',[A]), !.
adore2dot_drawActivity(A,C) :- 
	hasForKind(A,successors), 
	swritef(C,'  %w [label="S", shape=doublecircle];',[A]), !.
adore2dot_drawActivity(A,C) :- 
	getPreviousName(A,Id),
	adore2dot_genOutputs(A,Outs),
	adore2dot_genLabel(A,Label),
	adore2dot_genInputs(A,Ins),
	swritef(C,'  %w [label="%w|%w%w%w"];',[A,Id,Outs,Label,Ins]).

adore2dot_genOutputs(A,'') :- 
	findall(V,usesAsOutput(A,V),[]),!.
adore2dot_genOutputs(A,R) :- 
	findall(V,usesAsOutput(A,V),Outs),
	adore2dot_genVarList(A,Outs,Tmp),
	swritef(R,'(%w) := ',[Tmp]).

adore2dot_genInputs(A,'()') :- 
	findall(V,usesAsInput(A,V),[]),!.
adore2dot_genInputs(A,R) :- 
	findall(V,usesAsInput(A,V),Ins),
	adore2dot_genVarList(A,Ins,Tmp),
	swritef(R,'(%w)',[Tmp]).

adore2dot_genLabel(A,R) :- 
	hasForKind(A,invoke), !,
	hasForService(A,S), hasForOperation(A,O),
	swritef(R,"%w::%w",[S,O]).
adore2dot_genLabel(A,R) :- 
	hasForKind(A,assign), 
	hasForFunction(A,R),!.
adore2dot_genLabel(A,'') :- 
	hasForKind(A,assign).
adore2dot_genLabel(A,K) :- hasForKind(A,K). 

%%%%
%% Variables
%%%%

adore2dot_genVarList(_,[],'').
adore2dot_genVarList(A,[H],R) :- 
	adore2dot_genVar(A,H,R),!.
adore2dot_genVarList(A,[H|T],R) :- 
	adore2dot_genVar(A,H,V),
	adore2dot_genVarList(A,T,Others),
	swritef(R,"%w,%w",[V,Others]).

adore2dot_genVar(A,V,R) :- 
	usesAsBinding(A,V,Part),
	isConstant(V), hasForInitValue(V,Val),
	swritef(R,'%w: \'%w\'',[Part,Val]), !.
adore2dot_genVar(A,V,R) :- 
	usesAsBinding(A,V,Part),
	adore2dot_genVarLabel(V,Label),
	swritef(R,'%w: %w',[Part,Label]), !.
adore2dot_genVar(_,V,R) :- 
	isConstant(V), hasForInitValue(V,Val),
	swritef(R,'\'%w\'',[Val]),!.
adore2dot_genVar(_,V,R) :- 
	adore2dot_genVarLabel(V,R).

%% A var label is it's user readable name
adore2dot_genVarLabel(V,Label) :- 
	getVariable(V,Tmp), %% a variable, not an anonymous field access
	getPreviousName(Tmp,OldName), %% The REAL name, not the renamed one
	adore2dot_suffixToStar(OldName, PrettyName), %% '_star' <-> '*'
	adore2dot_genFields(V,Fields), %% fields as x.y.z
	swritef(Label,"%w%w",[PrettyName,Fields]). %% that's all folks

adore2dot_genFields(I,'') :- \+ fieldAccess(I,_,_), !.
adore2dot_genFields(I,R) :- fieldAccess(I,_,L), adore2dot_showAsPoint(L,R).

adore2dot_showAsPoint([],'').
adore2dot_showAsPoint([H],R) :-  
	adore2dot_suffixToStar(H,V),swritef(R,'.%w',[V]),!.
adore2dot_showAsPoint([H|T],R) :- 
	adore2dot_showAsPoint(T,O),
	swritef(R,".%w%w",[H,O]).

adore2dot_suffixToStar(V,R) :- 
	\+ is_list(V), string_to_list(V,L), adore2dot_suffixToStar(L,R).
adore2dot_suffixToStar([],'') :- !.
adore2dot_suffixToStar("_star",'*')  :- !.
adore2dot_suffixToStar([H|T],R) :- 
	string_to_list(C,[H]), adore2dot_suffixToStar(T,O),
	swritef(R,"%w%w",[C,O]).

%%%%
%% Orders: 
%%%%
adore2dot_genOrders(P,C) :- 
	findall(X,adore2dot_genOrder(P,X), List),
	concatenate(List,C).
adore2dot_genOrder(P,C) :- 
	isContainedBy(Left,P), activity(Left),
	isContainedBy(Rght,P), activity(Rght), 
	path(Left,Rght), adore2dot_drawOrder(Left,Rght,C).

adore2dot_drawOrder(L,R,C) :- 
	waitFor(R,L), 
	swritef(C,'  %w -> %w ;',[L,R]).
adore2dot_drawOrder(L,R,C) :- 
	weakWait(R,L), 
	swritef(C,'  %w -> %w [style=dashed,arrowhead=odot];',[L,R]).
adore2dot_drawOrder(L,R,C) :- 
	isGuardedBy(R,L,V,true), getPreviousName(V,Label),
	swritef(C,'  %w -> %w [label="%w"];',[L,R,Label]).
adore2dot_drawOrder(L,R,C) :- 
	isGuardedBy(R,L,V,false), getPreviousName(V,Label), 
	swritef(C,'  %w -> %w [label="!%w"];',[L,R,Label]).
adore2dot_drawOrder(L,R,C) :- 
	onFailure(R,L,'*'),!,
	swritef(C,'  %w -> %w [color="red"];',[L,R]).
adore2dot_drawOrder(L,R,C) :- 
	onFailure(R,L,E),
	swritef(C,'  %w -> %w [color="red",label="%w"];',[L,R,E]).




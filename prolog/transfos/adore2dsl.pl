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
:- module(adore2dsl,[universe2dsl/1, process2dsl/2]).

%% universe2dsl(-Code:string) is det.
% transform everything in the universe into its concrete syntax representation
% 
universe2dsl(Code) :- 
	findall(C,process2dsl(_,C),Codes),
	concatenate(Codes,Code).

%% process2dsl(+P:processId, -Code) is det.
% generate Adore code associated to the process identified by processId
%
process2dsl(P,Code) :- 
	process(P), genProcessLabel(P,Label),
	genVariables(P,Variables),
	genActivities(P,Activities),
	genRelations(P,Relations),
	swritef(Code,'%w {\n%w%w%w}', [Label,Variables,Activities,Relations]).


genProcessLabel(P,Code) :- 
	isFragment(P), !, swritef(Code,'fragment %w',[P]).
genProcessLabel(P,Code) :- 
	hasForSrvName(P,Srv), hasForOpName(P,Op),
	swritef(Code,'orchestration %w::%w', [Srv,Op]).

%%%%
%% Variables 
%%%%

genVariables(P,Code) :- 
	findall(V,usedByProcess(P,V),RawVariables),
	sort(RawVariables,Variables),
	map(adore2dsl:genVariable,Variables,VarCodes),
	concatenate(VarCodes,ValidCode),
	swritef(Code,'  variables {\n%w\n  }\n',[ValidCode]).

genVariable(V,Code) :-
	isConstant(V), !, genVarName(V,Name), 
	hasForInitValue(V,Value), hasForType(V,Type),
	swritef(Code,"    %w := '%w' as %w;",[Name,Value,Type]).
genVariable(Fid,'') :-
	fieldAccess(Fid,_,_), !.
genVariable(V,Code) :-
	genVarName(V,Name), hasForType(V,Type),
	swritef(Code,"    %w as %w;",[Name,Type]).

genVarName(V,Name) :-
	variable(V), getPreviousName(V,RawName),
	(isSet(V), string_concat(RawName,'*',Name) | Name = RawName).
	
%%%%
%% Activities 
%%%%

genActivities(P,Code) :- 
	findall(C,genActivity(P,C),Activities),
	concatenate(Activities,ActCode),
	swritef(Code,'  activities {\n%w\n  }\n',[ActCode]).

genActivity(P,Code) :- 
	activity(A), isContainedBy(A,P), hasForKind(A,K),
	K \= predecessors, K \= successors,
	getPreviousName(A,Name),
	genInputs(A,Inputs),
	genOutputs(A,Outputs),
	genKind(A,K,Kind),
	swritef(Code,'    %w. %w%w%w;',[Name,Outputs,Kind,Inputs]).
	
genKind(A,invoke,Code) :- 
	hasForService(A,S), hasForOperation(A,O),
        swritef(Code, '%w::%w',[S,O]), !.
genKind(A,assign,F) :- 
	hasForFunction(A,F),!.
genKind(_,K,K).

genInputs(A,Code) :- 
	activity(A), findall(V,usesAsInput(A,V),Variables), 
	( length(Variables,0), Code = '()' 
          | genVarListCode(Variables, VarCodes),
	    swritef(Code,'(%w)',[VarCodes])).

genOutputs(A,Code) :- 
	activity(A), findall(V,usesAsOutput(A,V),Variables), 
	( length(Variables,0), Code = '' 
          | genVarListCode(Variables, VarCodes),
	    swritef(Code,'(%w) := ',[VarCodes])).

genVarListCode([E],Code) :- 
	fieldAccess(E,V,Fields),!, getPreviousName(V,Root),
	concatenate(Fields,FieldAccess,'.'),
	swritef(Code,'%w.%w',[Root,FieldAccess]).

genVarListCode([E],Code) :- 
	getPreviousName(E,Code),!.
genVarListCode([H|T],R) :- 
	getPreviousName(H,Code),
	genVarListCode(T,Others),
	swritef(R,'%w,%w',[Code,Others]).

%%%%
%% Relations
%%%%

genRelations(P,Code) :- 
	findall(C,genRelation(P,C),Relations),
	concatenate(Relations,RelCode),
	swritef(Code,'  relations {\n%w\n  }\n',[RelCode]).

genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	waitFor(A,B), genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'    %w < %w ;', [Blabel, Alabel]).
genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	weakWait(A,B), genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'    %w << %w ;', [Blabel, Alabel]).
genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	isGuardedBy(A,B,V,C), getPreviousName(V,CN),
	genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	(C, swritef(Code,'    %w < %w when %w;', [Blabel, Alabel,CN])
         | \+ C, swritef(Code,'    %w < %w when !%w;', [Blabel, Alabel,CN])).
genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	onFailure(A,B,Cond),
	genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'    fail(%w,\'%w\') < %w',[Alabel,Cond,Blabel]).


genActRelLabel(A,'^') :- hasForKind(A,predecessors),!.
genActRelLabel(A,'$') :- hasForKind(A,successors),!.
genActRelLabel(A,N) :- getPreviousName(A,N).

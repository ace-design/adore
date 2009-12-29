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
:- module(adore2xml,
	  [dumpUniverseAsXml/1, process2xml/2, composition2xml/2]).

%% TODO: message binding
%% TODO: fragment parameters
%% TODO: variable field access

dumpUniverseAsXml(FileName) :- 
	universe2xml(Code),
	swritef(CompleteCode,'<?xml version="1.0" ?>\n%w',[Code]),
	open(FileName,write,Stream), write(Stream,CompleteCode), close(Stream).

%% universe2dsl(-Code:string) is det.
% transform everything in the universe into its concrete syntax representation
% 
universe2xml(Code) :- 
	findall(C,process2xml(_,C),Processes),
	findall(C,composition2xml(_,C),Compositions),
	flatten([Processes,Compositions],Codes), concatenate(Codes,Tmp),
	swritef(Code,'<universe view="structure" >\n%w\n</universe>',[Tmp]).

%% process2dsl(+P:processId, -Code) is det.
% generate Adore code associated to the process identified by processId
%
process2xml(P,Code) :- 
	process(P), genProcessLabel(P,Label),
	genVariables(P,Variables),
	genActivities(P,Activities),
	genRelations(P,Relations),
	swritef(Code,'  <process id="%w" %w >\n%w%w%w  </process>', 
                [P, Label, Variables,Activities,Relations]).


genProcessLabel(P,Code) :- 
	isFragment(P), !, swritef(Code,'name="%w" isFragment="true"',[P]).
genProcessLabel(P,Code) :- 
	hasForSrvName(P,Srv), hasForOpName(P,Op),
	swritef(Code,'name="%w::%w" isFragment="false" service="%w" operation="%w"', [Srv,Op,Srv,Op]).

%% genProcessName(P,P) :- isFragment(P), !.
%% genProcessName(P,Name) :- 
%% 	hasForSrvName(P,Srv), hasForOpName(P,Op),
%% 	swritef(Name,'%w::%w', [Srv,Op]).

%%%%
%% Variables 
%%%%

genVariables(P,Code) :- 
	findall(V,usedByProcess(P,V),RawVariables),
	sort(RawVariables,Variables),
	map(adore2xml:genVariable,Variables,VarCodes),
	concatenate(VarCodes,ValidCode),
	swritef(Code,'    <variables>\n%w\n    </variables>\n',[ValidCode]).

genVariable(V,Code) :-
	isConstant(V), !, genVarName(V,Name),
	hasForInitValue(V,Value), hasForType(V,Type),
	swritef(Code,'      <variable id="%w" %w type="%w" value="%w" />',[V,Name,Type,Value]).
genVariable(Fid,'') :-
	fieldAccess(Fid,_,_), !.
genVariable(V,Code) :-
	genVarName(V,Name), hasForType(V,Type),
	swritef(Code,'      <variable id="%w" %w type="%w"/>',[V,Name,Type]).

genVarName(V,Name) :-
	variable(V), getPreviousName(V,RawName),
	swritef(Tmp,'name="%w" isSet=',[RawName]),
	( isSet(V), string_concat(Tmp,'"true"',Name) 
         | string_concat(Tmp,'"false"',Name)).
	
%%%%
%% Activities 
%%%%

genActivities(P,Code) :- 
	findall(C,genActivity(P,C),Activities),
	concatenate(Activities,ActCode),
	swritef(Code,'    <activities>\n%w\n    </activities>\n',[ActCode]).

genActivity(P,Code) :- 
	activity(A), isContainedBy(A,P), hasForKind(A,K),
	K \= predecessors, K \= successors,
	getPreviousName(A,Name),
	genInputs(A,Inputs),
	genOutputs(A,Outputs),
	genKind(A,K,Kind),
	swritef(Code,'      <activity id="%w" name="%w" %w >\n%w\n%w\n      </activity>',
                [A,Name,Kind,Inputs,Outputs]).
	
genKind(A,invoke,Code) :- 
	hasForService(A,S), hasForOperation(A,O),
        swritef(Code, ' kind="invoke" service="%w" operation="%w"',[S,O]), !.
genKind(A,assign,Code) :- 
	hasForFunction(A,F), swritef(Code,'kind="assign" function="%w"',[F]),!.
genKind(_,K,Code) :- swritef(Code,'kind="%w"',[K]).

genInputs(A,Code) :- 
	activity(A), findall(V,usesAsInput(A,V),Variables), 
	( length(Variables,0), Code = '        <inputs />' 
          | genVarListCode(Variables, VarCodes),
	    swritef(Code,'        <inputs>\n%w\n        </inputs>',[VarCodes])).

genOutputs(A,Code) :- 
	activity(A), findall(V,usesAsOutput(A,V),Variables), 
	( length(Variables,0), Code = '        <outputs />' 
          | genVarListCode(Variables, VarCodes),
	    swritef(Code,'        <outputs>\n%w\n        </outputs>',[VarCodes])).

genVarListCode([E],Code) :- 
	fieldAccess(E,V,Fields),!,
	concatenate(Fields,FieldAccess,'/'),
	swritef(Code,'          <variableRef uid="%w" path="%w" />',
                [V,FieldAccess]).

genVarListCode([E],Code) :- 
	swritef(Code,'          <variableRef uid="%w" />',[E]),!.
genVarListCode([H|T],R) :- 
	swritef(Code,'          <variableRef uid="%w" />',[H]),!,
	genVarListCode(T,Others),
	swritef(R,'%w\n%w',[Code,Others]).

%%%%
%% Relations
%%%%

genRelations(P,Code) :- 
	findall(C,genRelation(P,C),Relations),
	concatenate(Relations,RelCode),
	swritef(Code,'    <relations>\n%w\n    </relations>\n',[RelCode]).

genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	waitFor(A,B), genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'      <relation left="%w" right="%w" kind="waitFor" />', 
                      [Blabel, Alabel]).
genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	weakWait(A,B), genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'      <relation left="%w" right="%w" kind="weakWait" />', 
                [Blabel, Alabel]).
genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	isGuardedBy(A,B,V,C),
	genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'      <relation left="%w" right="%w" kind="guard" variable="%w" condition="%w" />', [Blabel, Alabel,V,C]).

genRelation(P,Code) :- 
	activity(A), isContainedBy(A,P), activity(B), isContainedBy(B,P),
	onFailure(A,B,Cond),
	genActRelLabel(A,Alabel), 
	genActRelLabel(B,Blabel), 
	swritef(Code,'      <relation left="%w" right="%w" kind="fail" error="" />',[Blabel,Alabel,Cond]).


%genActRelLabel(A,'') :- hasForKind(A,predecessors),!.
%genActRelLabel(A,'$') :- hasForKind(A,successors),!.
genActRelLabel(A,N) :- getPreviousName(A,N).

%%%%
%% Composition
%%%%

composition2xml(Ctx,C) :-
	context(Ctx), 
	contextTarget(Ctx,Target),
	genCompositionOutput(Ctx,Out),
	genDirectives(Ctx,Directives), 
	swritef(C,'  <composition id="%w" target="%w" %w>\n%w\n  </composition>',
                  [Ctx, Target, Out, Directives]).

genCompositionOutput(Ctx,'') :- \+ contextOutput(Ctx,_).
genCompositionOutput(Ctx,C) :- 
	contextOutput(Ctx,Out),
	swritef(C,'output="%w" ',[Out]).
	
genDirectives(Ctx,Code) :- 
	findall(A,genApply(Ctx,A),Applys),
	findall(S,genSet(Ctx,S),Sets),
	flatten([Applys,Sets],List),
	concatenate(List,Code).

genApply(Ctx,Code) :- 
	context(Ctx), applyFragment(Id,Ctx,Block,Fragment),
	genActivityBlock(Block,Target),
	swritef(Code,'      <apply id="%w" fragment="%w">\n%w\n      </apply>',[Id,Fragment,Target]).

genActivityBlock(Block,Code) :- 
	activityBlock(_,Block,Activities),
	isWellFormed(Block,Process),
	generateListOfActivities(Activities,ActCode),
	swritef(Code,'        <block id="%w" process="%w">\n%w\n        </block>',[Block,Process,ActCode]).

generateListOfActivities([E],L) :- !, genActLabel(E,L).
generateListOfActivities([H|T],Code) :- 
	genActLabel(H,L), generateListOfActivities(T,Others),
	swritef(Code,'%w\n%w',[L,Others]).

genActLabel(A,L) :- swritef(L,'          <activityRef uid="%w" />',[A]).

genSet(Ctx,Code) :- 
	context(Ctx), setDirective(Ctx,V), getPreviousName(V,Name),
	swritef(Code,'      <toSet variable="%w" />',[Name]).

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

:- module(metrics,[writeMetricsIntoFile/1]).

writeMetricsIntoFile(F) :- 
	computeUniverseMetrics(Content),
	open(F,write,Stream), write(Stream,Content), close(Stream).

computeUniverseMetrics(R) :- 
	findall(P,process(P),All), sort(All,Processes), 
	computeMetricsLoop(Processes,Metrics),!,
	computeCompositionMetrics(CompositionMetrics),!,
	swritef(R,'<?xml version="1.0" encoding="UTF-8"?>\n<universe>\n%w%w\n</universe>\n',[Metrics,CompositionMetrics]).

computeMetricsLoop([],'').
computeMetricsLoop([H|T],R) :- 	
	computeMetricsAsXml(H,Xml), computeMetricsLoop(T,Others),
	swritef(R,'%w%w',[Xml,Others]).

computeMetricsAsXml(P,Xml) :- 
	process(P), defineProcessXmlName(P,Name),
	activitiesMetricsAsXml(P,XmlActs),
	relationsMetricsAsXml(P,XmlRels),
	processComplexityMetricsAsXml(P,XmlComplex),
	fragmentUsageMetricAsXml(P,XmlUsage),
	swritef(Xml,'%w%w%w%w%w  </process>\n',[Name,XmlActs,XmlRels, XmlComplex, XmlUsage]).
	

defineProcessXmlName(P,Xml) :- 
	isFragment(P), 
	swritef(Xml,'  <process name="%w" fragment="true">\n',[P]).
defineProcessXmlName(P,Xml) :- 
	swritef(Xml,'  <process name="%w" fragment="false">\n',[P]).
%%%%%%
%%% Activity cardinality
%%%%%%
activitiesMetricsAsXml(P,Xml) :- 
	findall(I, metrics_isInvoke(P,I),IL), length(IL,Ir),
	findall(R, metrics_isReply(P,R),RL), length(RL,Rr),
	findall(T, metrics_isThrow(P,T),TL), length(TL,Tr),
	getMiscActivityCardinality(P,Misc), 
	getActivityCardinality(P,Tmp),
	metrics_alignFragment(P,Tmp,Total),
	swritef(Xml,'    <activities total="%w">\n      <invoke>%w</invoke>\n      <reply>%w</reply>\n      <throw>%w</throw>\n      <misc>%w</misc>\n    </activities>\n',[Total,Ir,Rr,Tr,Misc]).

getActivityCardinality(P,R) :- 
	findall(A,isContainedBy(A,P),L), length(L,R).

getMiscActivityCardinality(P,R) :- 
	getActivityCardinality(P,All), 	
	findall(I, metrics_isInvoke(P,I),IL), length(IL,Ir),
	findall(R, metrics_isReply(P,R),RL), length(RL,Rr),
	findall(T, metrics_isThrow(P,T),TL), length(TL,Tr),
	Tmp is All - (Ir + Rr + Tr),
	metrics_alignFragment(P,Tmp,R).

metrics_alignFragment(P,T,R) :- 
	isFragment(P), !, R is T-3.
metrics_alignFragment(_,R,R).

metrics_isInvoke(P,A) :- 
	isContainedBy(A,P), activity(A), hasForKind(A,invoke).
metrics_isReply(P,A) :- 
	isContainedBy(A,P), activity(A), hasForKind(A,reply).
metrics_isThrow(P,A) :- 
	isContainedBy(A,P), activity(A), hasForKind(A,throw).

%%%%%%
%%% Order relation cardiality
%%%%%%

relationsMetricsAsXml(P,Xml) :- 
	findall([_,_],metrics_isWaitFor(P,_,_),WFl), length(WFl,WFr),
	findall([_,_],metrics_isGuard(P,_,_),Gl), length(Gl,Gr),
	findall([_,_],metrics_isWeakWait(P,_,_),WWl), length(WWl,WWr),
	findall([_,_],metrics_isFail(P,_,_),Fl), length(Fl,Fr),
	Total is WFr + Gr + WWr + Fr,
	swritef(Xml, '    <relations total="%w">\n      <wait_for>%w</wait_for>\n      <guard>%w</guard>\n      <weak_wait>%w</weak_wait>\n      <fail>%w</fail>\n    </relations>\n',[Total, WFr,Gr, WWr, Fr]).
	
getRelationCardinality(P,R) :- 
	findall([_,_],metrics_isWaitFor(P,_,_),WFl), length(WFl,WFr),
	findall([_,_],metrics_isGuard(P,_,_),Gl), length(Gl,Gr),
	findall([_,_],metrics_isWeakWait(P,_,_),WWl), length(WWl,WWr),
	findall([_,_],metrics_isFail(P,_,_),Fl), length(Fl,Fr),
	R is WFr + Gr + WWr + Fr.

metrics_isWaitFor(P,A,B) :- 
	isContainedBy(A,P), waitFor(A,B), isContainedBy(B,P). 
metrics_isGuard(P,A,B) :- 
	isContainedBy(A,P), isGuardedBy(A,B,_,_), isContainedBy(B,P). 
metrics_isWeakWait(P,A,B) :- 
	isContainedBy(A,P), weakWait(A,B), isContainedBy(B,P). 
metrics_isFail(P,A,B) :- 
	isContainedBy(A,P), onFailure(A,B,_), isContainedBy(B,P). 

%%%%%%
%%% Process complexity
%%%%%%

processComplexityMetricsAsXml(P,Xml) :- 
	getProcessHeight(P,H), getProcessWidth(P,W), 
	getProcessSurface(P,Surf), getProcessMazeLevel(P,Maze),
	swritef(Xml,'    <complexity>\n      <width>%w</width>\n      <height>%w</height>\n      <surface>%w</surface>\n      <maze>%w</maze>\n    </complexity>\n',[W,H,Surf,Maze]).

getProcessSurface(P,Perim) :- 
	getProcessHeight(P,H), getProcessWidth(P,W), Perim is H * W.

getProcessHeight(P,0) :- findall(T,metrics_processHeight(P,T), []).
getProcessHeight(P,H) :- 
	findall(T,metrics_processHeight(P,T), Tmp), 
	sort(Tmp,Sorted), last(Sorted,H).

metrics_processHeight(P,H) :- 
	process(P), isProcessEntryPoint(P,Entry), isProcessExitPoint(P,Exit),
	getPath(Entry,Exit,Path), length(Path,H).

getProcessWidth(P,0) :- findall(T,metrics_processWidth(P,T), []).
getProcessWidth(P,W) :- 
	findall(T,metrics_processWidth(P,T), Tmp), 
	sort(Tmp,Sorted), last(Sorted,W).

metrics_processWidth(P,W) :- 
	process(P), isContainedBy(A,P), activity(A), 
	findall(Next,path(A,Next),L), length(L,W).

getProcessMazeLevel(P,Maze) :- 
	findall(T,metrics_processHeight(P,T), Tmp), length(Tmp,Maze).

%%%%%%
%%% Fragment Usage
%%%%%%

fragmentUsageMetricAsXml(F,'    <usage />\n') :- \+ isFragment(F).
fragmentUsageMetricAsXml(F,Xml) :- 
	isFragment(F), 
	getFragmentApplyCardinality(F,App),
	getCrossCutLevel(F,Lvl),
	swritef(Xml,'    <usage>\n      <apply>%w</apply>\n      <targets>%w</targets>\n    </usage>\n',[App,Lvl]).

getFragmentApplyCardinality(F,R) :- 
	process(F), isFragment(F),
	findall([I,C,B],applyFragment(I,C,B,F),L), length(L,R).

getCrossCutLevel(F,L) :- 
	findall(T,metrics_fragmentTarget(F,T),Tmp), sort(Tmp,Targets),
	length(Targets, L).
	
metrics_fragmentTarget(F,T) :- 
	process(F), isFragment(F), 
	applyFragment(_,C,B,F),
	activityBlock(C,B,Acts), member(A,Acts), 
	isContainedBy(A,T).

%%%%%%
%%% Composition
%%%%%%

computeCompositionMetrics(Xml) :-
	findall(X,context(X),Contexts),
	map(metrics:compositionContextMetricAsXml,Contexts, RawData),
	concatenate(RawData,Xml).

compositionContextMetricAsXml(Ctx, Xml) :- 
	context(Ctx), contextTarget(Ctx, ProcessId),
	findall(X,applyFragment(X,Ctx,_,_),Directives),
	map(metrics:applyDirectiveAsXml,Directives,XmlDirList),
	compositionOutputAsXml(Ctx,Output), 
	concatenate(XmlDirList,XmlDir),
	swritef(Xml,'  <composition id="%w" target=\"%w\">\n%w%w\n  </composition>',[Ctx,ProcessId,Output,XmlDir]).

compositionOutputAsXml(Ctx,Xml) :- 
	context(Ctx), contextOutput(Ctx,POut),!,
	swritef(Xml,'    <output>%w</output>\n',[POut]).
compositionOutputAsXml(_,'').

applyDirectiveAsXml(Id,Xml) :- 
	applyFragment(Id,_,Block,Fragment),
	activityBlockAsXml(Block,Targets),
	swritef(Xml,'    <apply id="%w">\n      <fragment name="%w" />\n      <targets>\n%w      </targets>\n    </apply>',[Id,Fragment,Targets]).

activityBlockAsXml(BlockId,Xml) :-
	activityBlock(_,BlockId, Targets), 
	activityListAsXml(Targets,Xml).

activityListAsXml([],'').
activityListAsXml([H|T],R) :-
	activityListAsXml(T,Others),
	swritef(R,'        <act name="%w\" />\n%w',[H,Others]).



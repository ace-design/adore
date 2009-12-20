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

:- dynamic bpelTargetNamespace/2.
:- dynamic bpelNamespace/3.
:- dynamic bpelImport/4.
:- dynamic bpelPnlk/6.
:- dynamic bpelVariable/2.
:- dynamic bpelTemplate/5.
:- dynamic bpelBridge/3.
:- dynamic toBpel/2.


adore2bpel_genProcess(P,Code) :- 
	bpelBridge(process,P,Process), 
	bpelTargetNamespace(Process,TargetNamespace),
	adore2bpel_genNamespaces(P,Namespaces),
	adore2bpel_genImports(P,Imports),
	adore2bpel_genPartnerLinks(P,PartnerLinks),
	adore2bpel_genVariables(P,Variables),
	adore2bpel_genActivities(P,Activities),
	swritef(Code,'<?xml version="1.0" encoding="UTF-8"?>\n<process\n    name="%w"\n    targetNamespace="%w"\n    xmlns="http://docs.oasis-open.org/wsbpel/2.0/process/executable"\n    xmlns:xsd="http://www.w3.org/2001/XMLSchema"\n    xmlns:sxt="http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Trace"\n    xmlns:sxed="http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Editor"\n    xmlns:tns="%w"\n    xmlns:sxeh="http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/ErrorHandling"\n%w>\n%w\n%w\n%w\n%w\n</process>\n',[P,TargetNamespace,TargetNamespace,Namespaces,Imports,PartnerLinks,Variables,Activities]).

%%%%
%% Namespaces 
%%%%

%adore2bpel_genNamespaces(provider_entry,C).

adore2bpel_genNamespaces(P,Code) :- 
	findall(E,(isContainedBy(A,P),bpelBridge(activity,A,E)),L),
	bpelBridge(process,P,Process), 
	flatten([Process,L],LComplete), sort(LComplete,Entities),
	findall(S,(member(X,Entities),adore2bpel_genNamespace(X,S)),Lines),
	concatenate(Lines,Code,'\n').

adore2bpel_genNamespace(Elem,Code) :-
	bpelNamespace(Elem,Prefix,Namespace),
	swritef(Code,'    xmlns:%w="%w"',[Prefix,Namespace]).
adore2bpel_genNamespace(Elem,'') :- 
	\+ bpelNamespace(Elem,_,_). 
%	write('Warning: missing namespace for element ['),write(Elem),write(']\n').

%%%%
%% Imports 
%%%%

adore2bpel_genImports(P,Code) :-
	findall(E,(isContainedBy(A,P),bpelBridge(activity,A,E)),L),
	bpelBridge(process,P,Process), 
	flatten([Process,L],LComplete), sort(LComplete,Entities),
	findall(S,(member(X,Entities),adore2bpel_genImport(X,S)),Lines),
	concatenate(Lines,Code,'\n').

adore2bpel_genImport(Elem,Code) :-
	bpelImport(Elem,Namespace,Location,Type),
	swritef(Code,'  <import namespace="%w" location="%w" importType="%w"/>',
    [Namespace,Location,Type]).
adore2bpel_genImport(Elem,'') :- \+ bpelImport(Elem,_,_,_).
%write('Warning: missing import for element ['),write(Elem),write(']\n').

%%%%
%% Partners Links
%%%%

adore2bpel_genPartnerLinks(P,Code) :-
	findall(E,(isContainedBy(A,P),bpelBridge(activity,A,E)),L),
	bpelBridge(process,P,Process), 
	flatten([Process,L],LComplete), sort(LComplete,Entities),
	findall(S,(member(X,Entities),adore2bpel_genPartnerLink(X,S)),Lines),
	concatenate(Lines,Links,'\n'),
	swritef(Code,'  <partnerLinks>\n%w\n  </partnerLinks>',[Links]).

adore2bpel_genPartnerLink(Elem,Code) :-
	bpelPnlk(Elem,Name,Namespace,Type,Role,''),
	swritef(Code,'    <partnerLink name="%w" xmlns:tns="%w" partnerLinkType="%w" partnerRole="%w"/>', [Name,Namespace,Type,Role]).
adore2bpel_genPartnerLink(Elem,Code) :-
	bpelPnlk(Elem,Name,Namespace,Type,'',Role),
	swritef(Code,'    <partnerLink name="%w" xmlns:tns="%w" partnerLinkType="%w" myRole="%w"/>', [Name,Namespace,Type,Role]).
adore2bpel_genPartnerLink(Elem,'') :- \+ bpelPnlk(Elem,_,_,_,_,_).
	%write('Warning: missing PartnerLink for element ['),write(Elem),write(']\n').

%%%%
%% Variables
%%%%

adore2bpel_genVariables(P,Code) :- 
	findall(V,usedByProcess(P,V),L), sort(L,Variables),
	findall(S,(member(X,Variables),adore2bpel_genVariable(X,S)),Lines),
	concatenate(Lines,VarCode,'\n'),
	swritef(Code,'  <variables>\n%w\n  </variables>',[VarCode]).

adore2bpel_genVariable(V,Code) :- 
	bpelVariable(V,Type),
	swritef(Code,'    <variable name="%w" type="%w" />',[V,Type]).
adore2bpel_genVariable(V,'') :- 
	\+ bpelVariable(V,_).
	%write('Warning: missing Variable for element ['),write(V),write(']\n').
	

%%%%
%% Activities
%%%%
% make, [universe], adore2bpel_genActivities(provider_entry,Code).

adore2bpel_genActivities(P,Code) :- 
	toBpel(P,seq(R,F)), %write(seq(R,F)), nl, 
	flatten([R,F],L), %write(seq(L)), nl,
	adore2bpel_genActivity(seq(L),Code).

adore2bpel_genActivitiesLoop([],''). 
adore2bpel_genActivitiesLoop([H|T],Code) :- 
	adore2bpel_genActivity(H,ActCode),
	adore2bpel_genActivitiesLoop(T,TCode),
	swritef(Code,'%w\n%w',[ActCode,TCode]).

adore2bpel_genActivity(seq(L),Code) :- !,
	adore2bpel_genActivitiesLoop(L,Content),
	swritef(Code,'  <sequence>\n%w\n</sequence>',[Content]).
adore2bpel_genActivity(flow(L),Code) :- !,
	adore2bpel_genActivitiesLoop(L,Content),
	swritef(Code,'  <flow>\n%w\n</flow>',[Content]).
adore2bpel_genActivity(if(Var,Lif,Lelse),Code) :- !,
	adore2bpel_genActivitiesLoop(Lif,ContentIf),
	adore2bpel_genActivitiesLoop(Lelse,ContentElse),
	swritef(Code,'  <if>\n    <condition>$%w</condition>\n%w\n<else>%w</else></if>',[Var,ContentIf,ContentElse]).

adore2bpel_genActivity(A,Code) :- 
%% %	write(A),nl,
	bpelBridge(activity,A,Template),
 	gensym(A,Label),
 	findall(V,usesAsInput(A,V),Ins),
 	findall(V,usesAsOutput(A,V),Outs),
%% %	write(Template),nl,write(Ins), nl, write(Outs),nl,
 	bpelTemplate(Template,Label,Ins,Outs,Code).

adore2bpel_genActivity(A,Code) :- 
	\+ bpelBridge(activity,A,_),
	%write('Warning: missing ActTemplate for element ['),write(A),write(']\n'),
	gensym(A,Label), swritef(Code,'  <empty name="%w" />',[Label]).


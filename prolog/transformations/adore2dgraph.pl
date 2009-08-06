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

:- dynamic adore2dgraph_visited/2.
adore2dgraph_flag(S,O) :- 
	assert(adore2dgraph_visited(S,O)).


adore2dgraph :- 
	findall(P,isDefinedAsAProcess(_,_,P), Processes), 
	adore2dgraph_genProcesses(Processes,Codes),
	concatenate(Codes, Core),
	adore2dgraph_genGraph(Core), 
	retractall(adore2dgraph_visited(_,_)).

adore2dgraph(P) :- 
	%% Running the transformation
	adore2dgraph_genProcesses([P],[Core]),
	adore2dgraph_genGraph(Core), 
	retractall(adore2dgraph_visited(_,_)).

adore2dgraph_genGraph(Core) :-
        %% Generate Dot File
	swritef(Code,'digraph dependencies {\n  fontname=Courier;\n  node [shape=record];\n  edge [fontname=Courier];\n%w } \n',[Core]), !, 
        tmp_file('adore2dgraph_dot',TmpDot),
	open(TmpDot,write,Stream), write(Stream,Code), close(Stream),
	%% Generate PNG file
	tmp_file('adore2dgraph_png',Output),
	adore2png_param(exec,Exec), 
 	swritef(CmdExec,'%w -Tpng %w > %w',[Exec,TmpDot,Output]), 
        shell(CmdExec),
	%% Display PNG file
	adore2png_param(viewer,Viewer), 
	swritef(CmdView,'%w %w',[Viewer,Output]), shell(CmdView).	

%% Processes
adore2dgraph_genProcesses([],[]). 
adore2dgraph_genProcesses([H|T],[R|O]) :-
	adore2dgraph_genProcess(H,R), adore2dgraph_genProcesses(T,O). 

adore2dgraph_genProcess(P,'') :- 
	hasForSrvName(P,S), hasForOpName(P,O), adore2dgraph_visited(S,O),!.
adore2dgraph_genProcess(P,C) :- 
	hasForSrvName(P,S), hasForOpName(P,O), %% S::O associated with P
	adore2dgraph_genBox(S,O,Box), 	    %% Generate Process Box
	getProcessPartners(P,Partners),	    %% Retrieving partners
	adore2dgraph_genPartners(Partners,PartnersList), %% Generating Partners
	concatenate(PartnersList,PartnersCode),
	adore2dgraph_genLinks(P,Links), %% Generating links
	concatenate([Box,PartnersCode,Links],C).  %% Writing result

%% Boxes
adore2dgraph_genBox(S,O,'') :- adore2dgraph_visited(S,O), !.
adore2dgraph_genBox(S,O,Code) :- 
	isExtService(S), !, adore2dgraph_flag(S,O),
	swritef(Code,'%w_%w [label="%w::%w",style=filled,fillcolor=grey];',[S,O,S,O]).
adore2dgraph_genBox(S,O,Code) :- 
	adore2dgraph_flag(S,O),
	swritef(Code,'%w_%w [label="%w::%w"];',[S,O,S,O]).
    
%% Partners
adore2dgraph_genPartners([],[]).
adore2dgraph_genPartners([H|T],[C|O]) :-
	adore2dgraph_genPartner(H,C), adore2dgraph_genPartners(T,O). 

adore2dgraph_genPartner([S,O],Code) :- 
	isDefinedAsAProcess(S,O,P), !,
	adore2dgraph_genProcess(P,Code).
adore2dgraph_genPartner([S,O],Code) :- 
	adore2dgraph_genBox(S,O,Code).

%% Links
adore2dgraph_genLinks(P,Code) :- 
	findall(C,adore2dgraph_genLink(P,C),Tmp),
	sort(Tmp,Links), concatenate(Links,Code).

adore2dgraph_genLink(P,Code) :- 
	isDefinedAsAProcess(SourceSrv,SourceOp,P),
	isPartnerOf(P,TargetSrv,TargetOp), 
	swritef(Code,'  %w_%w -> %w_%w ;',[SourceSrv,SourceOp,TargetSrv,TargetOp]).



%% displayDgraph :- 
%% 	tmp_file('adore2dgraph',Tmp),
%% 	generateDependeciesGraph(Tmp),
%% 	adore2png_param(viewer,E), swritef(Cmd,'%w %w',[E,Tmp]), shell(Cmd).

%% generateDependeciesGraph(OutputFile) :- 
%% 	adore2dgraph(Code), tmp_file('adore2dgraph',Tmp), 
%% 	open(Tmp,write,Stream), write(Stream,Code), close(Stream),
%% 	adore2png_param(exec,E), 
%% 	swritef(Cmd,'%w -Tpng %w > %w',[E,Tmp,OutputFile]), shell(Cmd).

%% adore2dgraph(R) :- 
%% 	adore2dgraph_genCore(C),
%% 	swritef(R,'digraph dependencies {\n  fontname=Courier;\n  node [shape=record];\n  edge [fontname=Courier];\n  rankdir=LR;\n%w } \n',[C]),!.

%% adore2dgraph_genCore(Code) :- 
%% 	findall(P,adore2dgraph_genService(_,P),MyBoxes),
%% 	concatenate(MyBoxes,MyNodes),
%% 	findall(P,adore2dgraph_genExtService(_,P),TheirBoxes),
%% 	concatenate(TheirBoxes,TheirNodes),
%% 	adore2dgraph_genLinks(Links),
%% 	concatenate([MyNodes,TheirNodes,Links],Code).

%% %% Services and operations defined as processes
%% adore2dgraph_genService(S,Code) :- 
%% 	hasForSrvName(_,S),
%% 	findall(C,adore2dgraph_genOperation(S,_,C),Ops),
%% 	concatenate(Ops,Operations,'|'),
%% 	swritef(Code,'  %w [label="%w|%w"] ;',[S,S,Operations]).
%% adore2dgraph_genOperation(S,O,Code) :- 
%% 	hasForSrvName(P,S),
%% 	hasForOpName(P,O),
%% 	swritef(Code,'{<%w>%w}',[O,O]).

%% %% Services and process defined as black boxes
%% adore2dgraph_genExtService(S,Code) :- 
%% 	isExtService(S),
%% 	inferServiceOperations(S,OpIds),
%% 	adore2dgraph_genExtOperations(OpIds,Ops),
%% 	concatenate(Ops,Operations,'|'),
%% 	swritef(Code,'  %w [fillcolor=grey,style=filled,label="%w|%w"] ;',[S,S,Operations]).

%% adore2dgraph_genExtOperations([],[]).
%% adore2dgraph_genExtOperations([Op|Tail],[Code|Others]) :- 
%% 	adore2dgraph_genExtOperation(Op,Code),
%% 	adore2dgraph_genExtOperations(Tail,Others).
 
%% adore2dgraph_genExtOperation(O,Code) :- 
%% 	swritef(Code,'{<%w>%w}',[O,O]).
    
%% %% Links between entities
%% adore2dgraph_genLinks(Code) :- 
%% 	findall(C,adore2dgraph_genLink(C),List),
%% 	sort(List,Links),
%% 	concatenate(Links,Code).
%% adore2dgraph_genLink(Code) :-
%% 	%% Target: 
%% 	hasForService(Act,Service),
%% 	hasForOperation(Act,Operation),
%% 	swritef(Target,'%w:%w',[Service,Operation]),
%% 	%% Source: 
%% 	isContainedBy(Act,Process),
%% 	hasForSrvName(Process,SourceSrv),
%% 	hasForOpName(Process,SourceOp),
%% 	swritef(Code,'%w:%w -> %w ;',[SourceSrv,SourceOp,Target]).
	


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
%% Temporary facts ... used to 'flag' visited nodes and avoid infinite loops
%%%%
:- dynamic adore2dgraph_visited/2.
adore2dgraph_flag(S,O) :- assert(adore2dgraph_visited(S,O)).

%%%%
%% Main interface with the transformation
%%%%

%% adore2dgraph(+File): generate a complete dep. graph, written in File
adore2dgraph(F) :- 
	findall(P,isDefinedAsAProcess(_,_,P), Processes), 
	adore2dgraph_genProcesses(Processes,Codes),
	concatenate(Codes, Core),
	adore2dgraph_genGraph(Core,F), 
	retractall(adore2dgraph_visited(_,_)).

%% adore2dgraph(+Process,+File): generate Process dep. graph in File
adore2dgraph(P,F) :- 
	adore2dgraph_genProcesses([P],[Core]),
	adore2dgraph_genGraph(Core,F), 
	retractall(adore2dgraph_visited(_,_)).

%% adore2dgraph_genGraph(+Core, +File): generate a single graph (described
%%  as DOT code in 'Core') as a PNG file stored in File
adore2dgraph_genGraph(Core,F) :-
	swritef(Code,'digraph dependencies {\n  fontname=Courier;\n  node [shape=record];\n  edge [fontname=Courier];\n rankdir=LR;\n%w } \n',[Core]), !, 
        tmp_file('adore2dgraph_dot',TmpDot),
	open(TmpDot,write,Stream), write(Stream,Code), close(Stream),
	adore2png_param(exec,Exec), 
 	swritef(CmdExec,'%w -Tpng %w > %w',[Exec,TmpDot,F]), 
        shell(CmdExec).	

%%%%
%% Code generation for processes
%%%%

%% adore2dgraph_genProcesses(+Processes,-Codes): iterates over Processes and 
%%  put each results (process generation) in the Codes list.
adore2dgraph_genProcesses([],[]). 
adore2dgraph_genProcesses([H|T],[R|O]) :-
	adore2dgraph_genProcess(H,R), adore2dgraph_genProcesses(T,O). 

%% adore2dgraph_genProcess(+Process,-Code):
%%  Generate the DOT Code corresponding to the dep graph of Process
adore2dgraph_genProcess(P,'') :- 
	hasForSrvName(P,S), hasForOpName(P,O), adore2dgraph_visited(S,O),!.
adore2dgraph_genProcess(P,C) :- 
	hasForSrvName(P,S), hasForOpName(P,O),    %% S::O associated with P
	adore2dgraph_genBox(S,O,Box), 	          %% Generate Process Box
	getProcessPartners(P,Partners),	          %% Retrieving partners
	adore2dgraph_genPartners(Partners,PartnersList), %% Generating Partners
	concatenate(PartnersList,PartnersCode),
	adore2dgraph_genLinks(P,Links),           %% Generating links
	concatenate([Box,PartnersCode,Links],C).  %% Writing result

%%%%
%% Code generation handling "boxes" (i.e. Service::Operation dep.)
%%%%

adore2dgraph_genBox(S,O,'') :- adore2dgraph_visited(S,O), !.
adore2dgraph_genBox(S,O,Code) :- 
	isExtService(S), !, adore2dgraph_flag(S,O),
	swritef(Code,'%w_%w [label="%w::%w",style=filled,fillcolor=grey];',[S,O,S,O]).
adore2dgraph_genBox(S,O,Code) :- 
	adore2dgraph_flag(S,O),
	swritef(Code,'%w_%w [label="%w::%w"];',[S,O,S,O]).

%%%%    
%% Handling 'intelligent' partner generation
%%%%

%% Iteration over a set of partners to produces a set of codes
adore2dgraph_genPartners([],[]).
adore2dgraph_genPartners([H|T],[C|O]) :-
	adore2dgraph_genPartner(H,C), adore2dgraph_genPartners(T,O). 

%% Generate a given partner (process => recursive call)
adore2dgraph_genPartner([S,O],Code) :- 
	isDefinedAsAProcess(S,O,P), !,
	adore2dgraph_genProcess(P,Code).
adore2dgraph_genPartner([S,O],Code) :- 
	adore2dgraph_genBox(S,O,Code).

%%%%
%% Handling dependency links
%%%%
adore2dgraph_genLinks(P,Code) :- 
	findall(C,adore2dgraph_genLink(P,C),Tmp),
	sort(Tmp,Links), concatenate(Links,Code).

adore2dgraph_genLink(P,Code) :- 
	isDefinedAsAProcess(SourceSrv,SourceOp,P),
	isPartnerOf(P,TargetSrv,TargetOp), 
	swritef(Code,'  %w_%w -> %w_%w ;', [SourceSrv,SourceOp,TargetSrv,TargetOp]).


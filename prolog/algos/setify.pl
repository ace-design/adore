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
:- module(setify,[doSetify/2,setify/3]).

%%%%%%
%% Related publication: 
%%  Sébastien Mosser, Mireille Blay-Fornarino, Johan Montagnat. 
%%  "Orchestration Evolution Following Dataflow Concepts: 
%%   Introducing Unanticipated Loops Inside a Legacy Workflow" (long paper) 
%%  in Proceedings of the International Conference on Internet 
%%     and Web Applications and Services (ICIW), Acceptation Rate: 28 %,
%%  IEEE Computer Society, Venice, Italy, 24-18 may 2009
%%  http://rainbow.polytech.unice.fr/publis/mosser-blay-fornarino-etal:2009.pdf
%%%%%%

%%% 'setification' algorithm

doSetify(P,V) :-
	setify(P,V, Set),
	executeActionSet(Set).

setify(Process, Variable, Actions) :-
	%% Identifying variable dataflow in process
	process(Process), variable(Variable), dataflow(Variable, Core), 
	%% Building the new 'set' variable
	buildVectorFromScalar(Variable, VarStarActions), 
	%% Identifying output variables, and build associated sets
	getBlockOutputVariable(Core, Outs),
	map(setify:buildVectorFromScalar, Outs, OutsActions),
	%% Identify variable substitution (v -> v*) outside the dataflow
	getSetificationSubstitutionSet(Process, [Variable], Core, VarActions), 
	getSetificationSubstitutionSet(Process, Outs, Core, OutsSubstActions), 
	%% Build the iteration policy and apply it to the dataflow
	buildIterationPolicy(Variable, Outs, Pid, PolicyAction),
	buildPolicyApplication(Pid,Core,PolicyAppActions),
	%% Building the final action set result
	flatten([VarStarActions, OutsActions, VarActions, OutsSubstActions, 
	         PolicyAction, PolicyAppActions], Actions).

buildVectorName(Scalar, Vector) :- 
	atom_concat(Scalar,'_star',Vector).

buildVectorFromScalar(Scalar, Actions) :- 
	buildVectorName(Scalar,Vector), hasForType(Scalar,Type),
	tracePrettyName(Scalar,Vector,Trace),
	Actions = [createVariable(Vector), Trace, 
	           setVariableType(Vector,Type), flagAsSet(Vector)].

tracePrettyName(Scalar,Vector,traceDerivation(setify,Ghost,Vector)) :-
	getPreviousName(Scalar,Old),
	buildVectorName(Old,Ghost).
	
getSetificationSubstitutionSet(_, [], _, []).
getSetificationSubstitutionSet(P, [H|T], B, Actions) :- 
	findall(A, buildVariableSetSubstitution(P,H,B,A), Tmp), 
	sort(Tmp, ToDo), 
	getSetificationSubstitutionSet(P, T, B, OthersActions),
	append(ToDo,OthersActions,Actions).
buildVariableSetSubstitution(Process, Scalar, Block, Action) :- 
	isContainedBy(A, Process), usesElem(A,Scalar), 
	\+ member(A, Block), buildVectorName(Scalar,Vector), 
	Action = substituteVariable(Scalar,Vector,A).

buildIterationPolicy(Variable, [], Id, Action) :- 
	gensym(p,Id), 
	buildVectorName(Variable,Vector),
	Action = defPolicy(Id,forall(Variable,Vector),none). 

buildIterationPolicy(Variable, [Output], Id, Action) :- 
	gensym(p,Id), 
	buildVectorName(Variable,Vector), buildVectorName(Output,OutVector),
	Action = defPolicy(Id,forall(Variable,Vector),append(Output,OutVector)).

buildPolicyApplication(_,[],[]).
buildPolicyApplication(Pid,[H|T],Actions) :- 
	buildPolicyApplication(Pid,T,Others),
	append([setIteration(H,Pid)],Others, Actions).
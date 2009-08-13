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

%%% 'setification' algorithm

doSetify(Process, Variable, Actions) :-
	%% Identifying variable dataflow in process
	process(Process), variable(Variable), dataflow(Variable, Core), 
	%% Building the new 'set' variable
	buildVectorFromScalar(Variable, VarStarActions), 
	%% Identifying output variables, and build associated sets
	getBlockOutputVariable(Core, Outs),
	map(buildVectorFromScalar, Outs, OutsActions),
	%% Identify variable substitution
	getSetificationSubstitutionSet(Process, [Variable], Core, VarActions), 
	getSetificationSubstitutionSet(Process, Outs, Core, OutsSubstActions), 
	%%
	buildIterationPolicy(Variable, Outs, Pid, PolicyAction),
	buildPolicyApplication(Pid,Core,PolicyAppActions),
	%% Building the final action set result
	flatten([VarStarActions, OutsActions, VarActions, OutsSubstActions, 
	         PolicyAction, PolicyAppActions], Actions).
	
buildVectorName(Scalar, Vector) :- 
	atom_concat(Scalar,'_star',Vector).

buildVectorFromScalar(Scalar, Actions) :- 
	buildVectorName(Scalar,Vector), hasForType(Scalar,Type),
	Actions = [createVariable(Vector), setVariableType(Vector,Type), 
	           flagAsSet(Vector)].
	

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

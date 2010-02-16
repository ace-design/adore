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

:- module(variable,[]).

%%%
% Atomic Activity Predicates
%%%

	
%% get/2: get(+V,-R): retrieve the variable associated to V
get(F,V) :- fieldAccess(F,V,_). %% as it can be a field access
get(V,V) :- variable(V).        %% or simply the variable.


%% belongsTo/2: belongsTo(?V,?P) ==> V is used in P
belongsTo(Var,Process) :- 
	activity:belongsTo(A,Process), 
	activity:useVariable(A,Var).

getType(Var,T) :- hasForType(Var,T).


getInitValue(Var,Val) :- hasForInitValue(Var,Val).


findEquivalentInBlock(Activity,V,Block,Candidate) :- 
	activity:useVariable(Activity,V,Direction),
	activity:isBlockInterfaceVariable(Block,Candidate,Direction),
	hasForType(V,T), hasForType(Candidate,T), 
	\+ isConstant(Candidate).

areEquivalent(Activity,V,Target,Candidate) :- 
	activity:useVariable(Activity,V,Direction),
	activity:useVariable(Target,Candidate, Direction),
	hasForType(V,T), hasForType(Candidate,T), 
	\+ isConstant(Candidate).


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
%% @author      Main Sebastien Mosser          [mosser@polytech.unice.fr]
%%%%

:- module(process,[]).


%%
% Process common predicate
%%

exists(P) :- process(P),!.
exists(P) :- context(C), contextOutput(C,P), !.

getPreds(P,A) :- 
	isFragment(P), getActivities(P,Acts), 
	member(A,Acts), hasForKind(A,predecessors).

getSuccs(P,A) :- 
	isFragment(P), activity:belongsTo(A,P), hasForKind(A,successors).

getHook(P,A) :- 
	isFragment(P), activity:belongsTo(A,P), hasForKind(A,hook).

%%
% Associated entities
%%

getVariables(P,Vars) :- 
	findall(V,variable:belongsTo(V,P),Tmp),
	sort(Tmp,Vars).

getActivities(P,Activities) :- 
	findall(V,activity:belongsTo(V,P),Tmp),
	sort(Tmp,Activities). 

bindsFragmentParameterToVariable(Process,Param,Var) :- 
	process(Process), isFragment(Process),
	hasForParameter(Process,Param), 
	getPreviousName(Var,Param), variable(Var).




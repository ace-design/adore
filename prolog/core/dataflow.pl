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


:- dynamic dataLink/3.
%% Based on the article published in ICIW'09

%%%%
%% Dataflow
%%%%

%% dataflow(+V,-Acts): identify Activities impacted by V dataflow
dataflow(V,Acts) :- 
	findall(A,variableUsageClosure(V,A),L), sort(L,Acts).

%% variableUsageClosure(+V,-A): Activity A uses V as input, or a 'descendant'
%variableUsageClosure(V,A) :- 
	
variableUsageClosure(V,A) :- 
	isDfActivity(A), usesElemAsInput(A,V).
variableUsageClosure(V,A) :- 
	isContainedBy(APrime,P), isContainedBy(A,P),
	isDfActivity(A), isDfActivity(APrime), \+ A == APrime, 
	usesElemAsInput(A,Vin), usesElemAsOutput(APrime,Vin), 
	existsPath(APrime,A), 
	variableUsageClosure(V,APrime).
variableUsageClosure(V,A) :- 
	isDfActivity(A), isDfActivity(APrime), \+ A == APrime, 
	isContainedBy(APrime,P), isContainedBy(A,P), 
	usesElemAsOutput(A,Vout), usesElemAsOutput(APrime,Vout), 
	existsPath(APrime,A), 
	variableUsageClosure(V,APrime).
variableUsageClosure(V,A) :- %% to take care of user's datalink
	dataLink(DLProcess,DLVar,DLAct),
	variable:belongsTo(V,RealProcess), 
	findRoot(V,VarRoot),
	variable:belongsTo(VarRoot,DLProcess),
	getPreviousName(VarRoot,DLVar), activity:belongsTo(ActRoot,DLProcess),
	process:getActivities(RealProcess,Activities), member(A,Activities),
	findRoot(A,ActRoot), getPreviousName(ActRoot,DLAct).

variableUsageClosure(V,A) :- %% to take care of user's datalink
	dataLink(DLProcess,DLVar,DLAct),
	variable:belongsTo(V,RealProcess), 
	findUserRoot(V,Tmp),getPreviousName(VarRoot,Tmp),
	variable:belongsTo(VarRoot,DLProcess),
	getPreviousName(VarRoot,DLVar), activity:belongsTo(ActRoot,DLProcess),
	process:getActivities(RealProcess,Activities), member(A,Activities),
	findRoot(A,ActRoot), getPreviousName(ActRoot,DLAct).

%% variableUsageClosure(V,A) :- %% to take care of user's datalink
%% 	variable:belongsTo(V,P),! , activity:belongsTo(A,P), 
%% 	findRoot(A,RootA), activity:belongsTo(RootA,RootProcess), 
%% 	writef('%w --> %w\n',[A,RootA]),nl,
%% 	getPreviousName(RootA,NameA), dataLink(RootProcess,NameVar,NameA), 
%% 	pebble(substitution,LambdaVar,V,_), 
%% 	pebble(derivation,LambdaAncestor,LambdaVar,_), %% TODO: fixe implem!
%% 	variable:belongsTo(LambdaAncestor,RootProcess),
%% 	getPreviousName(LambdaAncestor,NameVar).


%% isDfActivity(+A): an activity inside a dataflow can't be of any kind.
isDfActivity(A) :- activity(A), hasForKind(A,assign).
isDfActivity(A) :- activity(A), hasForKind(A,invoke).
isDfActivity(A) :- activity(A), hasForKind(A,nop).
isDfActivity(A) :- activity(A), hasForKind(A,hook).









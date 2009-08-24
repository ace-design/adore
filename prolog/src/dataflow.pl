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


%% Based on the article published in ICIW'09

%%%%
%% Dataflow
%%%%

%% dataflow(+V,-Acts): identify Activities impacted by V dataflow
dataflow(V,Acts) :- 
	findall(A,variableUsageClosure(V,A),L), sort(L,Acts).

%% variableUsageClosure(+V,-A): Activity A uses V as input, or a 'descendant'
variableUsageClosure(V,A) :- 
	isDfActivity(A), usesElemAsInput(A,V).
variableUsageClosure(V,A) :- 
	isDfActivity(A), isDfActivity(APrime), \+ A == APrime, 
	isContainedBy(APrime,P), isContainedBy(A,P), existsPath(APrime,A), 
	usesElemAsInput(A,Vin), usesElemAsOutput(APrime,Vin), 
	variableUsageClosure(V,APrime).

variableUsageClosure(V,A) :- 
	isDfActivity(A), isDfActivity(APrime), \+ A == APrime, 
	isContainedBy(APrime,P), isContainedBy(A,P), existsPath(APrime,A), 
	usesElemAsOutput(A,Vout), usesElemAsOutput(APrime,Vout), 
	variableUsageClosure(V,APrime).

%% isDfActivity(+A): an activity inside a dataflow can't be of any kind.
isDfActivity(A) :- activity(A), hasForKind(A,assign).
isDfActivity(A) :- activity(A), hasForKind(A,invoke).
isDfActivity(A) :- activity(A), hasForKind(A,nop).
isDfActivity(A) :- activity(A), hasForKind(A,hook).









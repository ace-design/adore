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



writeArrayLine(L) :-
	process(P), \+ isFragment(P), 
	getActivityCardinality(P,A), getOrderCardinality(P,O),
	swritef(L,'\texttt{%w} & %w & %w\n & &',[P,A,O]).

%%% Activity cardinality

getActivityCardinality(P,R) :- 
	findall(A,isContainedBy(A,P),L), length(L,R).

%%% Order relation cardiality

getOrderCardinality(P,R) :- 
	getWaitForCardinality(P,Wf), getGuardCardinality(P,G),
	getWeakWaitCardinality(P,Ww), getFailCardinality(P,F),
	R is Wf + G + Ww + F.

getWaitForCardinality(P,R) :- 
	findall([A,B],metrics_isWaitFor(P,A,B),L), length(L,R).
metrics_isWaitFor(P,A,B) :- 
	isContainedBy(A,P), waitFor(A,B), isContainedBy(B,P). 

getGuardCardinality(P,R) :- 
	findall([A,B],metrics_isGuard(P,A,B),L), length(L,R).
metrics_isGuard(P,A,B) :- 
	isContainedBy(A,P), isGuardedBy(A,B,_,_), isContainedBy(B,P). 

getWeakWaitCardinality(P,R) :- 
	findall([A,B],metrics_isWeakWait(P,A,B),L), length(L,R).
metrics_isWeakWait(P,A,B) :- 
	isContainedBy(A,P), weakWait(A,B), isContainedBy(B,P). 

getFailCardinality(P,R) :- 
	findall([A,B],metrics_isFail(P,A,B),L), length(L,R).
metrics_isFail(P,A,B) :- 
	isContainedBy(A,P), onFailure(A,B,_), isContainedBy(B,P). 
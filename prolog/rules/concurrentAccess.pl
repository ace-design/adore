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

:- module(concurrentAccess,[]).

run(Pairs) :- 
	findall([P,R],isConflicting(P,R), Tmp),
	sort(Tmp,Pairs).


isConflicting(Pair,Resource) :- 
	process(P), 
	activity:belongsTo(A,P), activity:belongsTo(APrime,P), A \= APrime,
	usesElemAsOutput(A,Resource), usesElem(APrime, Resource),
	\+ (relations:existsPath(A,APrime) | relations:existsPath(APrime,A)),!,
	Tmp = [A,APrime], sort(Tmp, Pair).
	
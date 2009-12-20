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
%% Extracts dependencies between processes
%%%%


%% isExtService(?S): an external service is invoked but not defined.
isExtService(S) :- 
	hasForService(_,S), \+ hasForSrvName(_,S).

inferServiceOperations(S,Ops) :- 
	findall(O,hasForServiceAndOperation(S,O),L),
	sort(L,Ops).
	
hasForServiceAndOperation(S,O) :- 
	hasForService(A,S), hasForOperation(A,O).

getProcessPartners(P,Partners) :- 
	findall([S,O],isPartnerOf(P,S,O),Tmp),
	sort(Tmp,Partners).

isPartnerOf(P,S,O) :- 
	process(P), activity(A), isContainedBy(A,P), 
	hasForService(A,S), hasForOperation(A,O).

isDefinedAsAProcess(S,O,P) :- 
	hasForSrvName(P,S), hasForOpName(P,O).
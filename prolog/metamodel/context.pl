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

:- module(context,[]).

%%%
% Accessors predicates
%%%

target(C,P) :- 
	context(C), contextTarget(C,P), process(P).

output(C,P) :- 
	context(C), contextOutput(C,P).

directives(C,DirSet) :- 
	context(C), 
	findall(applyDir(X),applyFragment(X,C,_,_),Applys),
	findall(setDir(X), setDirective(C,X), Sets),
	flatten([Applys,Sets],DirSet).

%%%
% Context Predicates
%%%

isEquivalent(C1,C2) :- 
	output(C1,O), output(C2,O), target(C1,T), target(C2,T).

%%%
% Context Set Predicates
%%%




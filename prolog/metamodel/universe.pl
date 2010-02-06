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

:- module(universe,[]).

%%% Remarks: 
%  The implementation only deals with one universe at once. As a consequence, 
%  there is no 'identifier' for a given universe, as it can only exists ONE
%  universe at the same time in the ADORE engine.
%%%

%%%
% Accessors predicates
%%%

processes(Processes) :- 
	findall(X,process(X), Processes).

contexts(Contexts) :- 
	findall(X,context(X), Contexts).

knowledge([]).

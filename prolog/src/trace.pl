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
%% @author      Main S�bastien Mosser          [mosser@polytech.unice.fr]
%%%%

%% We leave 'peebbles' after us (cf 'Le petit poucet').
:- dynamic pebble/4.


%%%%%% traceRename/4: traceRename(K,O,N,C)
%%  -> K: pick one from  activity|variable|constant
%%  -> O: old name
%%  -> N: new name
%%  -> C: context of renaming (compile, duplication, merge).

traceRename(Kind, Old, New, Context) :-
	assert(pebble(rename(Kind), Old, New, Context)).
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

:- dynamic adore_silent/1.
adore_silent(false).
adore_silence(B) :- 
	retractall(adore_silent(_)), assert(adore_silent(B)).
%% aw means "adore write"
aw(_) :- adore_silent(true), !.
aw(M) :- adore_silent(false), write(M).


loadFiles :- 
	aw('%% Loading ADORE kernel'),
	[debug], [trace], [config],  [metamodel], [actions], [functions], 
	[conflicts], [helpers], [dependencies],
	aw('%% Loading Transformations'),
	loadTransfo('adore2dot'), loadTransfo('adore2png'), 
	loadTransfo('adore2dgraph').


loadTransfo(Name) :- 
	string_concat('../transformations/',Name,File),	[File].

header :- 
	aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
        aw('%%            ADORE Copyright (C) 2008 - ...           %%'), nl,
        aw('%%  Activity moDel supOrting oRchestration Evolution   %%'), nl,
        aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
        aw('%% Authors: Sebastien Mosser & Mireille Blay-Fornarino %%'), nl,
        aw('%% Main contact: mosser@polytech.unice.fr              %%'), nl,
        aw('%% Website:      http://www.adore-design.org           %%'), nl,
        aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
        aw('%%  This program comes with ABSOLUTELY NO WARRANTY.    %%'), nl,
        aw('%%  This is free software, and you are welcome to      %%'), nl,
        aw('%%  redistribute it under certain conditions.          %%'), nl,
        aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl.

:-  header, loadFiles, performDebugSubscription.
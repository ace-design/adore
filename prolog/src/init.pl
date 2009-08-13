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
aw(M) :- adore_silent(false), write(M), nl.


loadFiles :- 
	aw('%%%% Loading Source Kernel'),
        [debug], [trace], [config],  [metamodel], [actions], [functions], 
	[conflicts], [helpers], [dependencies], [dataflow], [weave], 
	[substitution],
	aw('%%%% Loading Algorithms'),
        loadAlgo('setify'),
	aw('%%%% Loading Transformations'),
        loadTransfo('adore2dot'), loadTransfo('adore2png'), 
	loadTransfo('adore2dgraph').


loadTransfo(Name) :-  
	string_concat('../transformations/',Name,File),	[File].

loadAlgo(Name) :-  
	string_concat('../algorithms/',Name,File), [File].

header :- 
	aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
        aw('%%            ADORE Copyright (C) 2008 - ...           %%'),
        aw('%%  Activity moDel supOrting oRchestration Evolution   %%'),
        aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
        aw('%% Authors: Sebastien Mosser & Mireille Blay-Fornarino %%'),
        aw('%% Main contact: mosser@polytech.unice.fr              %%'),
        aw('%% Website:      http://www.adore-design.org           %%'),
        aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
        aw('%%  This program comes with ABSOLUTELY NO WARRANTY.    %%'),
        aw('%%  This is free software, and you are welcome to      %%'),
        aw('%%  redistribute it under certain conditions.          %%'),
        aw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%').

:-  header, 
	aw('%%%%%%%% Loading the ADORE engine ...'),
	loadFiles, 
	aw('%%%% Debug Channels Activation'), performDebugSubscription, 
	aw('%%%% Checking ADORE consistency'), make,
        aw('%%%%%%%% ADORE engine loaded !').
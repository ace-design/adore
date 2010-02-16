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
	aw('%%%% Loading Prolog Internal Core'), 
        loadCore('facts_model'), 
        loadCore('engine'), loadCore('debug'), loadCore('trace'), 
	loadCore('actions'), loadCore('gensym'), loadCore('helpers'), 
	%% TODO: following should be merged in metamodel operationalization
	loadCore('functions'), loadCore('dependencies'), loadCore('dataflow'), 
	loadCore('substitution'),
	%% end of todo
	aw('%%%% Loading ADORE Metamodel'),
        loadMM('universe'), loadMM('activity'), loadMM('context'),
	loadMM('process'), loadMM('variable'), loadMM('relations'),
	aw('%%%% Loading Algorithms'),
        loadAlgo('contextMerge'), loadAlgo('applyRewrite'),
	loadAlgo('contextNormalization'), loadAlgo('setify'), 
	loadAlgo('clone'), loadAlgo('merge'), loadAlgo('weave'),
	loadAlgo('instantiateFragment'), loadAlgo('processNormalization'),
	loadAlgo('processSimplification'),
	aw('%%%% Loading Transformations'),
        loadTransfo('adore2dsl'), loadTransfo('adore2xml'),
	loadTransfo('adore2dot'), loadTransfo('adore2metrics'),
	loadTransfo('adore2png'), loadTransfo('adore2dgraph'),
	aw('%%%% Loading Detection Rules'), 
        loadRule('concurrentAccess'),
	aw('%%%% Loading Local Configuration'), 
        [config].

loadCore(Name) :- 
	string_concat('core/',Name,File), [File].

loadTransfo(Name) :-  
	string_concat('transfos/',Name,File), use_module(File).

loadAlgo(Name) :-  
	string_concat('algos/',Name,File), use_module(File).

loadRule(Name) :-  
	string_concat('rules/',Name,File), use_module(File).

loadMM(Name) :- 
	string_concat('metamodel/',Name,File), use_module(File).

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
	aw('%%%% Debug Channels Activation'), 
        performDebugSubscription, 
	aw('%%%% Checking ADORE consistency'), 
        make,
        aw('%%%%%%%% ADORE engine loaded !').
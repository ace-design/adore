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

loadFiles :- 
	write('%% Loading ADORE kernel'),
	[debug], [trace], [config],  [metamodel], [actions], [functions], 
	[conflicts], [helpers], 
	write('%% Loading Transformations'),
	loadTransfo('adore2dot'), loadTransfo('adore2png').


loadTransfo(Name) :- 
	string_concat('../transformations/',Name,File),	[File].

header :- 
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
        write('%%            ADORE Copyright (C) 2008 - ...           %%'), nl,
        write('%%  Activity moDel supOrting oRchestration Evolution   %%'), nl,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
        write('%% Authors: Sebastien Mosser & Mireille Blay-Fornarino %%'), nl,
        write('%% Main contact: mosser@polytech.unice.fr              %%'), nl,
        write('%% Website:      http://www.adore-design.org           %%'), nl,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
        write('%%  This program comes with ABSOLUTELY NO WARRANTY.    %%'), nl,
        write('%%  This is free software, and you are welcome to      %%'), nl,
        write('%%  redistribute it under certain conditions.          %%'), nl,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl.

:-  header, loadFiles, performDebugSubscription.
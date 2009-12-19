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

%%%%%%%%%%%%%%%%%%%%%%%
%% Adore Code Weaver %%
%%%%%%%%%%%%%%%%%%%%%%%

executeActionSet(Set) :- 
	elementarize(Set,Actions),
	execute(Actions).


:- dynamic isMacroAction/2.
shouldBeElementarized(G) :- 
	functor(G,F,A), Arity is A + 1, isMacroAction(F,Arity).

elementarize([],[]).
elementarize([H|T],R) :- 
	shouldBeElementarized(H), !, call(H,A), 
	elementarize(T,Tmp), append(A,Tmp,R).
elementarize([H|T],[H|O]) :- elementarize(T,O).


execute([]).
execute([H|T]) :- dinfo(exec,'#exec(~w)',H), H, execute(T).




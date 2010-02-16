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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adore Code Execution Framework %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

executeActionSet(Set) :- 
	execute(Set,'').


:- dynamic isMacroAction/2.
shouldBeElementarized(G) :- 
	functor(G,F,A), Arity is A + 1, isMacroAction(F,Arity).

execute([],_).
execute([H|T],Space) :- 
	shouldBeElementarized(H), !, 
	dinfo(exec,'~w#elementarize(~w)',[Space,H]), call(H,A),
	swritef(NewSpace,'  %w',[Space]), 
        execute(A,NewSpace), execute(T,Space).
execute([H|T],Space) :- 
	dinfo(exec,'~w#exec: ~w',[Space,H]), H, !, execute(T,Space).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adore Facts Self Deletion Framework %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

myRetract(F) :- 
	F, retract(F).
myRetract(F) :- 
	\+ F, !.%dinfo(exec,'fact [~w] does not exists!',[F]).
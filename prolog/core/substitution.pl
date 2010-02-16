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
%% Deprecated
%%%%














%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adore Substitution Rules %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% WARNING %%%%
%% Rule defined here MUST NOT induce any side-effect
%%  => elementary actions to perform MUST BE unified as LAST argument.
%%%%

:- assert(isMacroAction(substituteVariable,4)).

substituteVariable(Old,New,Act,Actions) :-
	findall(Actions,variableSubstitutionAction(Old,New,Act,Actions),Tmp),
	flatten(Tmp,Actions).

variableSubstitutionAction(Old,New,Act,Actions) :- 
	usesAsInput(Act,Old),
	Actions = [retract(usesAsInput(Act,Old)), addAsInput(New,Act)].
variableSubstitutionAction(Old,New,Act,Actions) :- 
	usesAsOutput(Act,Old),
	Actions = [retract(usesAsOutput(Act,Old)), addAsOutput(New,Act)].
variableSubstitutionAction(Old,New,Act,Actions) :- 
	usesAsBinding(Act,Old,Part),
	Actions = [retract(usesAsBinding(Act,Old,Part)),
	           setMessageBinding(Act,Part,New)].
variableSubstitutionAction(Old,New,Act,Actions) :- 
	usesElem(Act,Field), fieldAccess(Field,Old,Path),
	Actions = [retract(fieldAccess(Field,Old,Path)), 
	           setFieldAccess(Field,New,Path)].

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adore Substitution Rules %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% WARNING %%%%
%% Rule defined here MUST NOT induce any side-effect
%%  => elementary actions to perform MUST BE unified as LAST argument.
%%%%

%% substituteVariable(+O,+N,+Acts,-Actions)
%%  substitute variable Old with variable New when used by an activity in Acts
substituteVariable(Old,New,Acts,Actions) :- 
	member(A,Acts), activity(A), usesAsInput(A,Old), 
	Actions = [retract(usesAsInput(A,Old)), addAsInput(New,A)].
substituteVariable(Old,New,Acts,Actions) :- 
	member(A,Acts), activity(A), fieldAccess(I,Old,Path), usesAsInput(A,I),
	Actions = [retract(fieldAccess(I,Old,Path)), setFieldAcces(I,New,Path)].
substituteVariable(Old,New,Acts,Actions) :- 
	member(A,Acts), activity(A), usesAsOutput(A,Old), 
	Actions = [retract(usesAsOutput(A,Old)), addAsOutput(New,A)].
substituteVariable(Old,New,Acts,Actions) :- 
	member(A,Acts), activity(A), fieldAccess(I,Old,Path), usesAsOutput(A,I),
	Actions = [retract(fieldAccess(I,Old,Path)), setFieldAcces(I,New,Path)].


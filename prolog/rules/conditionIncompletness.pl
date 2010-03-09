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

:- module(conditionIncompletness,[]).

%% user interface:

play :- 
	run(Result), show(Result).

%% technical implementation

show([]) :- !.
show([conditionIncompletness(P,A,C,V)|T]) :- 
	H = conditionIncompletness(P,A,C,V),
	process(P), writef('Warning: %w\n',[H]),
	show(T).

run(Pairs) :- 
	findall(conditionIncompletness(P,A,C,V),isConflicting(P,A,C,V),Tmp),
	sort(Tmp,Pairs).


isConflicting(Process, Activity, Condition,Contrary) :- 
	process(Process), 
	activity:belongsTo(Activity, Process),
	isGuardedBy(_, Activity, Condition, Value), invert(Value,Contrary),
	\+ isGuardedBy(_, Activity, Condition, Contrary).
	
invert(true,false).
invert(false,true).

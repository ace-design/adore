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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Actions available on the ADORE metamodel %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% defActivity(+X): Declare an activity
%%   * Assumptions: X is not an activity
%%   * Guaranty: A unique fact will be defined to declare X as an activity
defActivity(X) :- isActivity(X), !, 
	dlog(def,failure,'defActivity/1 cannot re-create activity ~w',X), fail.
defActivity(X) :- assert(isActivity(X)), 
	dlog(def,info,'defActivity(~w) ... done!',X).

%% defWaitFor(+X,+Y): Declare an order between X and Y,
%%   * Assumptions: X and Y are two activities, no path from Y to X.
%%   * Guaranties: A unique fact will be defined inside the database
defWaitFor(X,_) :- \+ isActivity(X), !,
	dlog(def,failure,'defWaitFor/2 ~w isn\'t an activity!',[X]), fail.
defWaitFor(_,Y) :- \+ isActivity(Y), !,
	dlog(def,failure,'defWaitFor/2 ~w isn\'t an activity!',[Y]), fail.
defWaitFor(X,Y) :- existsPath(Y,X), !, 
	dlog(def,failure,'defWaitFor/2 ~w < ~w introduces a cycle!',[Y,X]), 
	fail.
defWaitFor(X,Y) :- waitFor(X,Y), !
	dlog(def,warning,'defWaitFor/1 ~w < ~w still exists!',[Y,X]).
defWaitFor(X,Y) :- assert(waitFor(X,Y)), 
	dlog(def,info,'defWaitFor(~w,~w) ... done!',[X,Y]).

%% defProcess(+X): Declare a process
%%   * Assumptions: X is not a process
%%   * Guaranty: A unique fact will be defined to declare X as a process
defProcess(X) :- isProcess(X), !,
	dlog(def,failure,'defActivity/1 cannot re-create process ~w',X), fail.
defProcess(X) :- assert(isProcess(X)), 
	dlog(def,info,'defProcess(~w) ... done!',X).

%% defBelongsTo(+A,+P): Declare that an activity belongs to a process
%%   * Assumptions: A is an activity, P is a process
%%   * Guaranty: A belongs to P, and only P (no activity share)
defBelongsTo(A,_) :- \+ isActivity(A), !, 
	dlog(def,failure,'defBelongsto/2 ~w isn\'t an activity!',[A]), fail.
defBelongsTo(_,P) :- \+ isProcess(P), !, 
	dlog(def,failure,'defBelongsto/2 ~w isn\'t a process',[P]), fail.
defBelongsTo(A,P) :- belongsTo(A,Q), \+ P = Q, !, 
	dlog(def,failure,'defBelongsto/2 ~w belongs to ~w',[A,Q]), fail.
defBelongsTo(A,P) :- belongsTo(A,P), !,
	dlog(def,warning,'defBelongsTo/2 ~w belonged to ~w!',[A,P]).
defBelongsTo(A,P) :- assert(belongsTo(A,P)),
	dlog(def,info,'defBelongsTo(~w,~w) ... done!',[A,P]).

%% defVariable(

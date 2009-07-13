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


%dlog(def,failure,'defActivity/1 cannot re-create activity ~w',X), fail.

%%%%
%% Business Processes
%%%%

%% createProcess/1: createProcess(P)
createProcess(P) :- %% \exists p \in Process* => fail
	process(P), !, 
	dfail(create,'Cannot create process \'~w\': it exists!',P).
createProcess(P) :- 
	assert(process(P)), 
	dinfo(create,'Process \'~w\' created.',P).

%% setAsFragment/1: setAsFragment(P)
setAsFragment(P) :- %% \not \exists p \in Process* => fail
	\+ process(P), !, 
	dfail(set,'setAsFragment/2: Unkown process \'~w\'!',P).
setAsFragment(P) :-
	assert(isFragment(P)), 
	dinfo(set,'Process \'~w\' flagged as fragment.',P).


%%%%
%% Activities
%%%%

%% createActivity/1: createActivity(A)
createActivity(A) :- %% \exists a \in Activity* => fail
	activity(A), !, 
	dfail(create,'Cannot create activity \'~w\': it exists!',A).
createActivity(A) :-
	assert(activity(A)), dinfo(create,'Activity \'~w\' created.',A).

%% setActivityKind/2: setActivityKind(A,K)
setActivityKind(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail('setActivityKind/2: Unknown activity \'~w\'!',A).
setActivityKind(A,K) :- 
	assert(hasForKind(A,K)),
	dinfo(set,'Activity \'~w\' flagged with kind \'~w\'.',[A,K]).

%% setContainment/2: setContainment(A,P)
setContainment(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setContainment/2: Unknown activity \'~w\'!',A).
setContainment(_,P) :- %% \not \exists p \in Process* => fail
	\+ process(P), !, 
	dfail(set,'setContainment/2: Unkown process \'~w\'!',P).
setContainment(A,P) :- 
	assert(isContainedBy(A,P)),
	dinfo(set,'Activity  \'~w\' contained by  \'~w\'',[A,P]).

%% setInvokedService/2: setInvokedService(A,I)
setInvokedService(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setInvokedService/2: Unknown activity \'~w\'!',A).
setInvokedService(A,_) :- %% kind(a) != invoke => fail
	\+ hasForKind(A,invoke), !, 
	dfail(set,'setInvokedService/2: Activity \'~w\' isn\'t an invocation!',A).
setInvokedService(A,I) :- 
	assert(hasForService(A,I)), 
	dinfo(set,'Activity  \'~w\' invokes service  \'~w\'',[A,I]).

%% setInvokedOperation/2: setInvokedOperation(A,I)
setInvokedOperation(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setInvokedOperation/2: Unknown activity \'~w\'!',A).
setInvokedOperation(A,_) :- %% kind(a) != invoke => fail
	\+ hasForKind(A,invoke), !, 
	dfail(set,'setInvokedOperation/2: Activity \'~w\' isn\'t an invocation!',A).
setInvokedOperation(A,I) :- 
	assert(hasForOperation(A,I)), 
	dinfo(set,'Activity  \'~w\' invokes operation  \'~w\'',[A,I]).

%% setMessageBinding/3: setMessageBinding(A,I,V)
setMessageBinding(A,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setMessageBinding/2: Unknown activity \'~w\'!',A).
setMessageBinding(A,I,V) :-
	assert(usesAsBinding(A,V,I)), 
	dinfo(set,'Activity  \'~w\' binds variable \'~w\' to part \'~w\'',[A,V,I]).

%% setFunction/2: setFunction(A,I)
setFunction(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setFunction/2: Unknown activity \'~w\'!',A).
setFunction(A,_) :- %% kind(a) != assign => fail
	\+ hasForKind(A,assign), !, 
	dfail(set,'setFunction/2: Activity \'~w\' isn\'t an assignment!',A).
setFunction(A,I) :- 
	assert(hasForFunction(A,I)),
	dinfo(set,'Activity  \'~w\' uses function \'~w\'',[A,I]).

%%%%
%% Variables
%%%%

%% createVariable/1: createVariable(V)
createVariable(V) :- %% \exists v \in Variable* => fail
	variable(V), !, 
	dfail(create,'Cannot create variable \'~w\': it exists!',V).
createVariable(V) :-
	assert(variable(V)), dinfo(create,'Variable \'~w\' created.',V).


%% setVariableType/2: setVariableType(V,T)
setVariableType(V,_) :- %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(set,'setVariableType/2: Unknown variable \'~w\'!',V).
setVariableType(V,T) :- 
	assert(hasForType(V,T)), 
	dinfo(set,'Variable \'~w\' flagged with type \'~w\'',[V,T]).

%% setInitValue/2: setInitValue(V,I)
setInitValue(V,_) :- %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(set,'setInitValue/2: Unknown variable \'~w\'!',V).
setInitValue(V,I) :- 
	assert(hasForInitValue(V,I)),
	dinfo(set,'Variable \'~w\' initialized with \'~w\' value',[V,I]).

%% setConstancy/1: setConstancy(V)
setConstancy(V) :- %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(set,'setConstancy/1: Unknown variable \'~w\'!',V).
setConstancy(V) :-
	assert(isConstant(V)), 
	dinfo(set,'Variable \'~w\' flagged as constant',V).

%% addAsInput/2: addAsInput(V,A)
addAsInput(V,_) :- %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(set,'addAsInput/2: Unknown variable \'~w\'!',V).
addAsInput(_,A) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'addAsInput/2: Unknown activity \'~w\'!',A).
addAsInput(V,A) :- 
	assert(hasForInput(A,V)), 
	dinfo(set,'Variable \'~w\' used as \'~w\' input.',[V,A]).

%% addAsOutput/2: addAsOutput(V,A)
addAsOutput(V,_) :- %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(set,'addAsOutput/2: Unknown variable \'~w\'!',V).
addAsOutput(_,A) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'addAsOutput/2: Unknown activity \'~w\'!',A).
addAsOutput(V,A) :- 
	assert(hasForOutput(A,V)), 
	dinfo(set,'Variable \'~w\' used as \'~w\' input.',[V,A]).

%%%%
%% Relations
%%%%

%% defWaitFor/2: defWaitFor(A,A)
defWaitFor(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defWaitFor/2: Unknown activity \'~w\'!',A).
defWaitFor(_,A) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defWaitFor/2: Unknown activity \'~w\'!',A).
defWaitFor(A1,A2) :- %% \exists path(a1 -> a2) \in WaitFor* => fail
	existsPath(A1,A2), !, 
	dfail(def,'defWaitFor/2: \'~w\' < \'~w\' introduces a cycle!',[A2,A1]).
defWaitFor(A1,A2) :- %% \exists a2 < a1 \in WaitFor* => warning
	waitFor(A1,A2), !, 
	dwarn(def,'defWaitFor/2: \'~w\' < \'~w\' still exists!',[A2,A1]).
defWaitFor(A1,A2) :- 
	assert(waitFor(A1,A2)), 
	dinfo(def,'Relation \'~w\' < \'~w\' is defined.',[A2,A1]).

%% defGuard/4: defGuard((A,A,V,B)
defGuard(A,_,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'deGuard/4: Unknown activity \'~w\'!',A).
defGuard(_,A,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'deGuard/4: Unknown activity \'~w\'!',A).
defGuard(_,A,V,_) :- %% v \not \in Outputs(a) => fail
	\+ hasForOutput(A,V), !, 
	dfail(def,'deGuard/4: Activity \'~w\' doesn\'t assign \'~w\'!',[A,V]).
defGuard(A1,A2,V,B) :- 
	assert(isGuardedBy(A1,A2,V,B)),
	dinfo(def,'Relation \'~w\' < \'~w\' when \'~w\' (\'~w\') is defined.',[A1,A2,V,B]).
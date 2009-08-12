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


%%%%
%% Business Processes
%%%%

%% createProcess/1: createProcess(P)
createProcess(P) :- %% \exists p \in Process* => fail
	process(P), !, 
	dfail(create,'Cannot create process \'~w\': it exists!',P).
createProcess(P) :- 
	assert(process(P)), 
	dinfo(create,'Process  \'~w\' created.',P).

%% setAsFragment/1: setAsFragment(P)
setAsFragment(P) :- %% \not \exists p \in Process* => fail
	\+ process(P), !, 
	dfail(set,'setAsFragment/1: Unkown process \'~w\'!',P).
setAsFragment(P) :-
	assert(isFragment(P)), 
	dinfo(set,'Process  \'~w\' flagged as fragment.',P).

%% setAsFragmentParameter/1: setAsFragmentParameter(P,I)
setAsFragmentParameter(P,_) :- %% \not \exists p \in Process* => fail
	\+ process(P), !, 
	dfail(set,'setAsFragmentParameter/2: Unkown process \'~w\'!',P).
setAsFragmentParameter(P,_) :- %% \not isFragment(p) => fail
	\+ isFragment(P), !, 
	dfail(set,'setAsFragmentParameter/2: Process \'~w\' is not a fragment!',P).
setAsFragmentParameter(P,I) :- %% \not \exists p \in Process* => fail
	assert(hasForParameter(P,I)), !, 
	dinfo(set,'Fragment \'~w\' uses \'~w\' as parameter.',[P,I]).

%% setService/2: setService(P,I).
setService(P,_) :- %% \not \exists p \in Process* => fail
	\+ process(P), !,
	dfail(set,'setService/2: Unkown process \'~w\'!',P).
setService(P,I) :- 
	assert(hasForSrvName(P,I)).

%% setOperation/2: setOperation(P,I).
setOperation(P,_) :- %% \not \exists p \in Process* => fail
	\+ process(P), !,
	dfail(set,'setOperation/2: Unkown process \'~w\'!',P).
setOperation(P,I) :- 
	assert(hasForOpName(P,I)).

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
	dfail(set,'setActivityKind/2: Unknown activity \'~w\'!',A).
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
	dinfo(set,'Activity \'~w\' contained by  \'~w\'',[A,P]).

%% setInvokedService/2: setInvokedService(A,I)
setInvokedService(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setInvokedService/2: Unknown activity \'~w\'!',A).
setInvokedService(A,_) :- %% kind(a) != invoke => fail
	\+ hasForKind(A,invoke), !, 
	dfail(set,'setInvokedService/2: Activity \'~w\' isn\'t an invocation!',A).
setInvokedService(A,I) :- 
	assert(hasForService(A,I)), 
	dinfo(set,'Activity \'~w\' invokes service  \'~w\'',[A,I]).

%% setInvokedOperation/2: setInvokedOperation(A,I)
setInvokedOperation(A,_) :- %% \not \exists a \in Activity* => fail
                       	\+ activity(A), !, 
	dfail(set,'setInvokedOperation/2: Unknown activity \'~w\'!',A).
setInvokedOperation(A,_) :- %% kind(a) != invoke => fail
	\+ hasForKind(A,invoke), !, 
	dfail(set,'setInvokedOperation/2: Activity \'~w\' isn\'t an invocation!',A).
setInvokedOperation(A,I) :- 
	assert(hasForOperation(A,I)), 
	dinfo(set,'Activity \'~w\' invokes operation  \'~w\'',[A,I]).

%% setMessageBinding/3: setMessageBinding(A,I,V)
setMessageBinding(A,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setMessageBinding/2: Unknown activity \'~w\'!',A).
setMessageBinding(A,I,V) :-
	assert(usesAsBinding(A,V,I)), 
	dinfo(set,'Activity \'~w\' binds variable \'~w\' to part \'~w\'',[A,V,I]).

%% setFunction/2: setFunction(A,I)
setFunction(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'setFunction/2: Unknown activity \'~w\'!',A).
setFunction(A,_) :- %% kind(a) != assign => fail
	\+ hasForKind(A,assign), !, 
	dfail(set,'setFunction/2: Activity \'~w\' isn\'t an assignment!',A).
setFunction(A,I) :- 
	assert(hasForFunction(A,I)),
	dinfo(set,'Activity \'~w\' uses function \'~w\'',[A,I]).

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

%% flagAsSet/1: flagAsSet(V)
flagAsSet(V) :- %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(set,'flagAsSet/1: Unknown variable \'~w\'!',V).
flagAsSet(V) :-
	assert(isConstant(V)), 
	dinfo(set,'Variable \'~w\' flagged as a dataset',V).


%% addAsInput/2: addAsInput(V,A)
addAsInput(V,_) :- %% \not \exists v \in Variable* => fail
	\+ getVariable(V,_), !, 
	dfail(set,'addAsInput/2: Unknown variable \'~w\'!',V).
addAsInput(_,A) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'addAsInput/2: Unknown activity \'~w\'!',A).
addAsInput(V,A) :- 
	assert(usesAsInput(A,V)), 
	dinfo(set,'Variable \'~w\' used as \'~w\' input.',[V,A]).

%% addAsOutput/2: addAsOutput(V,A)
addAsOutput(V,_) :- %% \not \exists v \in Variable* => fail
	getVariable(V,Rv), \+ variable(Rv), !, 
	dfail(set,'addAsOutput/2: Unknown variable \'~w\'!',Rv).
addAsOutput(_,A) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(set,'addAsOutput/2: Unknown activity \'~w\'!',A).
addAsOutput(V,A) :- 
	assert(usesAsOutput(A,V)), 
	dinfo(set,'Variable \'~w\' used as \'~w\' input.',[V,A]).

%% fieldAccess/3: accesstoField(I,V,L)
createFieldAccess(I,_,_) :- %% \exists i \in FieldAccess* => fail
	fieldAccess(I,_,_), !, 
	dfail(create,'Cannot create field access \'~w\': it exists!',I).
createFieldAccess(_,V,_) :-  %% \not \exists v \in Variable* => fail
	\+ variable(V), !, 
	dfail(create,'createFieldAccess/3: Unknown variable \'~w\'!',V).
createFieldAccess(_,_,[]) :- %% isEmpty(l) => fail
	dfail(set,'createFieldAccess/3: Empty field list!',[]).
createFieldAccess(I,V,L) :- 
	assert(fieldAccess(I,V,L)),
	dinfo(create,'Creating named access \'~w\' to fields ~w of variable \'~w\'',[I,L,V]).

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
defWaitFor(A1,A2) :- %% \exists path(a1 -> a2) => fail
	existsPath(A1,A2), !, 
	dfail(def,'defWaitFor/2: \'~w\' < \'~w\' introduces a cycle!',[A2,A1]).
defWaitFor(A1,A2) :- %% \exists a2 < a1 \in WaitFor* => warning
	waitFor(A1,A2), !, 
	dwarn(def,'defWaitFor/2: \'~w\' < \'~w\' still exists!',[A2,A1]).
defWaitFor(A1,A2) :- 
	assert(waitFor(A1,A2)), 
	dinfo(def,'Relation \'~w\' < \'~w\' is defined.',[A2,A1]).

%% defWeakWait/2: defWeakWait(A,A)
defWeakWait(A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defWeakWait/2: Unknown activity \'~w\'!',A).
defWeakWait(_,A) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defWeakWait/2: Unknown activity \'~w\'!',A).
defWeakWait(A1,A2) :- %% \exists path(a1 -> a2)  => fail
	existsPath(A1,A2), !, 
	dfail(def,'defWeakWait/2: \'~w\' < \'~w\' introduces a cycle!',[A2,A1]).
defWeakWait(A1,A2) :- %% \exists a2 < a1 \in WeakWait* => warning
	weakWait(A1,A2), !, 
	dwarn(def,'defWeakWait/2: \'~w\' << \'~w\' still exists!',[A2,A1]).
defWeakWait(A1,A2) :- 
	assert(weakWait(A1,A2)), 
	dinfo(def,'Relation \'~w\' << \'~w\' is defined.',[A2,A1]).


%% defGuard/4: defGuard(A,A,V,B)
defGuard(A,_,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defGuard/4: Unknown activity \'~w\'!',A).
defGuard(_,A,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defGuard/4: Unknown activity \'~w\'!',A).
defGuard(A1,A2,_,_) :- %% \exists path(a1 -> a2)  => fail
	existsPath(A1,A2), !, 
	dfail(def,'defGuard/4: \'~w\' < \'~w\' introduces a cycle!',[A2,A1]).
defGuard(_,A,V,_) :- %% v \not \in Outputs(a) => fail
	\+ usesAsOutput(A,V), !, 
	dfail(def,'defGuard/4: Activity \'~w\' doesn\'t assign \'~w\'!',[A,V]).
defGuard(A1,A2,V,B) :- 
	assert(isGuardedBy(A1,A2,V,B)),
	dinfo(def,'Relation \'~w\' < \'~w\' when \'~w\' is ~w is defined.',[A2,A1,V,B]).

%% defOnFail/3: defOnFail(A,A,S)
defOnFail(A,_,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defOnFail/3: Unknown activity \'~w\'!',A).
defOnFail(_,A,_) :- %% \not \exists a \in Activity* => fail
	\+ activity(A), !, 
	dfail(def,'defOnFail/3: Unknown activity \'~w\'!',A).
defOnFail(A1,A2,_) :- %% \exists path(a1 -> a2)  => fail
	existsPath(A1,A2), !, 
	dfail(def,'defOnFail/4: \'~w\' < \'~w\' introduces a cycle!',[A2,A1]).
defOnFail(A1,A2,M) :- 
	assert(onFailure(A1,A2,M)),
	dinfo(def,'Relation \'~w\' < \'~w\' exists on failure \'~w\'',[A2,A1,M]).
	

%%%%
%% Composition directives
%%%%

%% defCompositionContext/1: defCompositionContext(I)
defCompositionContext(I) :- %% \exists p in Context* => fail
	context(I),!,
	dfail(def,'defCompositionContext/1: Context  \'~w\' still exists!',I).
defCompositionContext(I) :-
	assert(context(I)),
	dinfo(def,'Context  \'~w\' created with success!',I).

%% setCompositionTarget/2: setCompositionTarget(I,P)
setCompositionTarget(I,_) :- %% \not \exists i in Context* => fail
	\+ context(I), !,
	dfail(set,'setCompositionTarget/2: Unknown context identifier \'~w\'',I).
setCompositionTarget(_,P) :- %% \not \exists p in Process* => fail
	\+ process(P), !,
	dfail(set,'setCompositionTarget/2: Unknown process \'~w\'',P).
setCompositionTarget(I,P) :- 
	assert(contextTarget(I,P)),
	dinfo(set,'Context \'~w\' uses \'~w\' as infered target',[I,P]).

%% setContextOutput/2: setContextOutput(I,I)
setContextOutput(I,_) :- 
	\+ context(I), !,
	dfail(set,'setContextOutput/2: Unknown context identifier \'~w\'',I).
setContextOutput(_,I) :- 
	process(I), !, 
	dfail(def,'setContextOutput/2: Process \'~w\' still exists!',I).
setContextOutput(I,P) :- 
	assert(contextOutput(I,P)),
	dinfo(set,'Context \'~w\' uses \'~w\' as identified result',[I,P]).


%% defActivityBlock/3: defActivityBlock(I,I,L)
defActivityBlock(C,_,_) :- 
	\+ context(C), !, 
	dfail(def,'defActivityBlock/3: Unknown context \'~w\' !',C).
defActivityBlock(_,I,_) :- 
	activityBlock(_,I,_), !, 
	dfail(def,'defActivityBlock/3: Block identifier \'~w\' still exists!',I).
defActivityBlock(C,_,L) :- 
	getAbsoluteNames(C,L,Acts),
	\+ maplist(activity,Acts), !,
	dfail(def,'defActivityBlock/3: Some elements in  ~w are not regular activities!',[Acts]).
defActivityBlock(C,I,L) :- 
	getAbsoluteNames(C,L,Acts),
	assert(activityBlock(C,I,Acts)),
	dinfo(def,'Block \'~w\' containing activities ~w created!',[I,Acts]).

%% defApply/4: defApply(I,P,A,P).
defApply(I,_,_,_) :- %% \exists i \in Apply* => fail
	applyFragment(I,_,_,_), !, 
	dfail(def,'defApply/4: Directive identifier \'~w\' still exists!',I).
defApply(_,C,_,_) :- %% \not \exists C \in Context* => fail
	\+ context(C), !, 
	dfail(def,'defApply/4: Unkown context \'~w\'!',C).
defApply(_,_,_,F) :- %% \not \exists p \in Fragment* => fail
	\+ isFragment(F), 
	dfail(def,'defApply/4: Unkown fragment \'~w\'!',F).

defApply(_,C,B,_) :- %% \not \exists a \in Activities(p) => fail
	activityBlock(C,B,Content),
	map(isContainedBy,Content,Processes),
	sort(Processes,Targets),
	length(Targets, L),
	\+ L == 1, !,
	dfail(def,'defApply/4: Activities in ~w come from different processes \'~w\'!',[Content,Targets]).
defApply(I,O,A,F) :- 
	assert(applyFragment(I,O,A,F)),
	dinfo(def,'Directive apply \'~w\' to \'~w\' successfuly created in context \'~w\' with id \'~w\'!',[F,A,O,I]).

%% setApplyParam/3: setApplyParam(I,I,S).
setApplyParam(I,_,_) :- 
	\+ applyFragment(I,_,_,_), !, 
	dfail(set,'setApplyParameter/3: Unknown directive identifier \'~w\'!',I).
setApplyParam(ApplyId, I,_) :- 
	applyFragment(ApplyId,_,_,F), 
	\+ hasForParameter(F,I), !, 
	dfail(set,'setApplyParameter/3: unknown parameter \'~w\' for fragment \'~w\'!',[I,F]).
setApplyParam(ApplyId, I, S) :-
	assert(applyParameter(ApplyId,I,S)),
	dinfo(set,'Directive \'~w\' uses \'~w\' for \'~w\' parameter',[ApplyId,S,I]).

%% defSetify/2: defSetify(I,V)
defSetify(C,_) :- 
	\+ context(C), !, 
	dfail(def,'defSetify/2: Unkown context \'~w\'!',C).
defSetify(C,V) :- 
	getAbsoluteName(C,V,Name),
	\+variable(Name), !,
	dfail(def,'defSetify/2: Unkown variable \'~w\' (from ~w)!',[Name,V]).
defSetify(C,V) :- 
	getAbsoluteName(C,V,Name),
	dinfo(def,'ToSet directive stored for variable \'~w\'.',[Name]).

%%%%
%% Policies
%%%%

defPolicy(I,_) :- 
	policy(I,_), !, 
	dfail(def,'defPolicy/2: Id \'~w\' still exist!',I).
defPolicy(I,F) :- 
	assert(policy(I,F)),
	dinfo(def,'Policy \'~w\' created with formula [~w].',[I,F]).

setIteration(A,_) :- 
	\+ activity(A), !, 
	dfail(set,'setIteration/2: Unknown activity \'~w\'!',A).
setIteration(_,P) :- 
	\+ policy(P,_), !, 
	dfail(set,'setIteration/2: Unknown policy \'~w\'!',P).
setIteration(A,P) :- 
	assert(iteratesOver(A,P)),
	dinfo(set,'Activity \'~w\' iterates over \'~w\'.',[A,P]).

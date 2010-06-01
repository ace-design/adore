%% Refactoring rules for the AoURN -> ADORE experience
%% author: Sebastien Mosser <mosser@polytech.unice.fr>
%% date: Tue Jun  1, 2010  9:29 AM

:- module(refactor, [doRefactor/1]).

%% Complete Refactoring

doRefactor(P) :- 
	dinfo(algo,'Running doRefactor(~w)',[P]),
	dinfo(algo,'  Computing Enrichment action set',[]),
	myTimer(refactor:enrich(P, EnrichActs)),
	length(EnrichActs,EnrichLength), 
	dinfo(algo,'  => Result: ~w actions',[EnrichLength]),
	dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(EnrichActs)), !,
	dinfo(algo,'  Computing PullIn/PushOut action set',[]),
	myTimer(refactor:buildActions(P, PullPushActs)),
	length(PullPushActs,LPullPush),
	dinfo(algo,'  => Result: ~w actions',[LPullPush]),
	dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(PullPushActs)), !,
	dinfo(algo,'doRefactor(~w) ended with success!',[P]).

buildActions(P, Actions) :- 
	pullIn(P, PullInActs), pushOut(P, PushOutActs), 
	flatten([PullInActs, PushOutActs], Actions).

%% Pullin: 
%%  variable used in the process, but never assigned in the 
%%  preceding control-flow => assigned when receiving the invocation message.

pullIn(P, Actions) :- 
	findall(A,refactor:pullInCandidate(P,A),Raws), sort(Raws, Actions).

pullInCandidate(P, Action) :- 
	process(P), variable:belongsTo(V,P), usesAsInput(A,V), 
	(\+ usesAsOutput(APrime, V)
         |  usesAsOutput(APrime,V), \+ relations:path(APrime,A)), 
	 activity:belongsTo(R,P), hasForKind(R,receive),
	 Action = addAsOutput(V,R).

%% Push out:
%%  variable assigned in the process, but not used by others 
%%  activities in the following control flow => responded when replying the 
%%  output message.

pushOut(P, Actions) :- 
	findall(A,refactor:pushOutCandidate(P,A),Raws), sort(Raws, Actions).

pushOutCandidate(P, Action) :- 
	process(P), variable:belongsTo(V,P), \+ isGuardedBy(_,_,V,_),
	usesAsOutput(A,V),
	(\+ usesAsInput(APrime, V)
         |  usesAsInput(APrime,V), \+ relations:path(A,APrime)),
	 activity:belongsTo(R,P), hasForKind(R,reply),
	 Action = addAsInput(V,R).

%% Enrichment:
%% 
enrich(P,Actions) :- 
	findall(A,refactor:enrichCandidate(P,A), Raws), 
	sort(Raws, Actions).

enrichCandidate(P, Actions) :- 
	process(P), whiteBoxCall(P, Invoke, Target),
	validateRequest(Invoke, Target, ReqActs),
	validateResponse(Invoke, Target, RespActs),
	flatten([ReqActs, RespActs], Actions).

whiteBoxCall(P, Caller, Callee) :- 
	activity:belongsTo(Caller, P), hasForKind(Caller, invoke), 
	hasForService(Caller, Srv), hasForOperation(Caller, Op), 
	process(Callee), hasForSrvName(Callee,Srv), hasForOpName(Callee, Op).

validateRequest(Activity, Target, Acts) :- 
	findall([V,T],(usesAsInput(Activity, V), hasForType(V,T)), Used),
	activity:belongsTo(R,Target), hasForKind(R,receive),
	findall([V,T], (usesAsOutput(R, V), hasForType(V,T)), Required),
	match(Activity, input, Used, Required, Acts).

validateResponse(_,_,[]).


%%%%
%% Helper (technical stuff)
%%%%

match(A, Direction, [], Required, Acts) :- 
	propagate(A, Direction, Required, Acts).

propagate(_,_,[],[]).
propagate(Activity, input, [[Var, _]|Tail], Acts) :- 
	findUserRoot(Var,RootName), activity:belongsTo(Activity, P),
	buildVarName(P,RootName, VarName), clone(Var,VarName,CloneActs),
	propagate(Activity, input, Tail, Others),
	flatten([CloneActs, addAsInput(VarName, Activity), Others], Acts).


buildVarName(Process, RootName, VarName) :- 
	process(Process), hasForSrvName(Process, Srv), 
	hasForOpName(Process, Op), gensym(RootName, Symb),
	swritef(Str,'%w_%w_%w',[Srv,Op,Symb]), string_to_atom(Str, VarName).

%% local clone (only take care of sets, should not be triggered in other cases)
clone(Old, New, Acts) :- 
	isSet(Old), hasForType(Old, T), 
	Acts = [createVariable(New), setVariableType(New,T), flagAsSet(New)].
clone(Old, New, Acts) :- 
	\+ isSet(Old), hasForType(Old, T), 
	Acts = [createVariable(New), setVariableType(New,T)].
%%%%%
%% Pointcut matching mechanisms for NF propertie
%% @author: Mireille Blay-Fornarino & Sebastien Mosser
%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expressing Pointcuts as logical predicates %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cut4logTime(Acts) :- %% build(logTime,cut4logTime,Acts), writeList(Acts).
	findall([A],(  findInvokeAct(cms,assignIntRes,A)
		     | findInvokeAct(cms,requestExtRes,A)), Resources),
	findall([A], findInvokeAct(msgBus,wait4msg,A), Waiters),
	findall([A], ( findVarUsageByType(worker,_,A), 
	               hasForKind(A,invoke)), Workers),
	intersection(Waiters,Workers,WaitAndWork),
	merge(WaitAndWork,Resources,Raws), sort(Raws,Acts).
	
cut4logError(Acts) :- %%  build(logError,cut4logError,Acts), writeList(Acts).
	findall([A],(onFailure(_,A,_),\+ activity:belongsTo(A,logError)),Raw),
	sort(Raw,Acts).

cut4logUpdate(Acts) :- %% build(logUpdate,cut4logUpdate,Acts), writeList(Acts).
	findall([A], ( findVarUsageByType(cmsEmployee,in,A), 
	               hasForKind(A,invoke)), CmsEmployees),
	findall([A], ( findVarUsageByType(worker,in,A), 
	               hasForKind(A,invoke)), Workers),
	merge(CmsEmployees,Workers, Raws), sort(Raws,Acts).

cut4logCreate(Acts) :- %% build(logCreate,cut4logCreate,Acts), writeList(Acts).
	findall([A], ( findVarUsageByType(cmsEmployee,out,A), 
	               hasForKind(A,invoke)), CmsEmployees),
	findall([A], ( findVarUsageByType(worker,out,A), 
	               hasForKind(A,invoke)), Workers),
	merge(CmsEmployees,Workers, Raws), sort(Raws,Acts).

cut4authIdle(Acts) :- %% build(authentifyWhenIdle,cut4authIdle,L),writeList(L).
	findall([A], ( findVarUsageByType(cmsEmployee,in,A), 
	               hasForKind(A,invoke)), CmsEmployees),
	findall([A], ( findInvokeAct(ui,_,A),
		       \+ (  activity:belongsTo(A,cms_authUser)
		            | activity:belongsTo(A,authentifyWhenIdle))), 
		UiPertinentInvocations),
	intersection(UiPertinentInvocations,CmsEmployees, Raws),
	sort(Raws,Acts).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers defined to query models %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findInvokeAct(Srv,Operation,A) :- 
	activity(A), hasForKind(A,invoke), 
	hasForService(A,Srv), hasForOperation(A,Operation).

findVarUsageByType(Type,Direction,A) :- % direction= in | out
	activity:useVariable(A,V,Direction), hasForType(V,Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% building directives %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

displayCuts :- 
	buildAllCuts(Cuts), dispatchCuts(Cuts, OnOrch, OnFrag), 
	factorizeCuts(OnFrag,OnFragFactorized), 
	factorizeCuts(OnOrch,OnOrchFactorized), 
	write('## Fragments applied on other Fragments'),nl,
	writeList(OnFragFactorized), 
	write('## Fragments applied on Orchestrations'),nl,
	writeList(OnOrchFactorized),!.

build(FragmentName,Pointcut,Directives) :- 
	call(Pointcut,List),
	findall(weave(FragmentName,E),member(E,List),Directives).

buildAllCuts(GlobalCuts) :- 
	build(logTime,cut4logTime,LogTimeCuts),
	build(logError,cut4logError,LogErrorCuts),
	build(logUpdate,cut4logUpdate,LogUpdateCuts),
	build(logCreate,cut4logCreate,LogCreateCuts),
	build(authentifyWhenIdle,cut4authIdle,AuthIdleCuts),
	flatten([LogTimeCuts,LogErrorCuts,LogUpdateCuts,
	         LogCreateCuts,AuthIdleCuts],GlobalCuts).

dispatchCuts(Raw,OnOrch, OnFragments) :- 
	findall(R, ( member(R,Raw), R = weave(F,[A]), 
	             activity:belongsTo(A,P), \+ isFragment(P)), OnOrch),	
        findall(R, ( member(R,Raw), R = weave(F,[A]), 
	             activity:belongsTo(A,P), isFragment(P)), OnFragments).
	
factorizeCuts([],[]).
factorizeCuts([weave(F,[A])|T], [weave([F],[A])|O]) :- 
	findall(weave(Fp,[A]),member(weave(Fp,[A]),T),[]),!, factorizeCuts(T,O).
factorizeCuts([weave(F,[A])|T], [weave(All,[A])|O]) :- 
	findall(weave(Fp,[A]),member(weave(Fp,[A]),T),L), 
	length(L,Length), Length > 0, 
	findall(Frag,member(weave(Frag,_),L),Matched), flatten([F,Matched],All),
	removeList(L,T,Retracted), factorizeCuts(Retracted,O).
	



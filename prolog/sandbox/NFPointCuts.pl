loadEx :- 
	consult('/Users/mireilleblay-fornarino/Documents/workspaceOpenEmbeddFaros/newCodeAdore/NFPointCuts.pl'),
	consult('/Users/mireilleblay-fornarino/Documents/workspaceOpenEmbeddFaros/newCodeAdore/pointcutsHelper.pl').
	
testAll(R,LErreur,_CorrectedContext,ErreurPaires,LMO,LMF,LWO,LWF) :- 
    introduceStatistic([],NewContextS),
	introducePersistence(NewContextS,NewContextP),
	introduceTimer(NewContextP,NewContextT),
	introduceSecurity(NewContextT,N),
	%print(N),
	length(N,R), 
	simpleLook4reflexivity(N,LErreur,NCorrectedContext),!,
	writeList(NCorrectedContext),
	simpleLook4nonConvergentPairs(NCorrectedContext,ErreurPaires),
	sortWeaving(NCorrectedContext,OnOrchestration,OnFragments),
	getMerges(OnOrchestration,LMergeOnOrchestration),length(LMergeOnOrchestration,LMO),
	getMerges(OnFragments,LMergeOnFragments),length(LMergeOnFragments,LMF),
	findall(X, (member(weave(X,Z,_),OnOrchestration)),LWeaveOnOrchestration),length(LWeaveOnOrchestration,LWO),
	findall(X, (member(weave(X,Z,_),OnFragments)),LWeaveOnFragments),length(LWeaveOnFragments,LWF).
	
	
getList4Seb(NCorrectedContext) :- 
	introduceStatistic([],NewContextS),
	introducePersistence(NewContextS,NewContextP),
	introduceTimer(NewContextP,NewContextT),
	introduceSecurity(NewContextT,N),
	length(N,R), 
	simpleLook4reflexivity(N,LErreur,NCorrectedContext),!,
	simpleLook4nonConvergentPairs(NCorrectedContext,ErreurPaires),
	sortWeaving(NCorrectedContext,OnOrchestration,OnFragments),
	getMerges(OnOrchestration,LMergeOnOrchestration),length(LMergeOnOrchestration,LMO),
	getMerges(OnFragments,LMergeOnFragments),length(LMergeOnFragments,LMF),
	findall(X, (member(weave(X,Z,_),OnOrchestration)),LWeaveOnOrchestration),length(LWeaveOnOrchestration,LWO),
	findall(X, (member(weave(X,Z,_),OnFragments)),LWeaveOnFragments),length(LWeaveOnFragments,LWF),!.
     
 %%Attention je ne sais pas pourquoi mais pour l'instant je ne cherche que sur cmsEMplyee ce qui est pas logique
introduceSecurity(Context,NewContext) :-
      findall(A, (findInvokeActivity(A,ui,_OP,_I,cmsEmployee,_O,_OT),not(isContainedBy(A,cms_authUser))), L),
      sort(L,LRInputEmployee),
      prepareWeavingOnActivityList(LRInputEmployee,authentifyWhenIdle,[[true],[true]],Context, NewContext).
          
	
introducePersistence(Context,NewContext) :-
    (findall(A, findInvokeActivity(A,_S,_OP,_I,cmsEmployee,_O,_OT), L1), sort(L1,LRInputEmployee);LRInputEmployee=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,worker,_O,_OT), L2), sort(L2,LRInputExternalWorker);LRInputExternalWorker=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,cmsEmployee), L3), sort(L3,LROutputEmployee);LROutputEmployee=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,worker), L4), sort(L4,LROutputExternalWorker);LROutputExternalWorker=[]),   

    prepareWeavingOnActivityList(LROutputEmployee,logCreate_CME,[[true],[true]],Context, NewContext1),
    prepareWeavingOnActivityList(LRInputExternalWorker,logCreate_worker,[[true],[true]],NewContext1, NewContext2),
    prepareWeavingOnActivityList(LRInputEmployee,logUpdate_CME,[[onOplu],[true]],NewContext2, NewContext3),     
    prepareWeavingOnActivityList(LROutputExternalWorker,logUpdate_worker,[[onOplu],[true]],NewContext3, NewContext).
 
introduceStatistic(Context,NewContext) :-
	findFailingActivities(_LA,LEX),
	createWeavingsOnFragments(LEX, NC),
	merge(Context,NC,NewContext).


%%pas convaincue par les directives
introduceTimer(Context,NewContext) :-
        findall(A, findInvokeActivity(A,msgBus,wait4msg,_I,worker,_O,_), L3), sort(L3,LMW),
        findall(A, findInvokeActivity(A,msgBus,wait4Msg,_I,worker,_O,_), L31), sort(L31,LMW1),
        merge(LMW,LMW1,LMW2),
        prepareWeavingOnActivityList(LMW2,logTime,[[getSecondParamIn],[getSecondParamIn]],Context, NewContextW),
        findall(A, findInvokeActivity(A,_,assignIntRes,_I,_,_O,_), L4), sort(L4,LAI),
        prepareWeavingOnActivityList(LAI,logTime,[[onOplu],[onOplu]],NewContextW, NewContextAI),
        findall(A, findInvokeActivity(A,_,requestExtRes,_I,_,_O,_), L5), sort(L5,LAE),
        prepareWeavingOnActivityList(LAE,logTime,[[onOplu],[onOplu]],NewContextAI, NewContext).
	
%% ALready implemented.. to be improved
createWeavingsOnFragments([],[]).
createWeavingsOnFragments([(E,X)|LEX],[weave(logError,[X],[[errorle(E)],[onOplu]])|Other]) :-
	createWeavingsOnFragments(LEX,Other) .
	
	
%% Actions for instanciation
errorle(E,_Activity,[bind(errorMsg,E),bind(exc,E)]).
onOplu(Activity,[bind(op,Operation)]) :-
    hasForOperation(Activity,Operation).
%to be implemented
getSecondParamIn(_Activity,[bind(op,param2)]).
	

        
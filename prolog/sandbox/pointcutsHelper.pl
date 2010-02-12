%%consult('/Users/mireilleblay-fornarino/Documents/ADORE/adore/adore/prolog/sandbox/pointcutsHelper.pl').

findInvokeActivity(A,Service,Operation,InputVariable,InputVariableType,OutputVariable,OutputVariableType) :-
hasForKind(A,invoke),
hasForService(A,Service),
hasForOperation(A,Operation),
findVar(in, A,InputVariable, InputVariableType),
findVar(out,A,OutputVariable, OutputVariableType).

findVar(_Inout,_A,Variable,VariableType) :-
	var(Variable), var(VariableType).
findVar(in,A,Variable,VariableType) :-
   usesAsInput(A,Variable),
   hasForType(Variable,VariableType).
findVar(out,A,Variable,VariableType) :-
   usesAsOutput(A,Variable),
   hasForType(Variable,VariableType).


prepareWeavingOnActivityList([],_Fragment,Context, Context).
prepareWeavingOnActivityList([A|ActivityList],Fragment,Context, NewContext) :-
    prepareWeaving(A,Fragment,Context, NewContext1) ,
    prepareWeavingOnActivityList(ActivityList,Fragment,NewContext1, NewContext).
    
prepareWeaving(Activity,Fragment,Context, NewContext) :-
    member(weave(ExistingFragment,[Activity]),Context),!,
    dealWithMergeOnActivity(ExistingFragment, Context,Activity,Fragment,NewContext).
prepareWeaving(Activity,Fragment,Context, NewContext) :-
    gensym(fragment,NF),
    Actions = [doClone(Fragment,NF),
    		   weave(NF, [Activity])],
    append(Context,Actions,NewContext).
    		
dealWithMergeOnActivity(ExistingFragment,Context,_Activity,Fragment,NewContext) :-    
    select(doMerge(Fragments,ExistingFragment),Context,Rest),!,
    gensym(fragment,NF),
    NewContext=[doClone(Fragment,NF),doMerge([NF|Fragments],ExistingFragment)|Rest].
dealWithMergeOnActivity(ExistingFragment,Context,Activity,Fragment,NewContext) :-    
    select(weave(ExistingFragment,[Activity]),Context,Rest),
    gensym(fragment,NF),
    NewContext=[doClone(Fragment,NF),
                doMerge([NF, ExistingFragment],NewMergeFragment),
                weave(NewMergeFragment,[Activity])|Rest].
        		   
%%%Recherche d'invocations avec ui comme Service : 
%%    findall(A, findInvokeActivity(A,ui,_OP,_I,_IT,_O,_OT), L), sort(L,LR).
%%%Recherche d'invocations avec  cmsEmployee en entrée : 
%%     findall(A, findInvokeActivity(A,_S,_OP,_I,cmsEmployee,_O,_OT), L), sort(L,LR).
%%%Recherche d'invocations avec  cmsEmployee en sortie : 
%%     findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,cmsEmployee), L), sort(L,LR).
%%

introducePersistence(Context,NewContext) :-
    (findall(A, findInvokeActivity(A,_S,_OP,_I,cmsEmployee,_O,_OT), L), sort(L,LRInputEmployee);LRInputEmployee=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,externalWorker,_O,_OT), L), sort(L,LRInputExternalWorker);LRInputExternalWorker=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,cmsEmployee), L), sort(L,LROutputEmployee);LROutputEmployee=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,externalWorker), L), sort(L,LROutputExternalWorker);LROutputExternalWorker=[]),   
    instanciateFragment(logCreate,[cmsEmployee],LCCEFragment),
    instanciateFragment(logCreate,[externalWorker],LCEWFragment),
    instanciateFragment(logUpdate,[cmsEmployee],LUCEFragment),
    instanciateFragment(logUpdate,[externalWorker],LUEWFragment),
    prepareWeavingOnActivityList(LRInputEmployee,LCCEFragment,Context, NewContext1),
    prepareWeavingOnActivityList(LRInputExternalWorker,LCEWFragment,NewContext1, NewContext2),
    prepareWeavingOnActivityList(LROutputEmployee,LUCEFragment,NewContext2, NewContext3),     
    prepareWeavingOnActivityList(LROutputExternalWorker,LUEWFragment,NewContext3, NewContext).
 
 %%Attention je ne sais pas pourquoi mais pour l'instant je ne cherche que sur cmsEMplyee ce qui est pas logique
introduceSecurity(Context,NewContext) :-
          findall(A, (findInvokeActivity(A,ui,_OP,_I,cmsEmployee,_O,_OT),not(isContainedBy(A,cms_authUser))), L),
          sort(L,LRInputEmployee),
          prepareWeavingOnActivityList(LRInputEmployee,authentifyWhenIdle,Context, NewContext).
          
          
%%introducePersistence([],NewContext),introduceSecurity(NewContext,N),print(N),length(N,R).
%%105 ...
%introducePersistence([],NewContext),introduceSecurity(NewContext,N),print(N),length(N,R), 
%findall(X, (member(doMerge(X,Z),N)),LMerge),length(LMerge,LM),
%findall(X, (member(weave(X,Z),N)),LWeave),length(LWeave,LW).


instanciateFragment(GenericFragment,ParameterList,[GenericFragment,ParameterList]).
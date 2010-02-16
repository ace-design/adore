%%consult('/Users/mireilleblay-fornarino/Documents/ADORE/adore/adore/prolog/sandbox/pointcutsHelper.pl').
%%Predicats to look for Joint points

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


%%%Recherche d'invocations avec ui comme Service : 
%%    findall(A, findInvokeActivity(A,ui,_OP,_I,_IT,_O,_OT), L), sort(L,LR).
%%%Recherche d'invocations avec  cmsEmployee en entrée : 
%%     findall(A, findInvokeActivity(A,_S,_OP,_I,cmsEmployee,_O,_OT), L), sort(L,LR).
%%%Recherche d'invocations avec  cmsEmployee en sortie : 
%%     findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,cmsEmployee), L), sort(L,LR).
%%
%%===================================================================
%% Predicates to prepare weaving of fragments on activities
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
    gensym(fragment,NewMergeFragment),
    NewContext=[doClone(Fragment,NF),
                doMerge([NF, ExistingFragment],NewMergeFragment),
                weave(NewMergeFragment,[Activity])|Rest].
        		   

%%===================================================================        		   
%%NF examples : looking for join points and preparing weaving

introducePersistence(Context,NewContext) :-
    (findall(A, findInvokeActivity(A,_S,_OP,_I,cmsEmployee,_O,_OT), L), sort(L,LRInputEmployee);LRInputEmployee=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,externalWorker,_O,_OT), L), sort(L,LRInputExternalWorker);LRInputExternalWorker=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,cmsEmployee), L), sort(L,LROutputEmployee);LROutputEmployee=[]),
    (findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,externalWorker), L), sort(L,LROutputExternalWorker);LROutputExternalWorker=[]),   
    instanciateFragment(logCreate_CME,[cmsEmployee],LCCEFragment),
    instanciateFragment(logCreate,[externalWorker],LCEWFragment),
    instanciateFragment(logUpdate_CME,[cmsEmployee],LUCEFragment),
    instanciateFragment(logUpdate,[externalWorker],LUEWFragment),
    prepareWeavingOnActivityList(LRInputEmployee,LCCEFragment,Context, NewContext1),
    prepareWeavingOnActivityList(LRInputExternalWorker,LCEWFragment,NewContext1, NewContext2),
    prepareWeavingOnActivityList(LROutputEmployee,LUCEFragment,NewContext2, NewContext3),     
    prepareWeavingOnActivityList(LROutputExternalWorker,LUEWFragment,NewContext3, NewContext).
 
 %%Attention je ne sais pas pourquoi mais pour l'instant je ne cherche que sur cmsEMplyee ce qui est pas logique
introduceSecurity(Context,NewContext) :-
      findall(A, (findInvokeActivity(A,ui,_OP,_I,cmsEmployee,_O,_OT),not(isContainedBy(A,cms_authUser))), L),
%%Pour avoir des paires critiques
%%findall(A, (findInvokeActivity(A,_,_OP,_I,cmsEmployee,_O,_OT),not(isContainedBy(A,cms_authUser))), L),

          sort(L,LRInputEmployee),
          prepareWeavingOnActivityList(LRInputEmployee,authentifyWhenIdle,Context, NewContext).
          

%%ATTENTION JE GARDE LA REFERENCE A L'ACTIVITE AVANT CLONAGE DONC 
%% IL FAUT LIRE LE DOWEAVE SUR .....

instanciateFragment(GenericFragment,_ParameterList,GenericFragment) :-!.
instanciateFragment(GenericFragment,ParameterList,[GenericFragment,ParameterList]).


%%=======================================================
%% To look for problems between pointcuts
%% SIMPLIFICATION

simplifyWeave(Context,[weave([F],LA)|LR]):-
    select(weave(X,LA),Context,Rest),
    member(doClone(F,X),Context),!,
    simplifyWeave(Rest,LR).
simplifyWeave(Context,[weave(LF,LA)|LR]) :-
    select(weave(X,LA),Context,Rest),
    select(doMerge(LX,X),Rest,Rest1),!,
    findall(F,(member(X1,LX),member(doClone(F,X1),Context)),LF),
    simplifyWeave(Rest1,LR).
simplifyWeave(_Context,[]).

extendedSimplify([],[]).
extendedSimplify([weave([F1|LF],LA)|L],[weave(F1,LA)|LR]) :-
    extendedSimplify([weave(LF,LA)|L],LR).
extendedSimplify([weave([],_LA)|L],LR)  :-
    extendedSimplify(L,LR).



%% Looking for reflexivity
simpleLook4reflexivity(Context,[weave(NF, [Activity])|LR],Rest) :-
    select(weave(NF, [Activity]),Context,Rest1),
    isContainedBy(Activity,NF),!,
    simpleLook4reflexivity(Rest1,LR,Rest).   
simpleLook4reflexivity(Context,LR,[weave(NF, [Activity])|Rest]) :-
    select(weave(NF, [Activity]),Context,Rest1),!,
    simpleLook4reflexivity(Rest1,LR,Rest).  
simpleLook4reflexivity(Context,[],Context).



%% Looking for recursivity
weaveAFragment(A1I,FI) :-
 isContainedBy(A1I,FI),
 isFragment(FI).
 
simpleLook4nonConvergentPairs(Context,Erreur) :-
    select(weave(F1,[A1I|_B1I]),Context,Rest), !,
    ( ( weaveAFragment(A1I,FI), !,
       findall(Erreur1,
                    simpleExistsWeavingOnF1FragmentActivityFromFI(Rest,F1,FI,Erreur1),Erreurs),
      simpleLook4nonConvergentPairs(Rest,Erreur2),
      merge(Erreurs,Erreur2,Erreur)) ;
   simpleLook4nonConvergentPairs(Rest,Erreur) ). 
simpleLook4nonConvergentPairs([],[]).

simpleExistsWeavingOnF1FragmentActivityFromFI(Context,F1,FI,[critical(F1,FI)]) :-
   member(weave(FI,[A1I|_B1I]),Context),
   weaveAFragment(A1I,F1),!.
simpleExistsWeavingOnF1FragmentActivityFromFI(Context,F1,FI,Erreur) :-
   member(weave(FI,[A1I|_B1I]),Context),
   weaveAFragment(A1I,FN),
   simpleExistsWeavingOnF1FragmentActivityFromFI(Context,F1,FN,Erreur).
      
   
%here are non-convergent critical pairs of fragments $(f_1,f_2)$ if the following set pattern of directives is detected :\\
%$weave(f_1,b_1^i), weave(f_i,b_i^j), weave(f_j,b_j^2), weave(f_2,b_2^1) $ where $b_1^i$ defines a block of activities in a fragment $i$.
    

%% To prepare clone and merge generation
%Le tri n'est pas encore appelé
sortWeaving(Context,NotOnFragment,OnFragment) :-
    select(weave(F1,[A2|LA2]),Context,Rest1),
    sortWeaving(Rest1,NotOnFragment1,OnFragment1),
    ( (weaveAFragment(A2,_F2), !, OnFragment=[weave(F1,[A2|LA2])|OnFragment1],NotOnFragment=NotOnFragment1 ) ; 
       (NotOnFragment=[weave(F1,[A2|LA2])|NotOnFragment1],OnFragment=OnFragment1) ).
 sortWeaving([],[],[]).

%%=======================================================





%% Version basée sur le code étendu à oublier
%%A OPtimiser sur la mise en facteur du weave
look4recursivity(Context,[weave(NF, [Activity])|LR],Rest) :-
    select(weave(NF, [Activity]),Context,Rest1),
    isContainedBy(Activity,Fragment),
    select(doClone(Fragment,NF),Rest1,Rest2),!,
    look4recursivity(Rest2,LR,Rest).
look4recursivity(Context,[weave(NF, [Activity])|LR],Rest) :-
    member(weave(NFs, [Activity]),Context),
    select(doMerge(Fragments,NFs),Context,Rest1), 
    isContainedBy(Activity,Fragment),
    select(NF,Fragments,OtherFragments),
    select(doClone(Fragment,NF),Rest1,Rest2),!,
	look4recursivity([doMerge(OtherFragments,NFs)|Rest2],LR,Rest).	    
look4recursivity(Context,[],Context).

 
getFragments([X|LX],[F|LF],Context) :-
    member(doClone(F,X),Context),
    getFragments(LX,LF,Context) .
getFragments([],[],_Context). 
           
 
findPrecedingWeaveOnFragment(Context,Fragment,Preceding2Fragment,Rest2) :-
    select(weave(F,[A|LA]),Context,Rest) , 
    isContainedBy(A,Fragment),!,
    do4Preceding(weave(F,[A|LA]), Rest,Preceding2Fragment,Rest2).
findPrecedingWeaveOnFragment(Context,_Fragment,[],Context). 

do4Preceding(weave(F,[A|LA]), Rest,Preceding2Fragment,Rest2) :-   
    select(doClone(Forigin,F),Rest,Rest1),
    findPrecedingWeaveOnFragment(Rest1,Forigin,Preceding,Rest2),
    append(Preceding,[doClone(Forigin,F),weave(F,[A|LA])],Preceding2Fragment).
    
do4Preceding(weave(F,[A|LA]), Rest,Preceding2Fragment,Rest2) :- 
    select(doMerge(Flist,F),Rest,Rest1),
    findPrecedingweaveSetOnFragment(Flist,F,Rest1,Preceding,Rest2),
    append(Preceding,[doMerge(Flist,F),weave(F,[A|LA])],Preceding2Fragment).
 
 %%A REVOIR MAIS Y  A T IL UNE SOLUTION ???
findPrecedingweaveSetOnFragment([F1|Flist],F,Context,Preceding,Rest) :-
    doClone(Forigin,F1),
    findPrecedingWeaveOnFragment(Context,Forigin,Preceding2Fragment,Rest2),
    findPrecedingweaveSetOnFragment(Flist,F,Context,Preceding2Set,Rest3),
    merge(Preceding2Set,Preceding2Fragment,L),
    list_to_set(L,Preceding),
    intersection(Rest2,Rest3,Rest).
findPrecedingweaveSetOnFragment([],_F,Context,[],Context).

 
 


%% Looking for recursivity
look4nonConvergentPairs(Context,Erreur) :-
  member(weave(F1,[A1I|_B1I]),Context),
  weaveAFragment(A1I,FI),
  member(doClone(Forigin,F1),Context),
  existsWeavingOnF1FragmentActivityFromFI(Context,Forigin,FI,Erreur),!.
 look4nonConvergentPairs(_Context,[]). 
existsWeavingOnF1FragmentActivityFromFI(Context,F1,FI,[critical(F1,FI)]) :-
  member(doClone(FI,NFI),Context),
  ( member(weave(NFI,[AI1|_]),Context) ;
    member(doMerge(L,NF2),Context),
    member(NFI,L),
    member(weave(NF2,[AI1|_]),Context)),
  weaveAFragment(AI1,F1),!.
 
existsWeavingOnF1FragmentActivityFromFI(Context,F1,FI,[critical(F1,FI)|Paire]) :-
  member(doClone(FI,NFI),Context),
( member(weave(NFI,[AI1|_]),Context) ;
    member(doMerge(L,NF2),Context),
    member(NFI,L),
    member(weave(NF2,[AI1|_]),Context)),
  weaveAFragment(AI1,FI2),
  existsWeavingOnF1FragmentActivityFromFI(Context,F1,FI2,Paire).
  
%existsWeavingOnF1FragmentActivityFromFI(Context,_F1,_FI,[]).

 


   
%%=======================================================
%% Test Global
%%========================================================
%%introducePersistence([],NewContext),introduceSecurity(NewContext,N),print(N),length(N,R).
%%105 ...
testSecurityPersistency(R,LM,LW,_LR,LErreur,_CorrectedContext,ErreurPaires,LMO,LMF,LWO,LWF) :- 
	introducePersistence([],NewContext),introduceSecurity(NewContext,N),print(N),length(N,R), 
	findall(X, (member(doMerge(X,Z),N)),LMerge),length(LMerge,LM),
	findall(X, (member(weave(X,Z),N)),LWeave),length(LWeave,LW),
	simplifyWeave(N,CC), print(CC), extendedSimplify(CC,NCC),!,
%%	(look4recursivity(N,LR,CorrectedContext);CorrectedContext=N, print('pas de recursivite')),
	simpleLook4reflexivity(NCC,LErreur,NCorrectedContext),!,
	simpleLook4nonConvergentPairs(NCorrectedContext,ErreurPaires),
%	look4nonConvergentPairs(CorrectedContext,ErreurPaire),
	sortWeaving(NCorrectedContext,OnOrchestration,OnFragments),
	getMerges(OnOrchestration,LMergeOnOrchestration),length(LMergeOnOrchestration,LMO),
	getMerges(OnFragments,LMergeOnFragments),length(LMergeOnFragments,LMF),
	findall(X, (member(weave(X,Z),OnOrchestration)),LWeaveOnOrchestration),length(LWeaveOnOrchestration,LWO),
	findall(X, (member(weave(X,Z),OnFragments)),LWeaveOnFragments),length(LWeaveOnFragments,LWF).
	
getMerges([], []).
getMerges([weave(F,LA)|Context], AllMerges) :-
   findall(weave(Fi,LA),member(weave(Fi,LA),Context),Fl),
   subtract(Context, Fl, Rest),
   getMerges(Rest, Merges),
  (  (Fl == [],AllMerges = Merges) ; AllMerges=[[weave(F,LA)|Fl]|Merges]).
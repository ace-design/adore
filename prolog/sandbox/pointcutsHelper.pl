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

%LA activities executed when error (LE) is thrown from (LX).
findFailingActivities(LA,LEX):-
    findall(Act,onFailure(Act,X,E),LA),
    findall((E,X),onFailure(Act,X,E),LEX).
    

%%%Recherche d'invocations avec ui comme Service : 
%%    findall(A, findInvokeActivity(A,ui,_OP,_I,_IT,_O,_OT), L), sort(L,LR).
%%%Recherche d'invocations avec  cmsEmployee en entrée : 
%%     findall(A, findInvokeActivity(A,_S,_OP,_I,cmsEmployee,_O,_OT), L), sort(L,LR).
%%%Recherche d'invocations avec  cmsEmployee en sortie : 
%%     findall(A, findInvokeActivity(A,_S,_OP,_I,_IT,_O,cmsEmployee), L), sort(L,LR).
%%
%%===================================================================
%% Predicates to prepare weaving of fragments on activities
prepareWeavingOnActivityList([],_Fragment,_,Context,Context).
prepareWeavingOnActivityList([A|LA],Fragment,DirectivePattern,Context,[weave(Fragment,[A],DirectivePattern)|NewContext]) :-
    prepareWeavingOnActivityList(LA,Fragment,DirectivePattern,Context,NewContext).


%%========== 



%%% JoinPoint tests : based on simplified weaving list
%% Looking for reflexivity
simpleLook4reflexivity(Context,[weave(NF, [Activity],D)|LR],Rest) :-
    select(weave(NF, [Activity],D),Context,Rest1),
    isContainedBy(Activity,NF),!,
    simpleLook4reflexivity(Rest1,LR,Rest).   
simpleLook4reflexivity(Context,LR,[weave(NF, [Activity],D)|Rest]) :-
    select(weave(NF, [Activity],D),Context,Rest1),!,
    simpleLook4reflexivity(Rest1,LR,Rest).  
simpleLook4reflexivity(Context,[],Context).


%% Looking for recursivity
weaveAFragment(A1I,FI) :-
 isContainedBy(A1I,FI),
 isFragment(FI).
 
 

simpleLook4nonConvergentPairs(Context,Erreur) :-
    select(weave(F1,[A1I|_B1I],_D1),Context,Rest), !,
    ( ( weaveAFragment(A1I,FI), !,
       findall(Erreur1,
                    simpleExistsWeavingOnF1FragmentActivityFromFI(Rest,F1,FI,Erreur1),Erreurs),
      simpleLook4nonConvergentPairs(Rest,Erreur2),
      merge(Erreurs,Erreur2,Erreur)) ;
   simpleLook4nonConvergentPairs(Rest,Erreur) ). 
simpleLook4nonConvergentPairs([],[]).

simpleExistsWeavingOnF1FragmentActivityFromFI(Context,F1,FI,[critical(F1,FI)]) :-
   member(weave(FI,[A1I|_B1I],_DI),Context),
   weaveAFragment(A1I,F1),!.
simpleExistsWeavingOnF1FragmentActivityFromFI(Context,F1,FI,Erreur) :-
   member(weave(FI,[A1I|_B1I],_DI),Context),
   weaveAFragment(A1I,FN),
   simpleExistsWeavingOnF1FragmentActivityFromFI(Context,F1,FN,Erreur).
      
   
%here are non-convergent critical pairs of fragments $(f_1,f_2)$ if the following set pattern of directives is detected :\\
%$weave(f_1,b_1^i), weave(f_i,b_i^j), weave(f_j,b_j^2), weave(f_2,b_2^1) $ where $b_1^i$ defines a block of activities in a fragment $i$.
    

%% To prepare clone and merge generation
%Le tri n'est pas encore appelé
sortWeaving(Context,NotOnFragment,OnFragment) :-
    select(weave(F1,[A2|LA2],D1),Context,Rest1),
    sortWeaving(Rest1,NotOnFragment1,OnFragment1),
    ( (weaveAFragment(A2,_F2), !, OnFragment=[weave(F1,[A2|LA2],D1)|OnFragment1],NotOnFragment=NotOnFragment1 ) ; 
       (NotOnFragment=[weave(F1,[A2|LA2],D1)|NotOnFragment1],OnFragment=OnFragment1) ).
 sortWeaving([],[],[]).

%%=======================================================

getMerges([], []).
getMerges([weave(F,LA,D)|Context], AllMerges) :-
   findall(weave(Fi,LA,Di),member(weave(Fi,LA,Di),Context),Fl),
   subtract(Context, Fl, Rest),
   getMerges(Rest, Merges),
  (  (Fl == [],AllMerges = Merges) ; AllMerges=[[weave(F,LA,D)|Fl]|Merges]).
 %consult('/Users/mireilleblay-fornarino/Documents/ADORE/adore/prolog/src/tests/t0.pl').
 %process(P),check4ForgottenJointPoints(P,Conflicts).
 
 equivalent(A1,A2) :- 
   activity(A1),activity(A2),hasForKind(A1,K),hasForKind(A2,K),
   equivalentAccordingToActivityKind(A1,A2,K).
equivalentAccordingToActivityKind(A1,A2,invoke) :-
   hasForService(A1,I),hasForService(A2,I),
   hasForOperation(A1,O),hasForOperation(A2,O),!,
   equivalentAccordingToVariables(A1,A2).
equivalentAccordingToVariables(_A1,_A2) :-
  print('equivalentAccordingTo Variables in input and output has to be implemented').

notEquivalent(A1,A2) :-
	\+ equivalent(A1,A2).

checkProcess(P,Conflicts) :- 
  process(P),
  check4ForgottenJointPoints(P,Conflicts).

%===========================
%%Corriger pour ne plus prendre (je crois) les fragments sur fragments
%et/ou les blocs... car dans ce cas on peut considerer que l'interaction est deja posée..
check4ForgottenJointPoints(P,Conflicts) :-
  contextTarget(C,P), %do it for all the contexts
  findall((B,F), applyFragment(_,C, B, F), ListBF),
  checkForAllControlledActivities(ListBF,Conflicts).
  
checkForAllControlledActivities([],[]) :- !.

checkForAllControlledActivities(ListBF, Conflicts) :-
   checkForAllControlledActivities(ListBF,ListBF,Conflicts).
   
checkForAllControlledActivities([],_ListBF,[]) .
checkForAllControlledActivities([(B,F)|L],ListBF,Conflicts) :-
	%controlledActivities(ListBF,B,ControlledActivities),
	activityBlock(_,B,ControlledActivities),
	fragments(ListBF,F,FragmentsToTest),
	areFragmentsReusingActivity(FragmentsToTest, ControlledActivities, ConflictsOnF),
	buildConflictForgottenJointPoints(F,ConflictsOnF,CF),!,
	checkForAllControlledActivities(L,ListBF,OtherConflicts) ,
	append(CF, OtherConflicts, Conflicts).

areFragmentsReusingActivity([],_ControlledActivities,[]).
areFragmentsReusingActivity([F|Fragments],ControlledActivities,[(F,ReusedActivities)|Conflicts]) :-
    isActivityReusedInFragment(F,ControlledActivities,ReusedActivities),
    areFragmentsReusingActivity(Fragments,ControlledActivities,Conflicts).

isActivityReusedInFragment(_F,[],[]).
isActivityReusedInFragment(F,ControlledActivities,ReusedActivities ) :-
    activityReusedInFragment(F,ControlledActivities,ReusedActivities).

activityReusedInFragment(F,ControlledActivities,ReusedActivities) :-
	findall(X,(member(X,ControlledActivities), isActivityReusedInFragment(X,F)),ReusedActivities).
	
isActivityReusedInFragment(Activity,F) :-
	findall(X,isContainedBy(X,F),LA),
	\+ maplist(notEquivalent(Activity),LA).

controlledActivities([],_B,[]) :-!.
controlledActivities(ListBF,B,ControlledActivities) :-
    delete(ListBF,(B,_),LB),
    findall(A,(member((Block,_),LB),activityBlock(_,Block,LA),member(A,LA)),ControlledActivities).

fragments([],_F,[]) :-!.
fragments(ListBF,F,FragmentsToTest) :-
    delete(ListBF,(_B,F),LF),
    findall(Fx,member((_,Fx),LF),FragmentsToTest).
    

buildConflictForgottenJointPoints(_F,[],[]).
buildConflictForgottenJointPoints(F,ConflictsOnF,C) :-
	dealWithConflicts(ConflictsOnF,Conflicts),
	dwc(Conflicts,F,C).
dwc([],_,[]).	
dwc(Conflicts,F,[warning('DoesFragmentByAppliedOn',F,Conflicts)]).

dealWithConflicts([],[]).
dealWithConflicts([(_F,[])|ConflictsOnF],Conflicts) :-!,
    	dealWithConflicts(ConflictsOnF,Conflicts).
dealWithConflicts([(F,L)|ConflictsOnF],[(F,L)|Conflicts]) :-
    	dealWithConflicts(ConflictsOnF,Conflicts).    	
    
%================================
%ATTention j'utilise pour l'instant le fait que je ne detecte pas dequivalence sur pred et succ etc
%je dois absolument prendre en compte les variables pour que cela est du sens.
%Actuellement je trie a la main
% c'est a reecrire pour ne pas detecter au sein d'une orchestration.. ou les mettre à part.

checkForEquivalentActivities(P,Conflicts) :- 
    process(P), findall(X,isContainedBy(X,P),LA),
    checkForEquivalentActivities(LA,LA,Conflicts).
 
checkForEquivalentActivities([],_LA,[]).  
checkForEquivalentActivities([A|RA],LA,Conflicts) :-
    delete(LA,A,L_A),
    findall(X,(member(X,L_A),equivalent(X,A)),LEquiv),
    deleteElements(LA,LEquiv,NLA),
    deleteElements(RA,LEquiv,NRA),
    buildConflictEquivalentActivities([A|LEquiv], ConflictsOnA),
    checkForEquivalentActivities(NRA,NLA,ConflictsOnRest),
    append(ConflictsOnA,ConflictsOnRest,Conflicts).
 
deleteElements(LA,[],LA).   
deleteElements(LA,[A|L2Delete],NL) :-
    delete(LA,A,L),
    deleteElements(L,L2Delete,NL) .

buildConflictEquivalentActivities([_], []) :-!.
buildConflictEquivalentActivities([_], []) :- !.
buildConflictEquivalentActivities(LEquiv, [warning('areTheseActivitiesDuplicated',LEquiv) ]).

    
   
    
%================================
checkForComplementaryBranches(P,Conflicts) :-
    process(P), 
    findall(X,isContainedBy(X,P),LA),
    findall((Test,Variable,Valeur), (member(A,LA), isGuardedBy(A,Test,Variable,Valeur)),LTVV),
    areTherePairs(LTVV,C),
	buildConflictNoComplementaryBranch(P,C,Conflicts).
 
areTherePairs([],[]).
areTherePairs([(Test,Variable,Valeur)|L],C12) :-
    findall((Test,Variable,V), (member((Test,Variable,V),L),V\=Valeur),LNegation),
    dealWithPairs(Test,Variable,Valeur,LNegation,C1),
    deleteElements(L,LNegation,NL),
    delete(NL,(Test,Variable,Valeur),NNL),
    areTherePairs(NNL,C2),
    append(C1,C2,C12).
	
dealWithPairs(Test,Variable,Valeur,[],[(Test,Variable,Valeur)]):-!.
dealWithPairs(_Test,_Variable,_Valeur,_Negation,[]).
	   
 buildConflictNoComplementaryBranch(_P,[],[]) :- !.
  buildConflictNoComplementaryBranch(P,LV,[warning('noComplementaryBranchFor',P,LV) ]).
 
 %==========================
checkForConcurrentEnd(P,Conflicts) :-
     process(P),
     findConcurrentEndingActivities(P,Conflicts).  
     
findConcurrentEndingActivities(P,C) :-
        findall(A,( isContainedBy(A,P), isEndingActivity(A)),LA),
        getConcurrentActivityPair(P,LA,C).

    
getConcurrentActivityPair(_,[],[]) :- !.    
getConcurrentActivityPair(_,[_A],[]) :- !.
getConcurrentActivityPair(P,[A1|LA],C) :-
     findall( C , 
     		( member(A2,LA),
     		  concurrentActivities(A1,A2,P),
              buildConflictForConcurrentEnd(P,[A1,A2],C) ),
              ConflictOnA),
     getConcurrentActivityPair(P,LA,COnRest),
     append(ConflictOnA,COnRest,C).        
     			
concurrentActivities(A1,A2,P) :-
     process(P),
     isContainedBy(A1,P),
     isContainedBy(A2,P), A1\=A2,
     pathRec(RCV,A1,P1),pathRec(RCV,A2,P2),
     ( noContained(A1,A2,P1,P2) ; !,fail),
     ( noExcludingGuards(A1,A2) ; !,fail).
 
noContained(A1,_A2, _P1,P2) :-
     member(A1,P2), !, fail.
noContained(_A1,A2, P1,_P2) :-
     member(A2,P1), !, fail.
noContained(_A1,_A2, _P1,_P2).

     
pathRec(A1,A2,[A1,A2]) :-
      pathControl(A1,A2).
pathRec(A1,A3,[A1|P]) :-
      pathControl(A1,A2),
      pathRec(A2,A3,P).   

pathControl(A, B) :-
	waitFor(B, A).
pathControl(A, B) :-
	isGuardedBy(B, A, _, _).
pathControl(A, B) :-
	weakWait(B, A).
	
	
guards(A1,LG) :-
    findall( (Test,Variable,V1), 
		     isGuardedBy(A1,Test,Variable,V1), LG1),
    findall( LGP, 
    		 (pathControl(P,A1), guards(P,LGP)), LG2),
    append(LG2,LG22),
    append(LG1,LG22,LG).
noExcludingGuards(A1,A2) :-
    excludingGuards(A1,A2,L), L=[].
excludingGuards(A1,A2,L) :-
       guards(A1,LG1) , guards(A2,LG2),
       intersection(LG1,LG2,LG12),
       subtract(LG1,LG12,LG11),
       subtract(LG2,LG12,LG22),
       findall((T,V,Value), 
       	( member( (T,V,Value) , LG11), member((T,V,_V2),LG22)),
       	L).
%noExcludingGuards(A1,A2) :-
%    findall((Test,Variable,A1,V1,A2,V2),  
%    			( 	isGuardedBy(A1,Test,Variable,V1),
%    				isGuardedBy(A2,Test,Variable,V2),
%    				V1 \= V2),
%        	L),!,
%     L = [].

buildConflictForConcurrentEnd(_P,[],[]) :- !.      
buildConflictForConcurrentEnd(P,[A1,A2], [warning('ConcurrentEnd',P,[A1,A2]) ]).

isEndingActivity(A) :-
    hasForKind(A,reply).
isEndingActivity(A) :-
    hasForKind(A,throw).    
isEndingActivity(A) :-
    hasForKind(A,successors).

%===============
% POUBELLE
noCommonFailure(P1,P2) :-
    findall( (A1 , FailingActivity, Error) ,
             ( member(FailingActivity,P1), 
               onFailure(A1,FailingActivity,Error), member(A1,P1),
               notOnSameFailure(A1,FailingActivity,Error,P2) ), L),
     !, L = [],
    findall( (A1 , FailingActivity, Error) ,
             ( member(FailingActivity,P2), member(A1,P2),
               onFailure(A1,FailingActivity,Error),
               notOnSameFailure(A1,FailingActivity,Error,P1) ), L), !,
     L = [].
                 
            
           
 notOnSameFailure(_A,FailingActivity,Error,P) :-
    member(FailingActivity,P),
 	member(A2,P),
 	onFailure(A2,FailingActivity,ErrorB),
 	Error \= ErrorB.
 	
 notOnSameFailure(_A,FailingActivity,Error,P) :-
    member(FailingActivity,P),
 	member(A2,P),
 	\+  onFailure(A2,FailingActivity,Error).
             
%noExcludingGuard(P1,P2) :-
%    findall((Test,Variable,A1,V1,A2,V2), (member(A1,P1), isGuardedBy(A1,Test,Variable,V1),
%    								 member(Test,P2), isGuardedBy(A2,Test,Variable,V2),
%    								 V1 \= V2),
%        	L),!,
%     L = [].
 

    
    
    
%==========================
% ne gere pas les liens faibles... du coup pas grand chose...
checkForConcurrentAccesses(P,Conflicts)  :-
    variablesInOutput(P,LV),
    testConcurrentAccessesTo(LV,P,Pairs),
    buildConflictForConcurrentAccess(P,Pairs,Conflicts).

buildConflictForConcurrentAccess(_P,[],[]) :- !.    
buildConflictForConcurrentAccess(P,[(_V,[])|L], LW) :- !,
	buildConflictForConcurrentAccess(P,L,LW).  
buildConflictForConcurrentAccess(P,[(V,LP)|L],
						 [warning('ConcurrentAccess',P,V,LP) | LW]) :-
	buildConflictForConcurrentAccess(P,L,LW).
  
variablesInOutput(P,LV) :-
    	process(P),
    	findall(V, (isContainedBy(A,P),usesAsOutput(A,V)),LV).
    	

testConcurrentAccessesTo([],_P,[]) :- !.
testConcurrentAccessesTo([V|LV],P,[(V,Conflict)|C]) :-
    findall((A1,A2), (usesAsOutput(A1,V), 
    				  usesAsOutput(A2,V), A1 \= A2,
    				  concurrentActivities(A1,A2,P)), Lout),
    findall((A1,A2), (usesAsOutput(A1,V), 
    				  usesAsInput(A2,V),A1 \= A2,
    				  concurrentActivities(A1,A2,P)), Linout),   				  
	append(Lout,Linout,Conflict),
	testConcurrentAccessesTo(LV,P,C).
	
%==========================
%Ne gere pas les to set

checkForNonInitialisedVariables(P,Conflicts)  :-
    variablesInInput(P,LVD),list_to_set(LVD,LV),
    testForgottenInitialisation(LV,P,Pairs),
    buildConflictForUninitializedVariables(P,Pairs,Conflicts).

buildConflictForUninitializedVariables(_P,[],[]) :- !.    
buildConflictForUninitializedVariables(P,Pairs, 
						 warning('Non-initializedVariables',P, Pairs)).
						 
variablesInInput(P,LV) :-
    	process(P),
    	findall(V, (isContainedBy(A,P),usesAsInput(A,V)),LV).
    	

testForgottenInitialisation([],_P,[]) :- !.

testForgottenInitialisation([V|LV],P,Pairs) :-
     hasForInitValue(V,_Value),!,
     testForgottenInitialisation(LV,P,Pairs).
testForgottenInitialisation([V|LV],P,Pairs) :-
     hasForParameter(P,Par),atom_concat(P,'_',PX),atom_concat(PX,Par,V),!,
     testForgottenInitialisation(LV,P,Pairs).     
testForgottenInitialisation([V|LV],P,Pairs) :-
     isHookVariable(P,V),!,
     testForgottenInitialisation(LV,P,Pairs).   
testForgottenInitialisation([V|LV],P,Pairs) :-
     fieldAccess(V,Var,_Field),!,
     ( isHookVariable(P,Var), PairsV=[],  ! ; 
       (hasForParameter(P,Par),atom_concat(P,'_',PX),atom_concat(PX,Par,Var), PairsV=[], ! );
       (findall(O1, usesAsOutput(O1,Var),LO),
     	  findall(I1, usesAsInput(I1,V),LI),
          testActivitiesInputOutput(P,V,LI,LO,PairsV),!)
     ),
     testForgottenInitialisation(LV,P,PairsRest),
	 append(PairsV,PairsRest,Pairs).
testForgottenInitialisation([V|LV],P,Pairs) :-
     getActivitiesOnVariable(V,LI,LO),
	 testActivitiesInputOutput(P,V,LI,LO,PairsV),!,
	 testForgottenInitialisation(LV,P,PairsRest),
	 append(PairsV,PairsRest,Pairs).


isHookVariable(P,V) :-
    isContainedBy(A,P), hasForKind(A,hook),
    usesAsInput(A,V).
getActivitiesOnVariable(V,LI,LO) :-
	     findall(O1, usesAsOutput(O1,V),LO),
	     findall(I1, usesAsInput(I1,V),LI).
	     
testActivitiesInputOutput(_P,_,[],_,[]). 
testActivitiesInputOutput(P, V, [I|LI],LO,Pairs) :-
         noPathWithoutAssignment(P,I,LO),
	     %member(O,LO),
	     %pathRecG(O,I,_),!,
	     testActivitiesInputOutput(P,V,LI,LO,Pairs).
testActivitiesInputOutput(P,V, [I|LI],LO,[(V,I)|Pairs]) :-
	     testActivitiesInputOutput(P,V,LI,LO,Pairs).
	     
pathRecG(A1,A2,[A1,A2]) :-
      path(A1,A2).
pathRecG(A1,A3,[A1|P]) :-
      path(A1,A2),
      pathRecG(A2,A3,P). 

noPathWithoutAssignment(P,I,LO) :-          
	getInitialActivity(P,A),
        findall(C,
                  ( pathRecG(A,I,C), 
                    \+ (member(O,LO), member(O,C))
		    ), LC),
        !, LC = [].
        
getInitialActivity(P,A) :-
     isContainedBy(A,P), 
     findall(X, path(X,A),LA),
     LA = [].
         	

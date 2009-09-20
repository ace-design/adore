weave(Orchestration) :-
     findall(Actions,
     		 (contextTarget(Composition,Orchestration),
     		  applyFragment(Apply,Composition,Block,Fragment),
     		  prepareWeave(Orchestration,Apply,Block,Fragment,Actions)),
     		  ActionList),
     flatten(ActionList,List),
     list_to_set(List,Set),
     executeActionSet(Set),
     normalise(Orchestration).	  
     
prepareWeave(Orchestration,Apply,Block,Fragment,Actions) :-
	prepareActivitiesAdding(Apply,Fragment,Orchestration,ActivitiesAdding) ,
 	prepareRelationsAdding(Apply,Fragment,Orchestration,Block,RelationsAdding),
 	dealWithHookVariables(Apply,Fragment,Orchestration,ActivitiesAdding,NewActivitiesAdding),
	append(NewActivitiesAdding,RelationsAdding,Actions).	


%1)Renvoie les actions qui ajoutent toutes les activités non hook à l'orchestration
%2)Y compris les variables
prepareActivitiesAdding(Apply,Fragment,NewOrchestration,AddingActionList) :-
    findall(ActionList,(isContainedBy(X,Fragment),
    		   copyActivity(Apply,X,NewOrchestration,ActionList)),ALL),
    flatten(ALL,AddingActionList).


%Chaque activité est
% 1) ajoutée à la base sous le apply + nomOrigine
%2) Sa sorte
%3) Ses variables sont ajoutées en étant toutes renommées 
%4) Activitée Ajoutée à la nouvelleOrchestration
%Nous renommerons les variables du hook plus tard par une action de renaming?
%ne pas oublier de les retirer alors...
copyActivity(_Apply,A,_NewOrchestration,[]) :-
	hasForKind(A,hook),!.
    
copyActivity(Apply,A,NewOrchestration,
	[createActivity(NewActivity),
	setActivityKind(NewActivity,K),
	setContainment(NewActivity,NewOrchestration),
	ActionList]) :-
    atom_concat(Apply,A,NewActivity),
    hasForKind(A,K),
    copyActivityAccordingToKind(Apply,A,NewOrchestration,KindActList),
    getInputVariables(A,VarlistIn),
    copyInputVariables(Apply,A,VarlistIn,AddingInputVar),
    getOutputVariables(A,VarlistOut),
    copyOutputVariables(Apply,A,VarlistOut,AddingOutputVar),
    append(KindActList,AddingInputVar,KA),
    append(KA,AddingOutputVar,ActionList). 
    
 
copyInputVariables(_Apply,_A,[],[]).
copyInputVariables(Apply,A,[V|VarlistIn],[
	createVariable(NewVar),
	setVariableType(NewVar,T),
	addAsInput(NewVar,A) |
	AddingInputVar]) :-
    atom_concat(Apply,V,NewVar),
    hasForType(V,T),
    copyVariableAccordingToKind(Apply,V,VarKindActList),
    copyInputVariables(Apply,A,VarlistIn,AddingInputVarRest),
    append(VarKindActList,AddingInputVarRest,AddingInputVar).
     
copyOutputVariables(_Apply,_A,[],[]).
copyOutputVariables(Apply,A,[V|VarlistIn],[
	createVariable(NewVar),
	setVariableType(NewVar,T),
	addAsOutput(NewVar,A) |
	AddingInputVar]) :-
    atom_concat(Apply,V,NewVar),
    hasForType(V,T),
    copyVariableAccordingToKind(Apply,V,VarKindActList),
    copyOutputVariables(Apply,A,VarlistIn,AddingInputVarRest),
    append(VarKindActList,AddingInputVarRest,AddingInputVar).
         

getInputVariables(A,VarList) :-
    findall(V,usesAsInput(A,V),VarList).
getOutputVariables(A,VarList) :-
    findall(V,usesAsOutput(A,V),VarList).	    

%2) Remplace les variables de l'activité hook par leur nom dans l'orchestration.
% Cette action se fait dans le nouveau fragment.
dealWithHookVariables(Apply,Fragment,Orchestration,ActivitiesAdding,NewActivitiesAdding).
		
%1)determine les 1ere et dernieres activities du bloc
%2)détermine les prédecesseurs et successeurs du bloc
%3)remplace dans les relations 
% - les successeurs 
% - les predecesseurs
%- h<X => h est remplacé par les dernieres
%- X<h =>  h est remplacé par les premieres
%%=== Attention c'est ici que on ajoutera des nop mais on en n'a pas besoin pour Faros
prepareRelationsAdding(Apply,Fragment,_Orchestration,Block,RelationsAdding) :-
 	getFirstActivitiesOfBlock(Block,FirstActivities) ,
 	getLastActivitiesOfBlock(Block,LastActivities),
 	getSuccessorsOfActivities(LastActivities,Succ),
 	getPredecessorsOfActivities(FirstActivities,Pred),
 	getActivities(Fragment,LA),
 	workOnRelationsAdding(Apply,LA,FirstActivities,LastActivities,
 		Succ,Pred,RelationsAdding).
 	
workOnRelationsAdding(_Apply,[],_FirstActivities,_LastActivities,_Succ,_Pred,[]). 	 
workOnRelationsAdding(Apply,[A|LA],FirstActivities,LastActivities,Succ,Pred,RelationsAdding) :-
	 findall(Actions,(waitFor(A,P),workOnRelationWait(Apply,A,P,FirstActivities,LastActivities,Succ,Pred,Actions)),WaitActionsList),
 	 findall(Actions,(weakWait(A,P),workOnRelationWeak(Apply,A,P,FirstActivities,LastActivities,Succ,Pred,Actions)),WeakActionsList),
 	 findall(Actions,(isGuardedBy(A,P,Var,Value),workOnRelationGuarded(Apply,A,P,Var,Value,Actions)),GuardedActionsList),
 	 append(WaitActionsList,WeakActionsList,WW),
 	 append(WW,GuardedActionsList,RelationsAdding1),
 	 workOnRelationsAdding(Apply,LA,FirstActivities,LastActivities,Succ,Pred,RelationsAdding2),
 	 append(RelationsAdding1,RelationsAdding2,RelationsAdding). 
 	  	 
workOnRelationWait(_Apply,A,P,_FirstActivities,_LastActivities,_Succ,_Pred,[]) :-
    hasForKind(A,hook),
    hasForKind(P,predecessors),!.
 workOnRelationWait(_Apply,P,A,_FirstActivities,_LastActivities,_Succ,_Pred,[]) :-
    hasForKind(A,hook),
    hasForKind(P,successors), !.
 workOnRelationWait(Apply,A,P,FirstActivities,_LastActivities,_Succ,Pred,ActionList) :-
    hasForKind(A,hook),!,
    atom_concat(Apply,P,NP),
    prepareRemoveRelations(waitFor,FirstActivities,Pred,RemoveActions),
    prepareAddingRelation(waitFor,FirstActivities,NP,AddingActions),
    append(RemoveActions,AddingActions,ActionList).
 workOnRelationWait(Apply,S,A,_FirstActivities,LastActivities,Succ,_Pred,ActionList) :-
    hasForKind(A,hook),!,
    atom_concat(Apply,S,NS),
    prepareRemoveRelations(defWaitFor,Succ,LastActivities,RemoveActions),
    prepareAddingRelation(defWaitFor,NS,LastActivities,AddingActions),
    append(RemoveActions,AddingActions,ActionList).
 workOnRelationWait(Apply,S,A,_FirstActivities,_LastActivities,_Succ,_Pred,AddingActions) :-
    atom_concat(Apply,A,NA),
    atom_concat(Apply,S,NS),
    prepareAddingRelation(defWaitFor,NS,NA,AddingActions).

prepareAddingRelation(Wait,S,A,[t]) :-
	atom(S),atom(A),!,
	t =.. [Wait,S,A].
prepareAddingRelation(_waitI,[],_A,[]) .
prepareAddingRelation(_wait,_A,[],[]) .
prepareAddingRelation(Wait,[S|L],A,ActionList) :-
	prepareAddingRelation(Wait,S,A,AddingActions),
	prepareAddingRelation(Wait,L,A,AddingActionList),
	append(AddingActions,AddingActionList,ActionList).
prepareAddingRelation(Wait,S,[A|L],ActionList) :-
    atom(S),
	prepareAddingRelation(Wait,S,A,AddingActions),
	prepareAddingRelation(Wait,S,L,AddingActionList),
	append(AddingActions,AddingActionList,ActionList).
	
%%==================
%% FUNCTIONS
%%===============
getActivities(P,LA) :-
  	findall(X,isContainedBy(X,P),LA).
 	 
getSuccessorsOfActivities(LA,LS) :-
	findAll(LSA,(member(A,LA),getSuccessors(A,LSA)),Lsuccs),
	flatten(Lsuccs,LS).
getPredecessorsOfActivities(LA,LS) :-
	findAll(LSA,(member(A,LA),getPredecessors(A,LSA)),Lpred),
	flatten(Lpred,LS).	
	  
getSuccessors(A,LA) :-
     findall(S,waitFor(S,A),LAS),
     findall(S,isGuardedBy(S,A,_V,_),LAG),
     findall(S,weakWait(S,A),LAW),
     append(LAS,LAG,LASG),
     append(LASG,LAW,LA).
     
getPredecessors(A,LASG) :-
	findall(P,waitFor(A,P),LAS),
     findall(P,isGuardedBy(A,P,_V,_),LAG),
 %    findall(S,weakWait(S,A),LAW),
     append(LAS,LAG,LASG). 
	
%Retire les relations inutiles j'en ai une dans to-BPEL4	
normalise(Orchestration).
    
 
 
 avoidUselessWait(P,A) :-
   waitFor(P,A),
   waitFor(A,R),
   waitFor(P,R),
   retract(waitFor(P,R)).  
 avoidUselessWait(_P,_A). 
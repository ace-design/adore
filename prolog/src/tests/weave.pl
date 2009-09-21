%Require dedicatedFunctions


%consult('/Users/mireilleblay-fornarino/Documents/ADORE/adore/prolog/src/tests/weave.pl').
% weave(provider_entry,Set). 
weave(Orchestration,Set) :-
     findall(Actions,
     		 (contextTarget(Composition,Orchestration),
     		  applyFragment(Apply,Composition,Block,Fragment),
     		  prepareWeave(Orchestration,Apply,Block,Fragment,Actions)),
     		  ActionList),
     flatten(ActionList,List),
     list_to_set(List,Set),
     executeActionSet(Set),
          normalise(Orchestration),
     atom_concat(Orchestration,'.png',FileName),
     adore2png(Orchestration,FileName).

     
prepareWeave(Orchestration,Apply,Block,Fragment,Actions) :-
	prepareActivitiesAdding(Apply,Fragment,Orchestration,ActivitiesAdding) ,
 	prepareRelationsAdding(Apply,Fragment,Orchestration,Block,RelationsAdding),
 	append(ActivitiesAdding,RelationsAdding,Actions1),
 	dealWithHookVariables(Apply,Block,Fragment,Actions1,Actions),!.

%%===============================
%% Prepare Activity Adding
%================================
%1)Renvoie les actions qui ajoutent toutes les activités non hook, pred, succ à l'orchestration
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
copyActivity(_Apply,A,_NewOrchestration,[]) :-
	hasForKind(A,predecessors),!.    
copyActivity(_Apply,A,_NewOrchestration,[]) :-
	hasForKind(A,successors),!.    
copyActivity(Apply,A,NewOrchestration,
	[createActivity(NewActivity),
	setActivityKind(NewActivity,K),
	setContainment(NewActivity,NewOrchestration),
	ActionList]) :-
    atom_concat(Apply,A,NewActivity),
    hasForKind(A,K),
    copyActivityAccordingToKind(A,NewActivity,KindActList),
    getInputVariables(A,VarlistIn),
    copyInputVariables(Apply,NewActivity,VarlistIn,AddingInputVar),
    getOutputVariables(A,VarlistOut),
    copyOutputVariables(Apply,NewActivity,VarlistOut,AddingOutputVar),
    append(KindActList,AddingInputVar,KA),
    append(KA,AddingOutputVar,ActionList). 
      
copyActivityAccordingToKind(A,NewActivity,Actions) :-   
   findall(A1, (hasForService(A,S),A1=setInvokedService(NewActivity,S)),LA1),
   findall(A2, (hasForOperation(A,S),A2=setInvokedOperation(NewActivity,S)),LA2),
   findall(A3, (hasForFunction(A,S),A3=setFunction(NewActivity,S)),LA3),
   append(LA1,LA2,LA12),
   append(LA12,LA3,Actions).

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
    copyVariableAccordingToKind(V,NewVar,VarKindActList),
    copyOutputVariables(Apply,A,VarlistIn,AddingInputVarRest),
    append(VarKindActList,AddingInputVarRest,AddingInputVar).
         
copyVariableAccordingToKind(V,NewVar,Actions) :-  
   findall(A1, (hasForInitValue(V,S),A1=setInitValue(NewVar,S)),LA1),
   findall(A2, (isConstant(V),A2=setConstancy(NewVar)),LA2),
   findall(A3, (isSet(V),A3=flagAsSet(NewVar)),LA3),
   append(LA1,LA2,LA12),
   append(LA12,LA3,Actions).
   :- dynamic hasForInitValue/2.  %%  setInitValue(V,S)
:- dynamic isConstant/1.       %%  setConstancy(V).
:- dynamic isSet/1.            %% flagAsSet(V).
%%===============================
%% dealWithHookVariables
%================================
    

%2) Remplace les variables de l'activité hook par leur nom dans l'orchestration.
%Cette action consiste à modifier les actions de la liste
dealWithHookVariables(Apply,Block,Fragment,ActivitiesAdding,NewActivitiesAdding) :-
    getHookActivity(Fragment,Hook),
    getBlockInputVariable([Hook], HookInputVars),
    getBlockInputVariableX(Block, InputVars),
    getBlockOutputVariable([Hook], HookOutputVars),
    getBlockOutputVariableX(Block, OutputVars),	
    dealWithVariableSubstitutions(Apply,HookInputVars,InputVars,ActivitiesAdding,Actions1),
    dealWithVariableSubstitutions(Apply,HookOutputVars,OutputVars,Actions1,NewActivitiesAdding).

dealWithVariableSubstitutions(_Apply,[],_InputVars,ActivitiesAdding,ActivitiesAdding).
dealWithVariableSubstitutions(Apply,[I|HookVars],Vars,ActivitiesAdding,Actions) :-
    	hasForType(I,T),
		member(X,Vars),
		hasForType(X,T), %%On suppose qu'il n'y en a qu'une...
		atom_concat(Apply,I,Name),
		replaceIn(Name,X,ActivitiesAdding,Actions1),
		dealWithVariableSubstitutions(Apply,HookVars,Vars,Actions1,Actions).

replaceIn(_Name,_X,[],[]).
replaceIn(Name,X,[Action|ActivitiesAdding],[NewAction|Actions1]) :- 
	Action =.. [Term,Name,Value],!,
	NewAction =.. [Term,X,Value],
    replaceIn(Name,X,ActivitiesAdding,Actions1).
replaceIn(Name,X,[Action|ActivitiesAdding],[NewAction|Actions1]) :- 
	Action =.. [defGuard,A,T,Name,Value],!,
	NewAction =.. [defGuard,A,T,X,Value],
	replaceIn(Name,X,ActivitiesAdding,Actions1).
%Oubli les create   
 replaceIn(Name,X,[Action|ActivitiesAdding],Actions1) :- 
	Action =.. [_,Name],!,
    replaceIn(Name,X,ActivitiesAdding,Actions1).
replaceIn(Name,X,[A|ActivitiesAdding],[A|Actions1]) :-
    replaceIn(Name,X,ActivitiesAdding,Actions1).
    	


:- dynamic usesAsInput/2.      %%  addAsInput(V,A)
:- dynamic usesAsOutput/2.     %%  addAsOutput(V,A)
:- dynamic fieldAccess/3.      %%  sFieldAccess(I,V,L).
			
%%===============================
%% prepareRelationsAdding
%================================		
%1)determine les 1ere et dernieres activities du bloc
%2)détermine les prédecesseurs et successeurs du bloc
%3)remplace dans les relations 
% - les successeurs 
% - les predecesseurs
%- h<X => h est remplacé par les dernieres
%- X<h =>  h est remplacé par les premieres
%%=== Attention c'est ici que on ajoutera des nop mais on en n'a pas besoin pour Faros
prepareRelationsAdding(Apply,Fragment,_Orchestration,Block,RelationsAdding) :-
 	getFirstActivitiesOfBlockX(Block,FirstActivities) ,
 	getLastActivitiesOfBlockX(Block,LastActivities),
 	getSuccessorsOfActivities(LastActivities,Succ),
 	getPredecessorsOfActivities(FirstActivities,Pred),
 	getActivities(Fragment,LA),
 	workOnRelationsAdding(Apply,LA,FirstActivities,LastActivities,
 		Succ,Pred,RelationsAdding).
 	
workOnRelationsAdding(_Apply,[],_FirstActivities,_LastActivities,_Succ,_Pred,[]). 	 
workOnRelationsAdding(Apply,[A|LA],FirstActivities,LastActivities,Succ,Pred,RelationsAdding) :-
	 findall(Actions,(waitFor(A,P),workOnRelation(Apply,waitFor,A,P,FirstActivities,LastActivities,Succ,Pred,Actions)),WaitActionsList),
 	 findall(Actions,(weakWait(A,P),workOnRelation(Apply,waitWeak,A,P,FirstActivities,LastActivities,Succ,Pred,Actions)),WeakActionsList),
 	 findall(Actions,(isGuardedBy(A,P,Var,Value),atom_concat(Apply,Var,NVar),workOnGuardedRelation(Apply,A,P,NVar,Value,FirstActivities,LastActivities,Succ,Pred,Actions)),GuardedActionsList),
 	 append(WaitActionsList,WeakActionsList,WW),
 	 append(WW,GuardedActionsList,RelationsAdding1),
 	 workOnRelationsAdding(Apply,LA,FirstActivities,LastActivities,Succ,Pred,RelationsAdding2),
 	 append(RelationsAdding1,RelationsAdding2,RelationsAdding3),
 	 flatten(RelationsAdding3,RelationsAdding). 
 	  	 
workOnRelation(_Apply,_Wait,A,P,_FirstActivities,_LastActivities,_Succ,_Pred,[]) :-
    hasForKind(A,hook),
    hasForKind(P,predecessors),!.
 workOnRelation(_Apply,_Wait,P,A,_FirstActivities,_LastActivities,_Succ,_Pred,[]) :-
    hasForKind(A,hook),
    hasForKind(P,successors), !.
 workOnRelation(Apply,Wait,A,P,FirstActivities,_LastActivities,_Succ,Pred,ActionList) :-
    hasForKind(A,hook),!,
    atom_concat(Apply,P,NP),
    prepareRemoveRelations(Wait,FirstActivities,Pred,RemoveActions),
    prepareWaitRelation(Wait,FirstActivities,NP,AddingActions),
    append(RemoveActions,AddingActions,ActionList).
 workOnRelation(Apply,Wait,S,A,_FirstActivities,LastActivities,Succ,_Pred,ActionList) :-
    hasForKind(A,hook),!,
    atom_concat(Apply,S,NS),
    prepareRemoveRelations(Wait,Succ,LastActivities,RemoveActions),
    prepareWaitRelation(Wait,NS,LastActivities,AddingActions),
    append(RemoveActions,AddingActions,ActionList).
 workOnRelation(Apply,Wait,A,P,_FirstActivities,_LastActivities,_Succ,Pred,ActionList) :-
    hasForKind(P,predecessors),!,
    atom_concat(Apply,A,NA),
%    prepareRemoveRelations(Wait,Pred,Pred,RemoveActions),
    prepareWaitRelation(Wait,NA,Pred,ActionList).
 workOnRelation(Apply,Wait,A,P,_FirstActivities,_LastActivities,Succ,_Pred,ActionList) :-
    hasForKind(A,successors),!,
    atom_concat(Apply,P,NP),
%    prepareRemoveRelations(Wait,Pred,Pred,RemoveActions),
    prepareWaitRelation(Wait,Succ,NP,ActionList).
 workOnRelation(Apply,Wait,S,A,_FirstActivities,_LastActivities,_Succ,_Pred,AddingActions) :-
    atom_concat(Apply,A,NA),
    atom_concat(Apply,S,NS),
    prepareWaitRelation(Wait,NS,NA,AddingActions).
prepareWaitRelation(_Wait,[],_A,[]) :-!.
prepareWaitRelation(_Wait,_A,[],[]) :-!.
prepareWaitRelation(Wait,S,A,[T]) :-
	atom(S),atom(A),!,
	(Wait=waitFor, WaitAction=defWaitFor|
	 Wait=waitWeak, WaitAction=defWeakWait),
	T =.. [WaitAction,S,A].

prepareWaitRelation(Wait,[S|L],A,ActionList) :-
	prepareWaitRelation(Wait,S,A,AddingActions),
	( (L==[],!, ActionList=AddingActions) |
		(prepareWaitRelation(Wait,L,A,AddingActionList),
		append(AddingActions,AddingActionList,ActionList))).
prepareWaitRelation(Wait,S,[A|L],ActionList) :-
    atom(S),
	prepareWaitRelation(Wait,S,A,AddingActions),
	prepareWaitRelation(Wait,S,L,AddingActionList),
	append(AddingActions,AddingActionList,ActionList).


workOnGuardedRelation(Apply,A,P,Var,Value,FirstActivities,LastActivities,Succ,Pred,Actions) :-
    hasForKind(A,hook),!,
    workOnGuardedRelation(Apply,FirstActivities,P,Var,Value,FirstActivities,LastActivities,Succ,Pred,Actions).
workOnGuardedRelation(Apply,A,P,Var,Value,FirstActivities,LastActivities,Succ,Pred,Actions) :-
    hasForKind(P,hook),!,
    workOnGuardedRelation(Apply,A,LastActivities,Var,Value,FirstActivities,LastActivities,Succ,Pred,Actions).
workOnGuardedRelation(Apply,A,P,Var,Value,FirstActivities,LastActivities,Succ,Pred,Actions) :-
    hasForKind(A,successors),!,
    workOnGuardedRelation(Apply,Succ,P,Var,Value,FirstActivities,LastActivities,Succ,Pred,Actions).
%A priori les autres cas sur predecessors ou successors ne sont pas possibles
%Si on est sur des atoms c'est que on est sur des activites du fragment sinon on a des listes
workOnGuardedRelation(Apply,A,P,Var,Value,_FirstActivities,_LastActivities,_Succ,_Pred,Actions) :-
    atom(A),atom(P),!,
    atom_concat(Apply,A,NA),
    atom_concat(Apply,P,NP),
    prepareGuardedRelations([NA],[NP],Var,Value,Actions).
workOnGuardedRelation(Apply,A,P,Var,Value,_FirstActivities,_LastActivities,_Succ,_Pred,Actions) :-
    atom(A),!, %P est forcement une liste
    atom_concat(Apply,A,NA),
    prepareGuardedRelations([NA],P,Var,Value,Actions).
workOnGuardedRelation(Apply,A,P,Var,Value,_FirstActivities,_LastActivities,_Succ,_Pred,Actions) :-
    atom(P),!, %A est forcement une liste
    atom_concat(Apply,P,NP),
    prepareGuardedRelations(A,[NP],Var,Value,Actions).
workOnGuardedRelation(_Apply,A,P,Var,Value,_FirstActivities,_LastActivities,_Succ,_Pred,Actions) :-
     %A et P sont  forcement des listes
    prepareGuardedRelations(A,P,Var,Value,Actions).
 
prepareGuardedRelations([],_,_Var,_Value,[]) :-!.
prepareGuardedRelations(_,[],_Var,_Value,[]) :-!.
prepareGuardedRelations([A|L],P,Var,Value,Actions) :-
   findall(Action,
   			  (member(T,P),
   			   Action = defGuard(A,T,Var,Value)),
   		   ActionList),
    prepareGuardedRelations(L,P,Var,Value,ActionsOnRest),
    append(ActionList,ActionsOnRest,Actions).    
            
    

%TO BE DONE	
prepareRemoveRelations(_WAIT,_FirstActivities,_Pred,[]).





%%==================
%% Normalisation
%%===============



%Retire les relations inutiles 
normalise(Orchestration) :-
    getActivities(Orchestration,LA),
    findall((A,P),
    	(member(A,LA),member(P,LA),A \=P, avoidUselessWait(A,P)),
    	_L).
    	

	
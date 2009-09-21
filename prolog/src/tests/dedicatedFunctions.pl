avoidUselessWait(P,A) :-
   waitFor(P,A),
   pathControl(R,P),R\=A,
  pathControl(A,R),
   retract(waitFor(P,A)).  
 avoidUselessWait(_P,_A). 
 
%%==================
%% FUNCTIONS
%%===============
getActivities(P,LA) :-
  	findall(X,isContainedBy(X,P),LA).
 	 
getHookActivity(Fragment,Hook) :-
     hasForKind(Hook,hook),
    isContainedBy(Hook,Fragment),!.

getFirstActivitiesOfBlockX(Block,FirstActivities) :-
    activityBlock(_,Block,Activities),
    getFirstActivitiesOfBlock(Activities,FirstActivities) .
getLastActivitiesOfBlockX(Block,LastActivities) :-
    activityBlock(_,Block,Activities),
    getLastActivitiesOfBlock(Activities,LastActivities) .
    
getBlockInputVariableX(Block, InputVars):-
        activityBlock(_,Block,Activities),
        getBlockInputVariable(Activities, InputVars).
getBlockOutputVariableX(Block, OutputVars):-
        activityBlock(_,Block,Activities),
        getBlockOutputVariable(Activities, OutputVars).
getInputVariables(A,VarList) :-
    findall(V,usesAsInput(A,V),VarList).
getOutputVariables(A,VarList) :-
    findall(V,usesAsOutput(A,V),VarList).	
     	 
getSuccessorsOfActivities(LA,LS) :-
	findall(LSA,(member(A,LA),getSuccessors(A,LSA)),Lsuccs),
	flatten(Lsuccs,LS).
getPredecessorsOfActivities(LA,LS) :-
	findall(LSA,(member(A,LA),getPredecessors(A,LSA)),Lpred),
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
     
     
 pathControl(A, B) :-
		waitFor(B, A).
pathControl(A, B) :-
		isGuardedBy(B, A, _, _).
pathControl(A, B) :-
		weakWait(B, A).
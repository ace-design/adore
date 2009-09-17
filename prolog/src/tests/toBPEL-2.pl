%consult('/Users/mireilleblay-fornarino/Documents/workspace/adoreCode/toBPEL-2.pl').
toBpel(O,seq([R|Blocks])) :-
  process(O),
  preprocessing(O),
  getReceive(O,R),
 %Recupere les descendants immediats (sur un receive on ne peut pas avoir autre chose que des wait4all
  getSuccessors(R,LS),
  getBlocks(LS,Blocks),
  print(Blocks).
  

%%toutes les activités qui sont conditionnées par un test exclusivement (c'est à dire qu'elle ne passe que par une branche)
% ne doivent pas être contraindre d'attendre la fin d'une activité exterieure.
%Pour cela nous "remontons" ces attentes sur le test lui-même.
%Pour l'instant je ne le fais que sur des liens directs... il doit falloir généraliser...
%%ET du coup je ne regarde pas la complementarité qui n'aurait pas de sens
preprocessing(P) :-
    process(P), findall(X,isContainedBy(X,P),LA),
 	movePrecedenceRelationShip(LA).
 	
movePrecedenceRelationShip([]). 	
movePrecedenceRelationShip([A|LA]) :-
    findall(T,isGuardedBy(A,T,_V,_), Tests),
    movePrecedenceRelationShip(A,Tests),
    movePrecedenceRelationShip(LA).

movePrecedenceRelationShip(_A,[]) :-!.
movePrecedenceRelationShip(A,Tests) :-
    findall(P,waitFor(A,P),LP),
    movePrecedenceRelationShip(A,Tests,LP).

movePrecedenceRelationShip(A,Tests,LP) :-
	 retractall( waitFor(A,_P)),
	 findall(P, (member(T,Tests), member(P,LP),asserta(waitFor(T,P))),_).  

getSuccessors(A,LA) :-
     findall(S,waitFor(S,A),LAS),
     findall(S,isGuardedBy(S,A,_V,_),LAG),
     findall(S,weakWait(S,A),LAW),
     append(LAS,LAG,LASG),
     append(LASG,LAW,LA).
				
getStrongSuccessors(A,LA) :-
     findall(S,waitFor(S,A),LAS),
     findall(S,isGuardedBy(S,A,_V,_),LAG),
     append(LAS,LAG,LA).
     
getReceive(O,R) :-
  isContainedBy(R,O),
  hasForKind(R,receive).
  
  
%We assume that it will be included in a good block  
getBlockOnActivity(A,B,ToBefollowedBy) :-
    getDirectSuccessors(A,FreeSucessors,LinkedSuccessors),
    buildSeqBlock(A,FreeSucessors,B,LBS),
    append(LinkedSuccessors,LBS, ToBefollowedBy).
 
buildFlowBlock([],[],[]) :-!.
buildFlowBlock([B],[NB],Succs) :- !,
   getBlockOnActivity(B,NB,Succs).
buildFlowBlock(LB,SB,Succs) :-
   buildSubBlocks(LB,SB,Succs).
 
buildSubBlocks([],[],[]).
buildSubBlocks([A|LB],[flow([SubPart|Flow])],Succs) :-
     getAllActivitiesWithSameSuccessors(A,LB,WithSameSuc,Rest),
     buildSubpart(A,WithSameSuc,SuccSP,SubPart),
     buildSubBlocks(Rest,Flow,SuccR),
     merge_set(SuccSP,SuccR,Succs).
     
buildSubpart(A,[],Successors,B) :-
	getBlockOnActivity(A,B,Successors).       
buildSubpart(A,WithSameSuc,Followings,seq(flow([A|WithSameSuc]),SubPart)) :-
    getSuccessors(A,Successors),
    getBlocksOnActivities(Successors,SubPart,Followings).
   
 buildIfBlock(_A,[],[],[],[]).

 buildIfBlock(A,TrueSuc,FalseSuc,[if(V,NTrueSuc,NFalseSuc)],Followings) :-
     isGuardedBy(_,A,V,_),
     getBlocksOnActivities(TrueSuc,NTrueSuc,TrueSucFollowings),
     getBlocksOnActivities(FalseSuc,NFalseSuc,FalseSucFollowings),
     merge_set(TrueSucFollowings,FalseSucFollowings,Followings).
     
 buildSeqBlock(A,[],A,[]) :- !.  
     
 buildSeqBlock(A,FreeSucessors,seq(A,Flow),Successors) :-
     getConditionnedSuccessors(A,FreeSucessors,TrueSuc,FalseSuc,Others),
     buildIfBlock(A,TrueSuc,FalseSuc,BlockIf,Succs),
     %getBlocksOnActivities(Others,NOthers,Followings),
     buildFlowBlock(Others,OFlow,Followings),
     append(BlockIf,OFlow,Flow),
     merge_set(Succs,Followings,Successors).
 
getBlocksOnActivities([],[],[]).   
getBlocksOnActivities([A|Others],[B|NOthers],Followings) :-
    getBlockOnActivity(A,B,ToBefollowedBy),
    getBlocksOnActivities(Others,NOthers,OtherFollowings),
    append(ToBefollowedBy,OtherFollowings,Followings).
   
getAllActivitiesWithSameSuccessors(_A,[],[],[]) :-!.    
getAllActivitiesWithSameSuccessors(A,LB,WithSameSuc,Rest) :-
    getSuccessors(A,LS),sort(LS,LSS),
    findall(S,(member(S,LB),getSuccessors(S,LSX),sort(LSX,LSS)),WithSameSuc),
    subtract(LB,WithSameSuc,Rest).
    
getConditionnedSuccessors(A,FreeSucessors,TrueSuc,FalseSuc,Others) :-     
     findall(S,(member(S,FreeSucessors),isGuardedBy(S,A,_V,true)),TrueSuc),
     findall(S,(member(S,FreeSucessors),isGuardedBy(S,A,_,false)),FalseSuc),
     subtract(FreeSucessors,FalseSuc,R),
     subtract(R,TrueSuc,Others).
     
getDirectSuccessors(A,ToBeIn,ToBeOut) :-
    getSuccessors(A,LS),
    findall(S,(member(S,LS),(waitFor(S,X)|isGuardedBy(S,X,_,_)),X\=A,\+ exclusiveRelativelyTo(S,A,X)),LinkedSuccessors),
    subtract(LS,LinkedSuccessors,FreeSucessors),
    conditionningEnd(FreeSucessors,NotConditionned,ToBeIn),
    append(LinkedSuccessors,NotConditionned,ToBeOut).

%conditionningEnd(LS,NotConditionning,Conditionning) :-
conditionningEnd([],[],[]). 
conditionningEnd([A|LS],NotConditionning,[A|Conditionning]) :-
   getSuccessors(A,LSS),
   conditionnedSuccessors(LSS,[A|LSS]),!,
   conditionningEnd(LS,NotConditionning,Conditionning). 
conditionningEnd([A|LS],[A|NotConditionning],Conditionning) :-
   conditionningEnd(LS,NotConditionning,Conditionning).    
  
conditionnedSuccessors([X|_LSA],LSS) :-
	%getStrongSuccessors(X,_LSX),
	getPredecessors(X,LPX),
	\+ intersection(LSS,LPX,LPX), !.
conditionnedSuccessors([X|LSA],LSS) :-
	getStrongSuccessors(X,LSX),
	%getPredecessors(X,LPX),
	%intersection(LSS,LPX,LPX),
	append(LSX,LSS,LSXS),
	conditionnedSuccessors(LSA,LSXS).
	   
getPredecessors(A,LASG) :-
	findall(P,waitFor(A,P),LAS),
     findall(P,isGuardedBy(A,P,_V,_),LAG),
 %    findall(S,weakWait(S,A),LAW),
     append(LAS,LAG,LASG).
     
exclusiveRelativelyTo(S,A,B) :-
    weakWait(S,A),
    weakWait(S,B), !.
exclusiveRelativelyTo(S,A,B) :-
    isGuardedBy(S,A,V,T1),
    isGuardedBy(S,B,V,T2), T1 \=T2,!.
exclusiveRelativelyTo(_S,A,B) :-
    exclusive(A,B).
 
 %%Ne suffit pas ... mais si je generalise c'est n'importe quoi...
 %% je dois tester alos la complementarite....
 
 %%Une activité liée par des liens faibles à ces predecesseurs est dupliquée...
%donc ... pour faros.... elle est exclusive avec tout autre activité......
exclusive(A,B) :-
    (duplicated(A) | duplicated(B)), !.
exclusive(A,B) :-
    isGuardedBy(A,Test,V,T1),
    isGuardedBy(B,Test,V,T2), T1 \=T2,!.
exclusive(A,B) :-
    weakWait(A,S),
    weakWait(B,S), !.
    
duplicated(A) :-
    weakWait(A,_S).
        
getBlocks([],[]).
getBlocks(L,[Res]):-
      getBlock4Activity(L,LB,LToBe),
      list_to_set(LToBe,LNA),
      removeLinkedActivities(LNA,LA1),
      getBlocks(LA1,LS),
      buildFlowBlock(LB,NLB,_),
      buildSeqBlock(NLB,LS,Res,_),!.
 
getBlock4Activity([],[],[]).
getBlock4Activity([A|L],[B|LB],LToBe) :-
     getBlockOnActivity(A,B,ToBefollowedBy),
     getBlock4Activity(L,LB,L_LToBe),
     append(ToBefollowedBy,L_LToBe,LToBe).
     
      
removeLinkedActivities(LNA,LA1) :-
  findall(A, 
  	( member(A,LNA),member(B,LNA), (waitFor(A,B) | isGuardedBy(A,B,_,_))),LA),
  subtract( LNA,LA,LA1).     
      
    
    
%getBlocks([A|L],[B|Blocks]) :-
%    getSuccessors(A,Sucessors),
%    compareSucessorsTo(Sucessors,L,NonCommon,Common),
%    buildSequence([A],NonCommon,B),
%Introduction d'attentes non voulues!!!!
%    dealWithCommonSuccessors(B,Common,L,Blocks).
    
%dealWithCommonSuccessors(Block,Common,L,[Seq|Blocks]) :-
%    getPredIn(Common,L,LA,Rest),
%    getBlockWithout(LA,Common,LB),
%    buildSequence([Block|LB],Common,Seq),
%    getBlocks(Rest,Rest,Blocks).
   
   
compareSucessorsTo([],_,[],[]) :- !.
compareSucessorsTo(Succ,[],Succ,[]) :- !.
compareSucessorsTo(Succ,L,NonCommon,Common) :-
    findall(S,(member(A,L), getSuccessors(A,S)),LS),
    append(LS,Successors),
    intersection(Succ,Successors,Common),
    subtract(Succ,Common,NonCommon).
    
buildSequence([A],Succ,seq(A,BlockSucc)) :-
    getBlocks(Succ,BlockSucc).
    
    
    
%?- trace, toBpel(O,B).
%O = provider_entry,
%[ seq(
%[     flow([provider_entry_sourceTimetablesxtimetable4Diploma0, provider_entry_sourceNewsxnewsNow1]), 
%      seq([
%           seq([provider_entry_concat2, provider_entry_truncate3]), 
%           seq([provider_entry_fromProviderinfoSinkxid4, provider_entry_last])])])]


%O = cms_setNewCrisisInformation,
%P = seq([cms_setNewCrisisInformation_a0, 
%       seq(cms_setNewCrisisInformation_a1, 
%        if(cms_setNewCrisisInformation_relevant, [cms_setNewCrisisInformation_a10], [cms_setNewCrisisInformation_r]))])

%O = provider_entrywithTime
%[[seq(provider_entrywithTime_e0, 
% [flow(    [seq(provider_entrywithTime_timestart, [seq(provider_entrywithTime_test, [if(provider_entrywithTime_timeout, [provider_entrywithTime_o], [provider_entrywithTime_last])])
%                                                  ]), 
%           flow([
%              seq( flow([provider_entrywithTime_sourceTimetablesxtimetable4Diploma0, provider_entrywithTime_sourceNewsxnewsNow1]), 
%                   [seq(provider_entrywithTime_concat2, 
%                        [seq(provider_entrywithTime_truncate3, 
%							[seq(provider_entrywithTime_fromProviderinfoSinkxid4, 
%									[seq(provider_entrywithTime_test, 
%										[if(provider_entrywithTime_timeout, [provider_entrywithTime_o], [provider_entrywithTime_last])])])])])])])])])]]


%O = provider_entrywithTimeAndCapacity,
%[seq( [seq( provider_entrywithTimeAndCapacity_e0, 
%            [flow( [seq(provider_entrywithTimeAndCapacity_timestart, 
%                     [seq(provider_entrywithTimeAndCapacity_test, 
%                        [if(provider_entrywithTimeAndCapacity_timeout, [provider_entrywithTimeAndCapacity_o], [])])]), 
%                   flow([seq( flow([provider_entrywithTimeAndCapacity_sourceTimetablesxtimetable4Diploma0, 
%                                    provider_entrywithTimeAndCapacity_sourceNewsxnewsNow1]), 
%                              [seq(provider_entrywithTimeAndCapacity_concat2, 
%                                 [seq(provider_entrywithTimeAndCapacity_truncate3, 
%                                     [seq(provider_entrywithTimeAndCapacity_fromProviderinfoSinkxid4, 
%                                        [flow([seq(provider_entrywithTimeAndCapacity_t, 
%													[seq(provider_entrywithTimeAndCapacity_testc, 
%															[if(provider_entrywithTimeAndCapacity_ok, [], 
%																[provider_entrywithTimeAndCapacity_oc])])]), 
%												flow([seq(provider_entrywithTimeAndCapacity_test, 
%														[if(provider_entrywithTimeAndCapacity_timeout, 
%																[provider_entrywithTimeAndCapacity_o], [])])])])])])])])])])])], 
%		[[provider_entrywithTimeAndCapacity_last]])]

%%ATTENTION IDVAR EST NON INITIALISEE MAIS ON LE SAIT... IL NE DEVRAIT JAMAIS PASSER PAR LA...
%%ON DOIT POUVOIR OTER LE TEST
%O = provider_entrywithTimeAndCapacity,
%[[seq(provider_entrywithTimeAndCapacity_e0, 
%			[flow(	[seq	(provider_entrywithTimeAndCapacity_timestart, 
%								[seq(provider_entrywithTimeAndCapacity_test, 
%									[if(provider_entrywithTimeAndCapacity_timeout, 
%										[provider_entrywithTimeAndCapacity_o], 
%										[provider_entrywithTimeAndCapacity_last])])]), 
%					flow([	seq(
%								flow([provider_entrywithTimeAndCapacity_sourceTimetablesxtimetable4Diploma0, 
%											provider_entrywithTimeAndCapacity_sourceNewsxnewsNow1]), 
%								[seq(provider_entrywithTimeAndCapacity_concat2, 
%										[seq( provider_entrywithTimeAndCapacity_truncate3, 
%												[seq(provider_entrywithTimeAndCapacity_fromProviderinfoSinkxid4, 
%													[flow([	seq(provider_entrywithTimeAndCapacity_t, 
%																[seq(	provider_entrywithTimeAndCapacity_testc, 
%																		[if(provider_entrywithTimeAndCapacity_ok, 
%																			[provider_entrywithTimeAndCapacity_last], 
%																			[provider_entrywithTimeAndCapacity_oc])])]), 
%															flow([seq(provider_entrywithTimeAndCapacity_test, 
%																[if(provider_entrywithTimeAndCapacity_timeout, 
%																	[provider_entrywithTimeAndCapacity_o], 
%																	[provider_entrywithTimeAndCapacity_last])])])])])])])])])])])]]

%Des ordres supplementaires ont été introduits masi on ne peut pas faire autrement
%O = provider_entryV3WithCache,
%[seq([	flow(	[provider_entryV3WithCache_sourceNewsxnewsNow1, 
%				flow([provider_entryV3WithCache_menuxmenuToday3, 
%					flow(	[seq(provider_entryV3WithCache_c,	[seq(provider_entryV3WithCache_test, 
%																	[if(provider_entryV3WithCache_valid, 
%																		[provider_entryV3WithCache_r], 
%																		[seq(	provider_entryV3WithCache_sourceTimetablesxtimetable4Diploma0, 
%																				[provider_entryV3WithCache_e])])])])])])])], 
%		[seq(	[provider_entryV3WithCache_concat2], 
%				[[seq(	provider_entryV3WithCache_concat4, 
%						[seq(	provider_entryV3WithCache_truncate5, 
%								[seq(provider_entryV3WithCache_fromProviderinfoSinkxid6, [provider_entryV3WithCache_last])])])]])])]




%O = provider_entryV3WithAll,
%[seq([	flow(	[provider_entryV3WithAll_c,
%           	flow([	seq(	provider_entryV3WithAll_e0, 
%								[flow(	[seq(	provider_entryV3WithAll_timestart, 
%												[seq(	provider_entryV3WithAll_testTime, 
%														[if(provider_entryV3WithAll_timeout, 
%															[provider_entryV3WithAll_oTime], 
%															[provider_entryV3WithAll_last])])]), 
%										flow(	[provider_entryV3WithAll_sourceNewsxnewsNow1, 
%												flow(	[provider_entryV3WithAll_menuxmenuToday3])])
%										])])])])], 
% 		[seq( [flow(	[seq( 	provider_entryV3WithAll_test, 
%							[if(provider_entryV3WithAll_valid, 
%									[provider_entryV3WithAll_r],
% 									[seq(	provider_entryV3WithAll_sourceTimetablesxtimetable4Diploma0, [provider_entryV3WithAll_e])])]), 
%						flow([provider_entryV3WithAll_concat2])] )], [seq([provider_entryV3WithAll_concat2], [[seq(provider_entryV3WithAll_concat4, [seq(provider_entryV3WithAll_truncate5, [seq(provider_entryV3WithAll_fromProviderinfoSinkxid6, [flow([seq(provider_entryV3WithAll_t, [seq(provider_entryV3WithAll_testcapa, [if(provider_entryV3WithAll_ok, [provider_entryV3WithAll_last], [provider_entryV3WithAll_o])])]), flow([seq(provider_entryV3WithAll_testTime, [if(provider_entryV3WithAll_timeout, [provider_entryV3WithAll_oTime], [provider_entryV3WithAll_last])])])])])])])]])])])]

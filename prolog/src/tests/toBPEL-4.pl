%Require dedicatedFunctions
%consult('/Users/mireilleblay-fornarino/Documents/workspace/adoreCode/toBPEL-4.pl').

%example
%toBpel(O,P),write('\n==============\n'),write(P).
toBpel(O,seq(R,Final)) :-
  process(O),
  preprocessing(O),
  getReceive(O,R),
 %Recupere les descendants immediats (sur un receive on ne peut pas avoir autre chose que des wait4all
  getSuccessors(R,LS),
  getBlocks(LS,Blocks),
  print(Blocks),
  postProcessing(Blocks,Final).
  

%%toutes les activités qui sont conditionnées par un test exclusivement (c'est à dire qu'elle ne passe que par une branche)
% ne doivent pas être contraindre d'attendre la fin d'une activité exterieure.
%Pour cela nous "remontons" ces attentes sur le test lui-même.
%Pour l'instant je ne le fais que sur des liens directs... il doit falloir généraliser...
%%ET du coup je ne regarde pas la complementarité qui n'aurait pas de sens

%%%% Une branche qui contient comme successuer un bloc solitaire ne doit pas ne doit pas avoir de succesur lié.
%% si c'est le cas, on cherche un des parents du lié sur lequel on deplace la relation d'ordre
preprocessingBis(A) :-
    getFatherOfSingles([A],LS),
    check4CorrectSingleFormAndCorrect(LS).

getFatherOfSingles([],[]) .    
getFatherOfSingles([A|LA],LSingle) :-
    getDirectSuccessors(A,LSuc,Linked),
    findall(S, (member(S,LSuc),isSolitaire(S,[A])),LAlones),
    getFatherOfSingles(LA,LSingle1),
    ( ( Linked=[], LAlones=[],!,
        getFatherOfSingles(LSuc,LSingle2),
        append(LSingle1,LSingle2,LSingle)) | 
        LSingle=[A|LSingle1]).
   
check4CorrectSingleFormAndCorrect([]).    
check4CorrectSingleFormAndCorrect([A|LS]) :-
    getDirectSuccessors(A,_FreeSucessors,LinkedSuccessors),
    (LinkedSuccessors=[],!|
     correct4Singles(A,LinkedSuccessors) ),    
    check4CorrectSingleFormAndCorrect(LS).
%Je ne fais qu'un cran.. mais il faudrait le faire mieux....
correct4Singles(A,[S|LinkedSuccessors]) :-
    getPredecessors(S,LP),
    retract(waitFor(S,A)),
    subtract(LP,[A],RLP),
    findall(P, (member(P,RLP), asserta(waitFor(P,A))),_),
    avoidUselessWait(P,A),
	correct4Singles(A,LinkedSuccessors) .
correct4Singles(_A,[]).	
	 
%avoidUselessWait(P,A) :-
%   waitFor(P,A),
%   waitFor(A,R),
%   waitFor(P,R),
%   retract(waitFor(P,R)).  
% avoidUselessWait(_P,_A). 
    
preprocessing(P) :-
    process(P), findall(X,isContainedBy(X,P),LA),
 	movePrecedenceRelationShip(LA),
 	getReceive(P,R),
 	preprocessingBis(R).
 	
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

%getSuccessors(A,LA) :-
%     findall(S,waitFor(S,A),LAS),
%     findall(S,isGuardedBy(S,A,_V,_),LAG),
%     findall(S,weakWait(S,A),LAW),
%     append(LAS,LAG,LASG),
%     append(LASG,LAW,LA).
				
getStrongSuccessors(A,LAS) :-
     findall(S,waitFor(S,A),LAS).
 %Pour ne pas separer les conditions...
 %    findall(S,isGuardedBy(S,A,_V,_),LAG),
 %    append(LAS,LAG,LA).
     
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
buildFlowBlock(LB,[flow(SB)],Succs) :-
   buildSubBlocks(LB,SB,Succs).
 
buildSubBlocks([],[],[]).
buildSubBlocks([A1|LB1],SubPartList,Succs) :- !,
     extraireBlocsSolitaires([A1|LB1],BS,RL),
     ( ( RL =[A|LB], 
       buildSubBlocksBasedOn(A,LB,SubPartListA,Succs),
      append(BS,SubPartListA,SubPartList)) |
     ( SubPartList = BS, Succs = [])).
buildSubBlocks(X,[X],[]).     
     
buildSubBlocksBasedOn(A,LB,SubPartList,Succs) :-     
     getAllActivitiesWithSameSuccessors(A,LB,WithSameSuc,Rest),
     buildSubpart(A,WithSameSuc,SuccSP,SubPart),        
     ( (Rest=[],SubPartList=[SubPart],Succs=SuccSP) |
       ( buildSubBlocks(Rest,Flow,SuccR),
         intersection(SuccR,SuccSP,LCommons),
         list_to_set(LCommons,LSuccessors),
         dealWithFollowingActivites(LSuccessors,LS),
         buildSeqBlock(flow([SubPart|Flow]),LS,SubPartList,Successors),
         merge_set(SuccSP,SuccR,LToBeLarge1),
     	 append(Successors,LToBeLarge1,LToBeLarge),
     	 subtract(LToBeLarge,LCommons,Succs))).
     
buildSubpart(A,[],Successors,B) :-
	getBlockOnActivity(A,B,Successors).       
buildSubpart(A,WithSameSuc,Followings,Res) :-
    getSuccessors(A,Successors),
    getBlock4Activity(Successors,SubPart,Followings),
%    getBlocksOnActivities(Successors,SubPart,Followings),
    dealWithResSeqBlock(flow([A|WithSameSuc]), SubPart, Res).
   
 buildIfBlock(_A,[],[],[],[]).

 buildIfBlock(A,TrueSuc,FalseSuc,[if(V,NTrueSuc,NFalseSuc)],Followings) :-
     isGuardedBy(_,A,V,_),
     getBlocksOnActivities(TrueSuc,NTrueSuc,TrueSucFollowings),
     getBlocksOnActivities(FalseSuc,NFalseSuc,FalseSucFollowings),
     merge_set(TrueSucFollowings,FalseSucFollowings,Followings).
     
 buildSeqBlock(A,[],A,[]) :- !.   
 buildSeqBlock(A,FreeSucessors,Res,Successors) :-
     getConditionnedSuccessors(A,FreeSucessors,TrueSuc,FalseSuc,Others),
     buildIfBlock(A,TrueSuc,FalseSuc,BlockIf,Succs),
     %getBlocksOnActivities(Others,NOthers,Followings),
     buildFlowBlock(Others,OFlow,Followings),
     append(BlockIf,OFlow,Flow),
     merge_set(Succs,Followings,Successors),
     dealWithResSeqBlock(A,Flow,Res).
dealWithResSeqBlock(A,[],A) :-!.
dealWithResSeqBlock(A,Flow,seq(A,Flow)).
     
 
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
    getDirectSuccessorsAccordingToWeakLink(A,LS,ToBeIn,ToBeOut).

 getDirectSuccessorsAccordingToWeakLink(A,LS,LS,[]) :-   
     weakWait(A,_),!.
 getDirectSuccessorsAccordingToWeakLink(A,LS,LS,[]) :-   
     member(S,LS),weakWait(S,A),!.
 getDirectSuccessorsAccordingToWeakLink(A,LS,ToBeIn,ToBeOut) :-   
    findall(S,(member(S,LS),(waitFor(S,X)|isGuardedBy(S,X,_,_)),X\=A,\+ exclusiveRelativelyTo(S,A,X)),LinkedSuccessors),
    subtract(LS,LinkedSuccessors,FreeSucessors),
    ToBeIn = FreeSucessors, ToBeOut=LinkedSuccessors.
 %   conditionningEnd(FreeSucessors,NotConditionned,ToBeIn),
 %   append(LinkedSuccessors,NotConditionned,ToBeOut).

%conditionningEnd(LS,NotConditionning,Conditionning) :-
conditionningEnd([],[],[]). 
conditionningEnd([A|LS],NotConditionning,[A|Conditionning]) :-
   onlyConditionnedSuccessors(A,_LSS),!,
   conditionningEnd(LS,NotConditionning,Conditionning). 
conditionningEnd([A|LS],[A|NotConditionning],Conditionning) :-
   conditionningEnd(LS,NotConditionning,Conditionning). 
conditionningEnd([A|LS],NotConditionning,[A|Conditionning]) :-
   getStrongSuccessors(A,LSS),
   conditionnedSuccessors(LSS,[A|LSS]),!,
   conditionningEnd(LS,NotConditionning,Conditionning). 
conditionningEnd([A|LS],[A|NotConditionning],Conditionning) :-
   conditionningEnd(LS,NotConditionning,Conditionning).    
  
onlyConditionnedSuccessors(A,LSS) :-
    findall(X,isGuardedBy(A,X,_,_),LSS),
    LSS \= [],
    findall(X,waitFor(A,X),Lw),
    Lw == [].
    
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
	   
%getPredecessors(A,LASG) :-
%	findall(P,waitFor(A,P),LAS),
%     findall(P,isGuardedBy(A,P,_V,_),LAG),
% %ATTENTION    findall(S,weakWait(S,A),LAW),
%     append(LAS,LAG,LASG).
     
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
        
%Construit les blocs correspondants aux activités
%Les activités forment alors un flow

%Principe : 
%1.Extraire les blocs isolés
%2.Lancer la machine sur le reste :
%    On obtient une liste de sous blocs LB et leurs successeurs  LToBe que l'on considere comme partagé
%  
%3.Faire un Bloc de ces 
%% getBlocks(+ActivityList,-ActivityList).        
getBlocks([],[]).
getBlocks(L,[flow([Res|BS])]):-
      extraireBlocsSolitaires(L,BS,RL),
      getBlock4Activity(RL,LB,LToBe),
      buildFlowBlock(LB,[NLB],Succ),
      removeActivityInBlock(NLB,LToBe,LToBeRest),
      merge_set(Succ,LToBeRest,LA),
      list_to_set(LA,LNA),
      dealWithFollowingActivites(LNA,LS),
      buildSeqBlock(NLB,LS,Res,_),!.
 
 extraireBlocsSolitaires([],[],[]).
 extraireBlocsSolitaires([A|L],BS,RL) :-
  getPredecessors(A,LPA),   
  solitaire(A,BS1,LPA),!,
  extraireBlocsSolitaires(L,BS2,RL),
  append(BS1,BS2,BS).
 extraireBlocsSolitaires([A|L],BS,[A|RL]) :-
  extraireBlocsSolitaires(L,BS,RL).
 
solitaire(A,[BA],LPA) :-
    isSolitaire(A,LPA),
    getBlockOnActivity(A,BA,_ToBefollowedBy).
    
isSolitaire(A,[]) :-    
    weakWait(A,_),!.
isSolitaire(A,L) :-
    getPredecessors(A,LPA),
    intersection(LPA,L,LPA),
    getSuccessors(A,LSX),!,
    areSolitaires(LSX,[A|L]).
areSolitaires([A|LR],L) :-
	getPredecessors(A,LPA),
    intersection(LPA,L,LPA),
    getStrongSuccessors(A,LSX),
    areSolitaires(LR,L),
    append(L,LR,NL),
    areSolitaires(LSX,[A|NL]).
areSolitaires([],_).
         
removeActivityInBlock(flow([A|L]),LA,LToBeRest) :-
    subtract(LA,[A|L],LToBeRest1),
	removeActivityInBlock(A,LToBeRest1,LToBeRest2),
	removeActivityInBlock(L,LToBeRest2,LToBeRest).
removeActivityInBlock(seq(A,L),LA,LToBeRest) :-
    removeActivityInBlock(A,LA,LToBeRest1),
	removeActivityInBlock(L,LToBeRest1,LToBeRest).
removeActivityInBlock([A|L],LA,LToBeRest) :-
	removeActivityInBlock(A,LA,LA1),
	removeActivityInBlock(L,LA1,LToBeRest).
removeActivityInBlock(A,LToBe,LToBeRest) :-
    subtract(LToBe,[A],LToBeRest).
    
dealWithFollowingActivites([],[]) :-!.   
dealWithFollowingActivites(LNA,LS) :-    
      removeLinkedActivities(LNA,LA1),
      getBlocks(LA1,LS).
     
%Lancer la construction de blocs sur une liste d'activités
%getBlock4Activity(+ActivityList, -BlockList, -ActivityList) 
%    On obtient une liste de sous blocs et leurs successeurs  LToBe que l'on considere comme partagé 
getBlock4Activity([],[],[]).
getBlock4Activity([A|L],ResBlockList,LToBe) :-
     getBlockOnActivity(A,B,ToBefollowedBy),
     ( (L=[],ResBlockList=[B],LToBe=ToBefollowedBy) |
       ( getBlock4Activity(L,LB,L_LToBe),
         intersection(ToBefollowedBy,L_LToBe,LCommons),
         list_to_set(LCommons,LSuccessors),
         dealWithFollowingActivites(LSuccessors,LS),
         buildSeqBlock(flow([B|LB]),LS,ResBlockList,Successors),
     	 append(ToBefollowedBy,L_LToBe,LToBeLarge1),
     	 append(Successors,LToBeLarge1,LToBeLarge),
     	 subtract(LToBeLarge,LCommons,NLSucc),
     	 removeActivityInBlock([B|LB],NLSucc,LToBe) ) ).
     
      
removeLinkedActivities(LNA,LA1) :-
  findall(A, 
  	( member(A,LNA),member(B,LNA),B\=A, pathRec(B,A,_) ) ,LA),
  subtract( LNA,LA,LA1).     
     
pathRec(A1,A2,[A1,A2]) :-
	      pathControl(A1,A2).
pathRec(A1,A3,[A1|P]) :-
	      pathControl(A1,A2),
	      pathRec(A2,A3,P).   
	
%pathControl(A, B) :-
%		waitFor(B, A).
%pathControl(A, B) :-
%		isGuardedBy(B, A, _, _).
%pathControl(A, B) :-
%		weakWait(B, A).
		    
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
    



postProcessing([],[]) :-!.
postProcessing(flow(Flow),flow(NewFlow) ) :- !,
    flatten(Flow,NList),
 	findall(X, member(flow(X),NList), LX),
 	findall(RX,(member(X,LX), postProcessing(X,RX)),LRX),
 	findall(flow(X), member(flow(X),NList), ToRetract),
 	subtract(NList,ToRetract,FlowRest),
 	postProcessing(FlowRest,NewRestFlow),
 	append(LRX,SimpleList),
    append(SimpleList,NewRestFlow,NewFlow1),
    ( (LX = [], NewFlow=NewFlow1) | (LX \=[], postProcessing(flow(NewFlow1),flow(NewFlow)))).  
postProcessing(seq(A,Seq),seq(NewSeq)) :- !,
    flatten([A|Seq],NList),
 	findall((X,Y), member(seq(X,Y),NList), LX),
 	( 	(LX =[], postProcessing(NList,NewSeq)) |
 		(LX \=[], flatten([A|Seq],NList), postProcessingSeq(NList,NewSeq))). 
postProcessing(if(V,LATrue,LAFalse),if(V,NLATrue,NLAFalse) ) :- !,
 	postProcessing(LATrue,NLATrue),
 	postProcessing(LAFalse,NLAFalse).
    
postProcessing([A|LA],[NX|NLX]) :- !,
	flatten([A|LA],[X|LX]), 
    postProcessing(X,NX),
    postProcessing(LX,NLX).
postProcessing(X,X).


postProcessingSeq([],[]) :- !.
postProcessingSeq([seq(X,Y)|L],NewList) :- !,
    postProcessing([X|Y],RX),
    postProcessingSeq(RX,NRX),
    postProcessingSeq(L,NewList1),
    append(NRX,NewList1,NewList).
postProcessingSeq([seq(X)|L],NewList) :- !,
    postProcessing(X,RX),
    postProcessingSeq(RX,NRX),
    postProcessingSeq(L,NewList1),
    append(NRX,NewList1,NewList).
postProcessingSeq([X|L],[RX|NewList]) :- !,
    postProcessing(X,RX),
    postProcessingSeq(L,NewList).

%O = provider_entrywithTime
%seq(provider_entrywithTime_e0,
%    [ flow([ seq(provider_entrywithTime_timestart,
%		 [ seq(provider_entrywithTime_test,
%		       [ if(provider_entrywithTime_timeout,
%			    [provider_entrywithTime_o],
%			    [provider_entrywithTime_last])
%		       ])
%		 ]),
%	     seq(flow([ provider_entrywithTime_sourceTimetablesxtimetable4Diploma0,
%			provider_entrywithTime_sourceNewsxnewsNow1
%		      ]),
%		 [ seq(provider_entrywithTime_concat2,
%		       [ seq(provider_entrywithTime_truncate3,
%			     [ seq(provider_entrywithTime_fromProviderinfoSinkxid4,
%				   [ seq(provider_entrywithTime_test,
%					 [ if(provider_entrywithTime_timeout,
%					      [ provider_entrywithTime_o
%					      ],
%					      [ provider_entrywithTime_last
%					      ])
%					 ])
%				   ])
%			     ])
%		       ])
%		 ])
%	   ])
 %   ]




%provider_entrywithCapacity
%[ flow([ flow([ seq(	flow([ provider_entrywithCapacity_sourceTimetablesxtimetable4Diploma0,
%			   				provider_entrywithCapacity_sourceNewsxnewsNow1
%			 				]),
%		    		[ seq(provider_entrywithCapacity_concat2,
%			  			[ seq(provider_entrywithCapacity_truncate3,
%							[ seq(provider_entrywithCapacity_fromProviderinfoSinkxid4,
%				      			[ seq(provider_entrywithCapacity_t,
%					    			[ seq(provider_entrywithCapacity_test,
%						  				[ if(provider_entrywithCapacity_ok,
%						       [ provider_entrywithCapacity_last
%						       ],
%						       [ provider_entrywithCapacity_o
%						  ])
%					    ])
%				      ])
%				])
%			  ])
%		    ])
%	      ])
 %      ])
%]

%O = provider_entrywithTimeAndCapacity,
%[ flow([ seq(provider_entrywithTimeAndCapacity_e0,
%	     		[ flow(	[ seq(provider_entrywithTimeAndCapacity_timestart,
%			  				[ seq(provider_entrywithTimeAndCapacity_test,
%								[ if(provider_entrywithTimeAndCapacity_timeout,
%				     				[ provider_entrywithTimeAndCapacity_o
%				     				],
%				     				[ provider_entrywithTimeAndCapacity_last
%				     				])
%								])
%			  				]),
%		      			seq(	flow([ provider_entrywithTimeAndCapacity_sourceTimetablesxtimetable4Diploma0,
%				 provider_entrywithTimeAndCapacity_sourceNewsxnewsNow1
%			       ]),
%			  [ seq(provider_entrywithTimeAndCapacity_concat2,
%				[ seq(provider_entrywithTimeAndCapacity_truncate3,
%				      [ seq(provider_entrywithTimeAndCapacity_fromProviderinfoSinkxid4,
%					    [ flow([ seq(provider_entrywithTimeAndCapacity_t,
%							 [ seq(provider_entrywithTimeAndCapacity_testc,
%							       [ if(provider_entrywithTimeAndCapacity_ok,
%								    [ provider_entrywithTimeAndCapacity_last
%								    ],
%								    [ provider_entrywithTimeAndCapacity_oc
%								    ])
%							       ])
%							 ]),
%						     seq(provider_entrywithTimeAndCapacity_test,
%							 [ if(provider_entrywithTimeAndCapacity_timeout,
%							      [ provider_entrywithTimeAndCapacity_o
%							      ],
%							      [ provider_entrywithTimeAndCapacity_last
%							      ])
%							 ])
%						   ])
%					    ])
%				      ])
%				])
%			  ])
%		    ])
%	     ])
%      ])
%]

%Des ordres supplementaires ont été introduits masi on ne peut pas faire autrement
%O = provider_entryV3WithCache,
%[ flow([ seq
%			(flow([ provider_entryV3WithCache_sourceNewsxnewsNow1,
%		    		provider_entryV3WithCache_menuxmenuToday3
%		  		]),
%	     	[ flow([ seq(provider_entryV3WithCache_concat2,
%			  			[ flow([ flow([]),
%				   		seq(provider_entryV3WithCache_concat4,
%				       	[ seq(provider_entryV3WithCache_truncate5,
%					     [ seq(provider_entryV3WithCache_fromProviderinfoSinkxid6,
%						   [ provider_entryV3WithCache_last
%						   ])
%					     ])
%				       ])
%				 ])
%			  ])
%		    ])
%	     ]),
%	 seq(provider_entryV3WithCache_c,
%	     [ seq(provider_entryV3WithCache_test,
%		   [ if(provider_entryV3WithCache_valid,
%			[provider_entryV3WithCache_r],
%			[ seq(provider_entryV3WithCache_sourceTimetablesxtimetable4Diploma0,
%			      [provider_entryV3WithCache_e])
%			])
%		   ])
%	     ])
%      ])



%O = provider_entryV3WithAll,
%[[ flow([ seq(flow([ provider_entryV3WithAll_c,
%		    seq(provider_entryV3WithAll_e0,
%			[ flow([ seq(provider_entryV3WithAll_timestart,
%				     [ seq(provider_entryV3WithAll_testTime,
%					   [ if(provider_entryV3WithAll_timeout,
%						[ provider_entryV3WithAll_oTime
%						],
%						[ provider_entryV3WithAll_last
%						])
%%				     ]),
%			 provider_entryV3WithAll_sourceNewsxnewsNow1,
%				 provider_entryV3WithAll_menuxmenuToday3
%			       ])
%			])
%		  ]),
%	     [ flow([ seq(seq(provider_entryV3WithAll_test,
%			      [ if(provider_entryV3WithAll_valid,
%				   [provider_entryV3WithAll_r],
%				   [ seq(provider_entryV3WithAll_sourceTimetablesxtimetable4Diploma0,
%					 [provider_entryV3WithAll_e])
%				   ])
%			      ]),
%			  [ flow([ seq(provider_entryV3WithAll_concat2,
%				       [ flow([ flow([]),
%						seq(provider_entryV3WithAll_concat4,
%						    [ seq(provider_entryV3WithAll_truncate5,
%							  [ seq(provider_entryV3WithAll_fromProviderinfoSinkxid6,
%								[ flow([ seq(provider_entryV3WithAll_t,
%									     [ seq(provider_entryV3WithAll_testcapa,
%%										   [ if(provider_entryV3WithAll_ok,
%											[ provider_entryV3WithAll_last
%											],
%											[ provider_entryV3WithAll_o
%											])
%										   ])
%									     ]),
%									 seq(provider_entryV3WithAll_testTime,
%									     [ if(provider_entryV3WithAll_timeout,
%										  [ provider_entryV3WithAll_oTime
%										  ],
%										  [ provider_entryV3WithAll_last
%										  ])
%									     ])
%								       ])
%								])
%							  ])
%						    ])
%					      ])
%				       ])
%				 ])
%			  ])
%		    ])
%	     ])
%      ])
%]
%
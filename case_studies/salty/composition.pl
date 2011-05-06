doSeqComposition :-
   doInstantiate(getAverageValue,control_controlSubmissionRateV1, v1,  [ ]),
   doWeave([weave(v1,[control_controlSubmissionRateV1_l1 ])]),
   doProcessSimplification(control_controlSubmissionRateV1),
   cccms_draw(control_controlSubmissionRateV1,'average0'),
   
   doInstantiate(setPeriodic,control_controlSubmissionRateV1, v11,  [ ]),
   doWeave([weave(v11,[control_controlSubmissionRateV1_l1 ])]),
   doProcessSimplification(control_controlSubmissionRateV1),
   cccms_draw(control_controlSubmissionRateV1,'periodic'),

   doInstantiate(getAverageValue,control_controlSubmissionRateV1, v2,  [ ]),
   doWeave([weave(v2,[control_controlSubmissionRateV1_l5 ])]),
   doProcessSimplification(control_controlSubmissionRateV1),
   cccms_draw(control_controlSubmissionRateV1,'average1'),

   doInstantiate(getAverageValue,control_controlTriggerRate, v3,  [ ]),
   doWeave([weave(v3,[control_controlTriggerRate_l1 ])]),
   doProcessSimplification(control_controlTriggerRate),
   cccms_draw(control_controlTriggerRate,'rate0'),
   
   doInstantiate(getAverageValue,control_maxQueueSize, v4,  [ ]),
   doWeave([weave(v4,[control_maxQueueSize_l1 ])]),
   doProcessSimplification(control_maxQueueSize),
   cccms_draw(control_maxQueueSize,'size0'),
   doInstantiate(setPeriodic,control_maxQueueSize, v41,  [ ]),
   doWeave([weave(v41,[control_maxQueueSize_l1 ])]),
   doProcessSimplification(control_maxQueueSize),
   cccms_draw(control_maxQueueSize,'periodicSize').

shouldDraw(true).    %% Draw resulting process as PNG file
% shouldDraw(false). %% Do not draw resulting process as PNG file


shouldDraw(true).    %% Draw resulting process as PNG file
% shouldDraw(false). %% Do not draw resulting process as PNG file

cccms_draw(_,_) :- shouldDraw(false),!.
cccms_draw(Process,PrettyName) :- 
	swritef(RealFile,'output/%w.png',[PrettyName]),
        adore2png(Process,RealFile).

% %consult('composition.pl').

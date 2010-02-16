
shouldDraw(true).    %% Draw resulting process as PNG file
%%shouldDraw(false). %% Do not draw resulting process as PNG file


%%% Variable Equivalences
adoreEquivalence(variable,[[replaceMission, mi], [askedMissionFailure, mi]]).

%%% Dataflow semantic relations
dataLink(unavailableExtResource,res,a2).

%%%%
%% Prolog Predicate to 'decorate' the demonstration
%%%%

cccms_draw(_,_) :- shouldDraw(false),!.
cccms_draw(Process,PrettyName) :- 
	swritef(RealFile,'output/%w.png',[PrettyName]),
        adore2png(Process,RealFile).
    
	
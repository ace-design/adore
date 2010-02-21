%%%%
%% Knowledge added by the designer to enrich CCCMS models
%%%%

%%% Variable Equivalence (business-driven)
adoreEquivalence(variable,[[replaceMission, mi], [askedMissionFailure, mi]]).

%%% Variable Equivalences (non functional) 
adoreEquivalence(variable,[[logUpdate, v], [logTime, key]]).
adoreEquivalence(variable,[[authentifyWhenIdle, u], [logUpdate, v]]).
adoreEquivalence(variable,[[authentifyWhenIdle, u], [logUpdate, v]]).
adoreEquivalence(variable,[[unavailableIntResource, worker], [logCreate, v]]).


%%% Weaving association
variableBinding(cms_handleAMission, [[cms_handleAMission_iw, logUpdate_v], 
	                             [cms_handleAMission_coord, logUpdate_v]]).


%%% Dataflow semantic relations
dataLink(cms_handleACrisis,choosen,a378). %% choosen -> choosen* with NF
dataLink(unavailableExtResource,res,a2).  %% er -> er* 
dataLink(cms_handleAMission,er,a414).     %% er -> er* with NF
dataLink(cms_handleAMission,ir,a402).     %% ir -> ir* with NF
dataLink(cms_handleAMission,ir,a403).     %% ir -> ir* with NF
dataLink(cms_handleAMission,ir,a404).     %% ir -> ir* with NF
dataLink(cms_handleAMission,ir,a405).     %% ir -> ir* with NF
dataLink(cms_handleAMission,ir,a406).     %% ir -> ir* with NF

%%%%
%% Prolog Predicate to 'decorate' the demonstration (optional)
%%%%

shouldDraw(true).    %% Draw resulting process as PNG file
% shouldDraw(false). %% Do not draw resulting process as PNG file

cccms_draw(_,_) :- shouldDraw(false),!.
cccms_draw(Process,PrettyName) :- 
	swritef(RealFile,'output/%w.png',[PrettyName]),
        adore2png(Process,RealFile).
    
	
%%%%
%% This file is part of ADORE [ www.adore-design.org ]
%%
%% Copyright (C) 2008-  Sebastien Mosser
%%
%% ADORE is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% ADORE is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with jSeduite:DataCache; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author      Main Sébastien Mosser          [mosser@polytech.unice.fr]
%%%%
:- module(instantiate, [doInstantiate/4]).

%%%%%%
%%% end user interface
%%%%%%

doInstantiate(Orig,Target,Output,Params) :- 
	dinfo(algo,'Running doInstantiate(~w,~w,~w)',[Orig,Target,Output]),
	dinfo(algo,'  Computing action set',[]),
	myTimer(instantiate:buildActions(Orig,Target,Output,Params,Directives)),
	length(Directives,LActions), 
	dinfo(algo,'  => Result: ~w actions',[LActions]),
	dinfo(algo,'  Executing action set',[]),
	myTimer(executeActionSet(Directives)),!,
	dinfo(algo,'doInstantiate(~w,~w,~w) ended with success!',
	      [Orig,Target,Output]).

buildActions(Orig,Target,Output,Params,Dirs) :- 
 	process:exists(Orig), process:exists(Target),\+ process:exists(Output),
	declareContext(instantiate(Orig,Target,Output),Ctx),
	clone:cloneProcess(Ctx,Orig,Output,CloneActions),
        performInstantiations(Ctx,Params,InstActions),
	flatten([CloneActions,InstActions],Dirs).


performInstantiations(_,[],[]).
performInstantiations(CtxId,[bind(Param,Value)|Tail],[Actions|Others]) :- 
	adoreContext(CtxId,instantiate(Orig,P,_)),
 	dinfo(algo,'  bind(~w,~w)', [Param, Value]),
 	identifyCandidate(Param,Orig,Candidate),
	dinfo(instantiate,'   Candidate: ~w',[Candidate]),
 	identifyTarget(Value,P,Target),
	dinfo(instantiate,'   Target: ~w',[Target]),
	computeMatch(CtxId,Candidate,Target,Actions),
	dinfo(instantiate,'   MatchActions: ~w',[Actions]),
 	performInstantiations(CtxId,Tail,Others).


identifyCandidate(Param,Fragment,variable(VarName)) :- %% Variable
	pebble(rename(variable),Param,VarName,compile(Fragment)),!.
identifyCandidate(Param,Fragment,constant(VarName)) :- %% Constant
	pebble(rename(constant),Param,VarName,compile(Fragment)),!.
identifyCandidate(P,F,_) :- 
	dfail(algo,'Unable to match \'~w\' param in fragment \'~w\'',[P,F]).


identifyTarget(Value,Process,variable(VarName)) :- %% Variable	
	pebble(rename(variable),Value,VarName,compile(Process)),!.
identifyTarget(Value,_,constant(Value)).



computeMatch(Ctx,variable(ParamVar),variable(ProcessVar), Actions) :- 
	%% Var --> Var
	getImmediateDerivation(Ctx,ParamVar,ClonedParamVar),
	Actions = [substVariable(ClonedParamVar,ProcessVar)],!.
computeMatch(Ctx,variable(ParamVar),constant(Value), Actions) :- 
	%% Var --> Constant
	getImmediateDerivation(Ctx,ParamVar,ClonedParamVar),
	Actions = [ setConstancy(ClonedParamVar),
	            setInitValue(ClonedParamVar,Value) ],!.
computeMatch(Ctx,constant(ParamVar),constant(Value), Actions) :- 
	%% Constant --> Constant 
	getImmediateDerivation(Ctx,ParamVar,ClonedParamVar),
	Actions = [ setInitValue(ClonedParamVar,Value) ],!.
computeMatch(_,FragCandidate, ProcessCandidate, _) :- 
	dfail(algo,'Unable to appariate \'~w\' -> \'~w\'',[FragCandidate,ProcessCandidate]).




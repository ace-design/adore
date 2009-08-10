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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                              %%
%% ADORE metamodel represented as logical facts %%
%%                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%% Vocabulary
%%%%

%% P: a valid process identidier
%% V: a valid variable identifier
%% T: a valid variable type name
%% A: a valid activity identifier
%% K: a valid activity kind
%% I: an a priori unknown identifier 
%% S: a "string" literal
%% L: A prolog list
%% 'valid' means syntactic & existing when needed 

%%%%
%%  Business Processes (Orchestration / Fragment)
%%%%

:- dynamic process/1.          %%  createProcess(P)
:- dynamic isFragment/1.       %%  setAsFragment(P)
:- dynamic hasForParameter/2.  %%  setAsFragmentParameter(P,I)

:- dynamic hasForSrvName/2.    %%  setService(P,I)
:- dynamic hasForOpName/2.     %%  setOperation(P,I) 

%%%%
%% Activities
%%%%

:- dynamic activity/1.         %%  createActivity(A)
:- dynamic hasForKind/2.       %%  setActivityKind(A,K)
:- dynamic isContainedBy/2.    %%  setContainment(A,P)

:- dynamic hasForService/2.    %%  setInvokedService(A,I)
:- dynamic hasForOperation/2.  %%  setInvokedOperation(A,I)
:- dynamic usesAsBinding/3.    %%  setMessageBinding(A,I,V)
:- dynamic hasForFunction/2.   %%  setFunction(A,I)  


%%%%
%% Variables 
%%%%

:- dynamic variable/1.         %%  createVariable(V)
:- dynamic hasForType/2.       %%  setVariableType(V,T)
:- dynamic hasForInitValue/2.  %%  setInitValue(V,S)
:- dynamic isConstant/1.       %%  setConstancy(V).
:- dynamic isSet/1.            %% flagAsSet(V).

:- dynamic usesAsInput/2.      %%  addAsInput(V,A)
:- dynamic usesAsOutput/2.     %%  addAsOutput(V,A)
:- dynamic fieldAccess/3.      %%  sFieldAccess(I,V,L).


%%%%
%% Relations
%%%%

:- dynamic waitFor/2.          %%  defWaitFor(A,A)
:- dynamic weakWait/2.         %%  defWeakWait(A,A)
:- dynamic isGuardedBy/4.      %%  defGuard(A,A,V,true|false)
:- dynamic onFailure/3.        %%  defOnFail(A,A,S|'*')

%%%%
%% Composition directive
%%%%

:- dynamic context/1.          %% defCompositionContext(I)
:- dynamic contextTarget/2.    %% setCompositionTarget(I,P)
:- dynamic contextOutput/2.    %% setContextOutput(I,I)
:- dynamic activityBlock/3.    %% defActivityBlock(I,L)
:- dynamic applyFragment/4.    %% defApply(I,P,I,P)
:- dynamic applyParameter/3.   %% setApplyParam(I,I,S)
:- dynamic setify/2.           %% defSetify(I,V)


%%%%
%% Policies
%%%%

:- dynamic policy/2.           %% defPolicy(I,Formula)
:- dynamic iteratesOver/2.     %% setIteration(A,Policy)

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
%% @author      Main S�bastien Mosser          [mosser@polytech.unice.fr]
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
%% 'valid' means syntactic & existing when needed 

%%%%
%%  Business Processes (Orchestration / Fragment)
%%%%

:- dynamic process/1.          %%  createProcess(P)
:- dynamic isFragment/1.       %%  setAsFragment(P)

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

:- dynamic hasForInput/2.      %%  addAsInput(V,A)
:- dynamic hasForOutput/2.     %%  addAsOutput(V,A)


%%%%
%% Relations
%%%%

:- dynamic waitFor/2.          %%  defWaitFor(A,A)
:- dynamic weakWait/2.         %%  defWeakWait(A,A)
:- dynamic isGuardedBy/4.      %%  defGuard(A,A,V,true|false)

%%%%
%% Actions over the metamodel (aka merge)
%%%%

:- dynamic mergeOrder/4.       %% defMergeOrder(I,I,A,P)


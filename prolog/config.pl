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

%%%
% ADORE parameter API 
%%%

:- dynamic adore_param/2.
storeParameter(Name, Value) :- 
	adore_param(Name, Old),!, retract(adore_param(Name,Old)),
	assert(adore_param(Name, Value)).
storeParameter(Name, Value) :- assert(adore_param(Name, Value)).

readParameter(Name, Value) :- adore_param(Name, Value),!.
readParameter(_, '').

%%%
% Global (default) configuration
%%%
:- storeParameter(adore2png_exec,'dot').

%%%
% Local Configuration (load '~/.adore.pl' only if such a file exists)
%%%
:- getenv('HOME',Home),	swritef(F,'%w/.adore.pl',[Home]), 
   exists_file(F), [F].
	

%%%%
%% Debugging ADORE ... (if any, use ~/.adore.el to declare it ...)
%%%%

%debugSubscription(compiler).
%debugSubscription(create).
%debugSubscription(def).
%debugSubscription(set).
%debugSubscription(algo).
%debugSubscription(timer).
%debugSubscription(clone).
%debugSubscription(setify).
%debugSubscription(merge).
%debugSubscription(weave).
%debugSubscription(instantiate).
%debugSubscription(exec).
%debugSubscription(refactor).
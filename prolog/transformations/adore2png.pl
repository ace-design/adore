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

%% adore2png/2: adore2png(+P,+F)
adore2png(P,F) :- 
	adore2dot(P,DotCode), tmp_file('adore2dot',Tmp), open(Tmp,write,Stream), 
	write(Stream,DotCode), close(Stream), adore2png_param(exec,E), 
	swritef(Cmd,'%w -Tpng %w > %w',[E,Tmp,F]), shell(Cmd).

%% display/2: display(+P,-F)
display(P,Tmp) :- 
	tmp_file('adore2png',Tmp), adore2png(P,Tmp), 
	adore2png_param(viewer,E), swritef(Cmd,'%w %w',[E,Tmp]), shell(Cmd).
	
	
	
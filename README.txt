####
## This file is part of ADORE [ www.adore-design.org ]
##
## Copyright (C) 2008-  Sebastien Mosser
##
## ADORE is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## ADORE is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with ADORE; if not, write to the Free Software Foundation,
## Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
## @author      Main Sebastien Mosser          [mosser@polytech.unice.fr]
####

####
## Third party set up
####

To be fully operational, ADORE requires a set of third party tools: 
  - SwiPL
  - Graphviz
  - Java (at least 1.5 for AntLR)

The rest of this file assumes that these tools are correctly installed.

####
## ADORE initial setup
####

  1. Set the ADORE_HOME environment variable in your ~/.bash_profile . You can
     also add the bin directory in your PATH, so Adore will be available
     everywhere

ADORE_HOME=/Users/mosser/repositories/adore
PATH=$ADORE_HOME/bin:$PATH
export ADORE_HOME PATH

  2. Edit the $ADORE_HOME/bin/adore.sh shell script, and set the SWIPL_HOME
     variable.
SWIPL_HOME='/opt/local/bin/'

  3. check $ADORE_HOME/prolog/src/config.pl and adapt it to your settings

  - debugSubscription(channel): print debug info at runtime for the given 
      channel. Channels should be commented in normal usage

  - adore2png: these parameters deal with the ADORE -> Graph transformation
    - exec: path to the 'dot' executable. It can defines some parameters
    - viewer: a command which can display a PNG image (filename as argument)

####
## Editing an ADORE file
####

  1. With Emacs: you just need to add the following line in your ~/.emacs file.
It defines syntax coloring, indentation rules (derived from c-mode)

(load-file (concat (getenv "ADORE_HOME") "/adore.el"))

  2. With anything else: just ... write!

####
## Running ADORE from command line 
####
  - Generating Facts from an ADORE file: the adore2facts.sh scripts wraps the 
    ANTLR-based ADORE compiler.

To compile $FILE and print corresponding Prolog facts on stdout: 
  mosser@asmodeus:~$ adore2facts.sh $FILE
To compile $FILE and also print the AST on stderr:
  mosser@asmodeus:~$ adore2facts.sh $FILE AST
=> show the AST of $FILE
  mosser@asmodeus:~$ adore2facts.sh $FILE AST > /dev/null

  - Running the ADORE engine: the adore.sh scripts wrap up the Prolog engine

To run the engine on an empty database:
  mosser@asmodeus:~$ adore.sh
To run the engine on an ADORE file $F (calls silently the compiler)
  mosser@asmodeus:~$ adore.sh $F


####
## Running ADORE from Emacs
####


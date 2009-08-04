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

  1. Set the ADORE_HOME environment variable in your ~/.bash_profile . You need
     to define a SWIPL_HOME variable to indicate where ADORE can find the 
     Prolog engine on your computer. You can also add the bin directory in 
     your PATH, so ADORE will be available everywhere. 

SWIPL_HOME='/opt/local/bin/'
ADORE_HOME=/absolute/path/to/adore
PATH=$ADORE_HOME/bin:$PATH
export ADORE_HOME PATH

  2. check $ADORE_HOME/prolog/src/config.pl and adapt it to your settings:

  - debugSubscription(channel): print debug info at runtime for the given 
      channel. All facts should be commented in normal usage.

  - adore2png: these parameters deal with the ADORE -> Graph transformation
    - exec: path to the 'dot' executable. It can defines some parameters
    - viewer: a command which can display a PNG image (filename as argument)

####
## Editing an ADORE file
####

  1. With Emacs: you just need to add the following line in your ~/.emacs file.
It defines syntax coloring, indentation rules (derived from c-mode)

(load-file (concat (getenv "ADORE_HOME") "/adore.el"))

/!\ Warning: Emacs inherits its environment from its parent process. So, even
    with an adequate .bash_profile, if the parent process doesn't use it, 
    the $ADORE_HOME variable will ne be set, and the adore mode will not
    be loaded. It basically happends when running Emacs from the Dock on an
    Apple Computer. To fix this mistake, you can use the following ugly hack
    in your ~/.emacs file:

(setenv "ADORE_HOME" "/absolute/path/to/adore")
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "ADORE_HOME") "/bin"))
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

ADORE defines a major mode to be fully operational with Emacs. The mode is
automaticaly loaded when you open a file "*.adore", if you've followed the
emacs setup part of this file.

You can interact with ADORE using the associated Menu, keyboard shortcuts or
interactive functions. Each action is described using the following pattern:
  - Menu Item [Keyboard Shortcut, FunctionName]

The ADORE mode defines 4 actions to interact with the engine: 

  - Run the Adore engine [C-c C-r, adore-run]: 
      => starts the engine using the current file as ADORE code

The engine pops in a new frame. You can interact with it directly as a 
prolog interpreter => just ask questions, it'll find answers for you!
Ther is only ONE engine running at time. So performing a 're-run' will simply 
destroy the previous engine and build a new one.

  - Kill the Adore engine [C-c C-k, adore-kill]: 
      => kill the ADORE engine

This command destructs the Adore engine and its related Emacs artefacts (Frame, 
buffer and process). It's actually the good way to clean your environment.

  - Generate picture [C-c C-p, adore-pict]:
      => generate the associated graph of a given process.

This command loads the current file into ADORE, and asks you for a process is
through the minibuffer. It then generates the expected graph which should pops
in a new window

  - Generate facts [C-c C-f, adore-facts]:
      => generate and display the elementary facts associated to your file

when running this command, a new frame will pop, and display the associated 
facts for the code you're editing. 


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
## along with jSeduite:DataCache; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
## @author      Main Sébastien Mosser          [mosser@polytech.unice.fr]
####
  

## globla variables
GOAL=''
FILE=''
STRICT='false'
VERBOSE='true'

## compiler artefacts
COMPILER='adore2facts.sh'
TMP=`mktemp -t run_sh_XXXXXX`
trap "rm $TMP* 2>/dev/null" 0

## check if the ADORE_HOME variable is set
function check_env() {
    if [ -z $ADORE_HOME ]
    then
	echo "You need to define the ADORE_HOME environment variable"
	exit 1
    fi
}

## Read arguments from the command line
function read_args() {
    for a in $@
    do
	case $a in
	    -h) usage 1;;
	    -f) FILE=$2;;
	    -g) GOAL=$2;;
	    -s) STRICT=$2;;
	    -v) VERBOSE=$2;;
	esac
        shift;
    done
}

## display Usage Tips and exit the process. 
##    => usage $EXPECTED_EXIT_CODE
function usage() {
    echo "####"
    echo "# ADORE Shell Wrapper: adore-wrapper.sh"
    echo '#   Usage: adore-wrapper.sh -f $F -r $R -v true|false'
    echo "#  -h: display this help"
    echo '#  -f $F: $F is the ADORE file you wanna work on.'
    echo "#     -> by default, you'll work on a desperatly empty universe"
    echo '#  -g $G: $R is the ADORE (ie Prolog) goal you wanna run over $F'
    echo "#     -> by default, do not run anything."
#    echo "#  -s true|false: if true, Adore fails on compilation errors"
#    echo "#     -> the default value is 'false'"
    echo "#  -v true|false: if true, Adore print out what's happening"
    echo "#     -> the default value is 'true'"
    echo "####"
    exit $1
}

## Display ADORE args, iv verbose display is required
function display_args() {
    if [ "$VERBOSE" = 'false' ]; then return; fi
    if [ "$FILE" = '' ]; then
	echo "#  - Input file: nothing specified"
    else
	echo "#  - Input File: '$FILE' "
    fi
    if [ "$GOAL" = '' ]; then
	echo "#  - Goal: nothing specified"
    else
	echo "#  - Goal: '$GOAL'"
    fi
    if [ "$STRICT" = 'true' ]; then
	echo "#  - Strict compilation: required"
    else
	echo "#  - Strict compilation: relaxed"
    fi
}


function compile() {
    if [ "$VERBOSE" = 'true' ]; then
	echo "## Compiling conrete syntax into facts ..."
	echo "## Using '$TMP' as compiler output"
    fi;
    $COMPILER $FILE > $TMP
    return $?
}

function run() {
    if [ "$FILE" != '' ]; then G="['$TMP']"; else G="[]"; fi;
    if [ "$GOAL" != '' ]; then G="$G,$GOAL"; fi;
    if [ "$VERBOSE" = 'false' ]; then G="adore_silence(true),$G"; fi;
    $SWIPL_HOME/swipl -s $ADORE_HOME/prolog/src/init.pl -g "$G"
}

function main() {
    read_args $@
    if [ "$VERBOSE" = 'true' ]; then
	echo "#####"
	echo "## ADORE Shell Wrapper"
	display_args
	echo "#####"
    fi;
    if [ "$FILE" != "" ]; then
	compile
	if [ "$STRICT" = 'true' -a $? -ne 0 ]; then
	    echo "# Compilation error, exiting ADORE"
	    exit 1
	fi
    fi;
    run
}

main $@






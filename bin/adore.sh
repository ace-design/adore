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
  
SWIPL_HOME='/opt/local/bin/'

## Do not edit after this line ##

COMPILER='adore2facts.sh'

if [ -z $ADORE_HOME ]
    then
    echo "You need to define the ADORE_HOME env. variable"
    exit 1
fi


FILE=''
echo "%%%%%%"
echo "%% Adore Shell Wrapper"
if [ $# -eq 0 ]
then
    echo "%% Facts database is empty"
else
    echo "$1" | grep -q '\.adore$' 2> /dev/null
    if [ $? -eq 0 ]
    then
	echo "%% Compiling conrete syntax into facts ..."
	echo "%%  - Input: '$1'"
	TMP=`mktemp -t run_sh_XXXXXX`
	trap "rm $TMP* 2>/dev/null" 0
	echo "%%  - Output: '$TMP'"
	$COMPILER $1 > $TMP
	FILE=\'$TMP\'
    else
	FILE=\'$TMP\'
    fi
    echo "%% Using $FILE as facts database"
fi

echo "%%%%%%"


$SWIPL_HOME/swipl -s $ADORE_HOME/prolog/src/init.pl -g [$FILE]

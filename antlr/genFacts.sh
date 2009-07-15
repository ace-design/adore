#!/bin/bash

if [ -z $ADORE_HOME ]
    then
    echo "You need to define the ADORE_HOME env. variable"
    exit 1
fi

JAR=$ADORE_HOME/antlr/adore-compiler.jar

java -jar $JAR $@

#!/bin/bash

FILE=$1

## Exec Time
DATA=`cat $FILE | grep inferences | cut -d ' ' -f 7`
EXECTIME='='
for i in $DATA
do
    EXECTIME="$EXECTIME $i +"
done

## Inferences
DATA=`cat $FILE | grep inferences | cut -d ' ' -f 2 | tr -d ','`
INFERENCES='='
for i in $DATA
do
    INFERENCES="$INFERENCES $i +"
done

## Actions
DATA=`cat $FILE | grep Result | cut -d ' ' -f7`
ACTIONS='='
for i in $DATA
do
    ACTIONS="$ACTIONS $i +"
done

DATA=`cat $FILE | grep done | cut -d ' ' -f5`
for i in $DATA
do
    ACTIONS="$ACTIONS $i) +"
done

echo $EXECTIME
echo $INFERENCES
echo $ACTIONS


#!/bin/sh

find . -name *~ -exec rm -vf '{}' \;
rm -rvf bin/fr
find . -name *.class -exec rm -vf '{}' \;
find . -name *.tokens -exec rm -vf '{}' \;
rm -vf ./*.jar

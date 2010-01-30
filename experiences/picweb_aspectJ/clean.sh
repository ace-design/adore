#!/bin/sh

find . -name "*.class" -exec rm -vf {} \;
find . -name "*~" -exec rm -vf {} \;
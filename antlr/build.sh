#!/bin/sh

CP='fr/unice/i3s/modalis/adore/language/'
echo "####"
echo "## Generating Java Source file"
java -jar jars/antlr-3.1.3.jar -o bin/$CP Adore*.g 
echo "## Compiling generated Java Code"
cp Compiler.java bin/$CP.
cp Main.java bin/.
javac -cp jars/antlr-3.1.3.jar bin/$CP*.java bin/Main.java
find bin -name \*.java -exec rm -f '{}' \;
echo "## Creating JAR file"
cd bin
jar cmf ../manifest.txt ../adore-compiler.jar  fr Main.class
echo "####"
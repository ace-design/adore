parser grammar AdoreParser;

options {
  output=AST;
  ASTLabelType=CommonTree;
  tokenVocab=AdoreLexer;
}

tokens {
	DEFINITIONS;
	ORCHESTRATION;
	EVOLUTION;
	BEHAVIOUR;
}

@header { package fr.unice.i3s.modalis.adore.language; }

definitions
	:	definition+ EOF -> ^(DEFINITIONS definition+);
definition
	:	ORCH s=IDENTIFIER DBL_COL o=IDENTIFIER behaviour 
		-> ^(ORCHESTRATION $s $o) 
	|	EVOL n=IDENTIFIER behaviour
		-> ^(EVOLUTION $n) ;
behaviour
	:	LFT_BRCKT RGHT_BRCKT 
		-> ^(BEHAVIOUR)  ;

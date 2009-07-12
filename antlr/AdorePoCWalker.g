tree grammar AdorePoCWalker;

options {
  ASTLabelType = CommonTree;
  tokenVocab=AdoreParser;
}

@header { package fr.unice.i3s.modalis.adore.language; }

definitions returns [String value]
	:	DEFINITIONS definition+ ;
	
definition 
	//@init( String value = new String())
	:	ORCHESTRATION s=IDENTIFIER o=IDENTIFIER { System.out.println($s.text); }
	|	EVOLUTION n=IDENTIFIER {System.out.println($n); };

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
	ACTIVITIES;
	ACTIVITY;
	WAIT_FOR;
	PARAMETERS;
	INPUTS;
	OUTPUTS;
	KIND;
	INVOKE;
	ASSIGNMENT;
}

@header { package fr.unice.i3s.modalis.adore.language; }

definitions
	:	definition+ 
			-> ^(DEFINITIONS definition+);
definition
	:	ORCH s=IDENTIFIER DBL_COL o=IDENTIFIER behaviour 
			-> ^(ORCHESTRATION $s $o behaviour) 
	|	EVOL n=IDENTIFIER behaviour
			-> ^(EVOLUTION $n behaviour) ;
behaviour
	:	LFT_BRCKT activities relations? RGHT_BRCKT 
			-> ^(BEHAVIOUR activities relations?)  ;
		
activities
	:	ACTIVITIES LFT_BRCKT activity+ RGHT_BRCKT
			-> ^(ACTIVITIES activity+);

activity
	:	 id=IDENTIFIER RIGHT_PAREN  kind in=param_list SEMI_COLON 
			-> ^(ACTIVITY $id kind ^(INPUTS $in));

kind	
	:	RECEIVE -> ^(KIND RECEIVE)
	|	REPLY	-> ^(KIND REPLY)
	|	THROW	-> ^(KIND THROW)
	|	f=IDENTIFIER 				-> ^(KIND ASSIGNMENT $f)
	|	s=IDENTIFIER DBL_COL o=IDENTIFIER 	-> ^(KIND INVOKE $s $o);	 

param_list
	:	LEFT_PAREN RIGHT_PAREN -> ^(PARAMETERS)
	|	LEFT_PAREN var_list RIGHT_PAREN -> ^(PARAMETERS var_list);

var_list:	id=IDENTIFIER -> $id
	|	id=IDENTIFIER COMMA var_list -> $id var_list;

relations
	:	RELATIONS LFT_BRCKT relation+ RGHT_BRCKT
			-> ^(RELATIONS relation+);

relation
	:	wait_for ;

wait_for
	:	left=IDENTIFIER LT right=IDENTIFIER SEMI_COLON
			-> ^(WAIT_FOR $left $right);
			

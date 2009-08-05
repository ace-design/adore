/** This file is part of ADORE [ www.adore-design.org ]
 *
 * Copyright (C) 2008-  Sebastien Mosser
 *
 * ADORE is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ADORE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with jSeduite:DataCache; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * @author      Main SŽbastien Mosser          [mosser@polytech.unice.fr]
 * @remarks: 	this is a QUICK AND DIRTY compiler ... really ...
 *		Do not do this at home ... !
 **/
 
parser grammar AdoreParser;

options {
  output=AST;
  ASTLabelType=CommonTree;
  tokenVocab=AdoreLexer;
}

tokens {
	DEFINITIONS;
	DEF;
	ORCHESTRATION;
	FRAGMENT;
	REQUIRE;
	VARIABLES;
	VAR;
	RELATIONS;
	ACTIVITIES;
	ACT;
	OUTS;
	INS;
	BIND;
	KIND;
	INVOKE;
	ASSIGNMENT;
	WAIT_FOR;
	WEAK_WAIT;
	COND_TRUE;
	COND_FALSE;
	UNIT;
	MERGE_FRAG;
	SCALAR;
	SET;
	ANONYMOUS;
	PARAMS;
	FIELDS;
	BLOCK;
	TARGET;
	ELEM_REF;
	OUTPUT;
}

@header { package fr.unice.i3s.modalis.adore.language; }

definitions
	:	definition+ 				-> ^(DEFINITIONS definition+) ;

definition
	:	REQU f=STR SEMI				-> ^(DEF ^(REQUIRE $f))
	|	ORCH s=ID DBL_COL o=ID core 		-> ^(DEF ^(ORCHESTRATION $s $o) core)
	|	FRAG n=ID params? core 			-> ^(DEF ^(FRAGMENT $n ^(PARAMS params?)) core)
	|	composition				-> ^(DEF composition)
	;

composition
	:	COMPOSITION (s=ID (DBL_COL o=ID)?)? (AS out=ID)? merge_core	
							-> ^(UNIT ^(TARGET $s? $o?) ^(OUTPUT $out?) merge_core)
	;

params	
	:	LT plist GT				-> plist
	;	
core	
	:	LFT_BRCKT vars acts rels? RGHT_BRCKT	-> vars acts rels? ;

vars
	: 	VARS LFT_BRCKT decl_var* RGHT_BRCKT 	-> ^(VARIABLES decl_var*);

decl_var
	:	variable 				-> ^(VAR variable)
	|	constant 				-> ^(CONST constant);
	
variable
	:	varname AS t=ID SEMI			-> varname $t
	|	varname ASSIGN v=STR AS t=ID SEMI	-> varname $t $v ;


varname
	:	v=ID					-> ^(SCALAR $v)
	|	v=ID STAR				-> ^(SET $v)
	;

constant
	:	CONST varname ASSIGN v=STR AS t=ID SEMI	-> varname $t $v;
	
acts	
	:	ACTS LFT_BRCKT activity+ RGHT_BRCKT 	-> ^(ACTIVITIES activity+) ;
	
activity
	:	id=ID DOT content SEMI 			-> ^(ACT $id content);

kind	
	:	RECEIVE | REPLY | THROW | HOOK | NOP
	|	s=ID DBL_COL o=ID 			-> INVOKE $s $o
	|	fct=ID					-> ASSIGNMENT $fct;
	
content	
	:	kind inputs 				-> ^(KIND kind) inputs ^(OUTS)
	|	l=ID ASSIGN r=ID			-> ^(KIND ASSIGNMENT) ^(INS ^(SCALAR $r)) ^(OUTS ^(SCALAR $l))
	|	l=ID STAR ASSIGN r=ID STAR 		-> ^(KIND ASSIGNMENT) ^(INS ^(SET $r)) ^(OUTS ^(SET $l))
	|	outputs kind inputs			-> ^(KIND kind) inputs outputs;

outputs	
	:	varaccess ASSIGN			-> ^(OUTS varaccess)
	|	LFT_PAREN vlist+ RGHT_PAREN ASSIGN 	-> ^(OUTS vlist+);
	
inputs	
	:	LFT_PAREN vlist? RGHT_PAREN 		-> ^(INS vlist?);	


vlist	:	plist | nplist;
plist
	: 	varaccess				-> varaccess
	|	varaccess COMMA plist 			-> varaccess plist;

nplist  
	:	left=ID COLON varaccess 		-> ^(BIND varaccess $left)
	|	left=ID COLON varaccess COMMA nplist 	-> ^(BIND varaccess $left) nplist ;

varaccess
	:	id=ID 					-> ^(SCALAR $id)
	|	id=ID fieldaccess+ STAR?		-> ^(SCALAR $id ^(FIELDS fieldaccess+ STAR?))
	|	id=ID STAR				-> ^(SET $id)
	|	value=STR AS type=ID			-> ^(ANONYMOUS $value $type)
	;	

fieldaccess
	:	DOT i=ID				-> $i
	;

rels	
	:	RELS LFT_BRCKT  rel+ RGHT_BRCKT 	-> ^(RELATIONS rel+);

ord	: 	(a=ID -> $a)  | PREDS | SUCCS;

rel	
	:	l=ord LT r=ord SEMI 			-> ^(WAIT_FOR $r $l)
	|	l=ord LT LT r=ord SEMI			-> ^(WEAK_WAIT $r $l)
	| 	l=ord LT r=ord WHEN c=ID SEMI		-> ^(COND_TRUE $r $l $c)
	|	l=ord LT r=ord WHEN NOT c=ID SEMI	-> ^(COND_FALSE $r $l $c)
	;
	
merge_core
	:	LFT_BRCKT directive+ RGHT_BRCKT 	-> directive+
	;

directive
	:	APPLY e=ID INTO actBlock  SEMI 		
							-> ^(MERGE_FRAG $e actBlock)
	| 	APPLY e=ID LFT_PAREN  pBind RGHT_PAREN INTO  actBlock  SEMI	
							-> ^(MERGE_FRAG $e actBlock pBind)
	;

elemRef	:	s=ID ((DBL_COL o=ID)? DBL_COL a=ID)?	-> ^(ELEM_REF $s $o? $a?)
	;
actBlock
	:	elemRef					-> ^(BLOCK elemRef)
	|	LFT_BRCKT actList RGHT_BRCKT 		-> ^(BLOCK actList)
	;

actList
	: 	elemRef					-> elemRef
	|	elemRef COMMA actList			-> elemRef actList
	;
	
pBind  
	:	left=ID COLON s=STR 			-> ^(BIND $s $left)
	|	left=ID COLON s=STR COMMA pBind 	-> ^(BIND $s $left) pBind ;

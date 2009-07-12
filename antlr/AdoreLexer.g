lexer grammar AdoreLexer;

@header { package fr.unice.i3s.modalis.adore.language; }

/** KEYWORDS **/
ORCH: 		'orchestration';
EVOL:		'evolution';
ACTIVITIES:	'activities';
RELATIONS:	'relations';
RECEIVE:	'receive';
REPLY:		'reply';
THROW:		'throw';
//DO:		'do';

/** PUNCTUATION**/
DBL_COL:	'::';
LFT_BRCKT:	'\{';
RGHT_BRCKT:	'\}';
LEFT_PAREN:	'(';
RIGHT_PAREN:	')';
SEMI_COLON:	';';
LT:		'<';
ASSIGN:		':=';
COMMA:		',';
COLON:		':';	

/** Expected tokens **/
IDENTIFIER:	('a'..'z') ID;
ID	:	('A'..'Z'|'0'..'9'|'a'..'z')*;	

/** IGNORED **/
INLN_COM: 	'//' ~('\r' | '\n')* NL 		{ skip(); };
MLT_COM:	'/*' '\u0000'..'\uFFFE'+ '*/' NL?	{ skip(); };
NL: 		('\r'? '\n')+				{ skip(); };
WSPCE: 		(' '|'\t')+ 				{ skip(); };

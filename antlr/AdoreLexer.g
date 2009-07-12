lexer grammar AdoreLexer;

@header { package fr.unice.i3s.modalis.adore.language; }

/** KEYWORDS **/
ORCH: 		'orchestration';
EVOL:		'evolution';
DBL_COL:	'::';
LFT_BRCKT:	'\{';
RGHT_BRCKT:	'\}';	

/** Expected tokens **/
IDENTIFIER:	('a'..'z') ID;
ID	:	('A'..'Z'|'0'..'9'|'a'..'z')*;	

/** IGNORED **/
INLN_COM: 	'//' ~('\r' | '\n')* NL 		{ skip(); };
MLT_COM:	'/*' '\u0000'..'\uFFFE'+ '*/' NL?	{ skip(); };
NL: 		('\r'? '\n')+				{ skip(); };
WSPCE: 		(' '|'\t')+ 				{ skip(); };

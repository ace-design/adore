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
lexer grammar AdoreLexer;

@header { package fr.unice.i3s.modalis.adore.language; }
 
 /** KRYWORDS **/

ORCH: 		'orchestration';
FRAG:		'fragment';
ACTS:		'activities';
RELS:		'relations';
RECEIVE	:	'receive';
REPLY:		'reply';
THROW:		'throw';
HOOK:		'hook';
NOP:		'nop';
VARS:		'variables';
AS:		'as';
CONST:		'const';
REQU:		'require';
WHEN:		'when';
COMPOSITION:	'composition';
APPLY:		'apply';
TOSET:		'toSet';
FAILURE	:	'fail';

/** PUNCTUATION**/
STAR:		'*';
NOT:		'!';
PREDS:		'^';
SUCCS:		'$';
DBL_COL:	'::';
LFT_BRCKT:	'\{';
RGHT_BRCKT:	'\}';
LFT_PAREN:	'(';
RGHT_PAREN:	')';
SEMI:		';';
LT:		'<';
GT	:	'>';
ASSIGN:		':=';
COMMA:		',';
COLON:		':';
INTO:		'=>';	
DOT:		'.';

/** Expected tokens **/
ID:		('a'..'z') ('A'..'Z'|'0'..'9'|'a'..'z')* ;

STR:	'\'' ~'\''* '\'';
	
/** IGNORED **/
INLN_COM: 	'//' ~('\r' | '\n')* NL 	{ skip(); };
COMMENT	:	'/*' .* '*/'			{ skip(); };
NL: 		('\r'? '\n')+			{ skip(); };
WSPCE: 		(' '|'\t')+ 			{ skip(); };

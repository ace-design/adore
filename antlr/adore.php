<?php
/******************************************************************************
 * adore.php
 * -----
 * Author: Sebastien Mosser (mosser@polytech.unice.fr)
 * Copyright: (c) 2008 Sebastien Mosser (www.adore-design.org) 
 * Release Version: 1\.0\.8
 * Date Started: 2009/08/27
 *
 * Adore language file for GeSHi.
 *
 * CHANGES
 * -------
 *
 * TODO 
 * ----
 *
 ******************************************************************************
 *
 *     This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *****************************************************************************/

$language_data = array (
    'LANG_NAME' => 'Adore Textual Description',
    'COMMENT_SINGLE' => array(2 => '#'),
    'COMMENT_MULTI' => array('/*' => '*/'),
    //Multiline-continued single-line comments
    'COMMENT_REGEXP' => array(1 => '/\/\/(?:\\\\\\\\|\\\\\\n|.)*$/m'),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array("'"),
    'ESCAPE_CHAR' => '\\',
    'NUMBERS' => GESHI_NUMBER_INT_BASIC,
    'KEYWORDS' => array(
        1 => array('require', 'orchestration', 'fragment', 'composition'),
        2 => array('const','nop','receive','reply','throw','when','as',
                   'apply','hook', 'toSet', 'fail'),
        3 => array('variables', 'activities', 'relations'),
        4 => array('integer', 'boolean', 'struct', 'float', 'string', 'time', 
                   'date', 'dateTime'),
        ),
    'SYMBOLS' => array('(', ')', '{', '}','^', '$','=','>',':'),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => false,
        2 => false,
        3 => false,
        4 => false,
        ),
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => 'color: #b1b100;',
            2 => 'color: #000000; font-weight: bold;',
            3 => 'color: #000066;',
            4 => 'color: #993333;'
            ),
        'COMMENTS' => array(
            1 => 'color: #666666; font-style: italic;',
            2 => 'color: #339933;',
            'MULTI' => 'color: #808080; font-style: italic;'
            ),
        'ESCAPE_CHAR' => array(
            0 => 'color: #000099; font-weight: bold;'
            ),
        'BRACKETS' => array(
            0 => 'color: #009900;'
            ),
        'STRINGS' => array(
            0 => 'color: #ff0000;'
            ),
        'NUMBERS' => array(
            0 => 'color: #0000dd;',
            ),
        'METHODS' => array(
            1 => 'color: #202020;',
            2 => 'color: #202020;'
            ),
        'SYMBOLS' => array(
            0 => 'color: #339933;'
            ),
        'REGEXPS' => array(
            ),
        'SCRIPT' => array(
            )
        ),
    'OOLANG' => true,
    'OBJECT_SPLITTERS' => array(
        1 => '.',
        2 => '::'
        ),
    'REGEXPS' => array(
        ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(
        ),
    'HIGHLIGHT_STRICT_BLOCK' => array(
        ),
    'TAB_WIDTH' => 4
);

?>
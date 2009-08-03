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
 * @author      Main Sébastien Mosser          [mosser@polytech.unice.fr]
 * @remarks: 	this is a QUICK AND DIRTY compiler ... really ...
 *		Do not do this at home ... !
 **/
package fr.unice.i3s.modalis.adore.language;

import java.io.*;
import java.util.Scanner;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import java.util.ArrayList;

public class Compiler {

    private boolean debug = false;

    public ArrayList<String> run(File f) throws Exception {	
	return this.run(f.getAbsolutePath());
    }

    public ArrayList<String> run(String path) throws Exception {	
	CommonTree ast = getAST(new FileReader(path));
	if(debug)
	    System.err.println(ast.toStringTree());
	return processAST(ast);
    }

    public void debug() { this.debug = true; }
    

    /************************************************
     * Private Lexer , Parser & Walker manipulation *
     * You really do not want to know ...           *
     ************************************************/

    private CommonTree getAST(Reader reader) throws Exception {
        AdoreParser tokenParser = new AdoreParser(getTokenStream(reader));
        AdoreParser.definitions_return parsed = tokenParser.definitions(); 
        reader.close();
        return (CommonTree) parsed.getTree();
    }

    private CommonTokenStream getTokenStream(Reader reader) throws Exception {
        AdoreLexer lexer = new AdoreLexer(new ANTLRReaderStream(reader));
        return new CommonTokenStream(lexer);
    }

    private ArrayList<String> processAST(CommonTree ast) throws Exception {
        AdorePoCWalker w = new AdorePoCWalker(new CommonTreeNodeStream(ast));
        return w.definitions(); 
    }

}

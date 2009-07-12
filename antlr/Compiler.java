package fr.unice.i3s.modalis.adore.language;

import java.io.*;
import java.util.Scanner;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import java.util.ArrayList;

public class Compiler {

    public ArrayList<String> run(String path) throws Exception {	
	CommonTree ast = getAST(new FileReader(path));
	System.out.println(ast.toStringTree());
	return processAST(ast);
    }

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
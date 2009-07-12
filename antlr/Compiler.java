package fr.unice.i3s.modalis.adore.language;

import java.io.*;
import java.util.Scanner;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Compiler {

    public String run(String path) throws Exception {	
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

    private String processAST(CommonTree ast) throws RecognitionException {
        AdorePoCWalker w = new AdorePoCWalker(new CommonTreeNodeStream(ast));
        return w.definitions(); // start rule method
    }
}

/**
import java.io.*;
import java.util.Scanner;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Processor {

    public static void main(String[] args) throws IOException, RecognitionException {
        if (args.length == 0) {
            new Processor().processInteractive();
        } else if (args.length == 1) { // name of file to process was passed in
            new Processor().processFile(args[0]);
        } else { // more than one command-line argument
            System.err.println("usage: java com.ociweb.math.Processor [file-name]");
        }
    }

    private void processFile(String filePath) throws IOException, RecognitionException {
        CommonTree ast = getAST(new FileReader(filePath));
        //System.out.println(ast.toStringTree()); // for debugging
        processAST(ast);
    }


    private CommonTokenStream getTokenStream(Reader reader) throws IOException {
        MathLexer lexer = new MathLexer(new ANTLRReaderStream(reader));
        return new CommonTokenStream(lexer);
    }

    private void processAST(CommonTree ast) throws RecognitionException {
        MathTree treeParser = new MathTree(new CommonTreeNodeStream(ast));
        treeParser.script(); // start rule method
    }

    private void processInteractive() throws IOException, RecognitionException {
        MathTree treeParser = new MathTree(null); // a TreeNodeStream will be assigned later
        Scanner scanner = new Scanner(System.in);

        while (true) {
            System.out.print("math> ");
            String line = scanner.nextLine().trim();
            if ("quit".equals(line) || "exit".equals(line)) break;
            processLine(treeParser, line);
        }
    }

    // Note that we can't create a new instance of MathTree for each
    // line processed because it maintains the variable and function Maps.
    private void processLine(MathTree treeParser, String line) throws RecognitionException {
        // Run the lexer and token parser on the line.
        MathLexer lexer = new MathLexer(new ANTLRStringStream(line));
        MathParser tokenParser = new MathParser(new CommonTokenStream(lexer));
        MathParser.statement_return parserResult =
            tokenParser.statement(); // start rule method

        // Use the token parser to retrieve the AST.
        CommonTree ast = (CommonTree) parserResult.getTree();
        if (ast == null) return; // line is empty

        // Use the tree parser to process the AST.
        treeParser.setTreeNodeStream(new CommonTreeNodeStream(ast));
        treeParser.statement(); // start rule method
    }

} 
 **/
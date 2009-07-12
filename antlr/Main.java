import fr.unice.i3s.modalis.adore.language.*;
import java.util.ArrayList;

public class Main {

    public static void main(String[] args) throws Exception {
	String filePath = args[0];
	fr.unice.i3s.modalis.adore.language.Compiler compiler = 
	    new fr.unice.i3s.modalis.adore.language.Compiler();
	ArrayList<String> facts = compiler.run(filePath);
	String pl = toProlog(facts);
	System.out.println(pl);
    }

    public static String toProlog(ArrayList<String> facts) {
	String r = ":- \n\t";
	for(String s: facts)
	    r += s + ",\n\t";
	return r.substring(0,r.length()-3)+".\n";
    }
}
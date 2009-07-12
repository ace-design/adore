import fr.unice.i3s.modalis.adore.language.*;

public class Main {
    public static void main(String[] args) throws Exception {
	String filePath = args[0];
	fr.unice.i3s.modalis.adore.language.Compiler compiler = 
	    new fr.unice.i3s.modalis.adore.language.Compiler();
	compiler.run(filePath);
    }
}
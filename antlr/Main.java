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

import fr.unice.i3s.modalis.adore.language.*;
import java.util.ArrayList;

public class Main {

    public static void main(String[] args) throws Exception {
	String filePath = args[0];
	fr.unice.i3s.modalis.adore.language.Compiler compiler = 
	    new fr.unice.i3s.modalis.adore.language.Compiler();
	if (args.length == 2 && args[1].equals("AST"))
	    compiler.debug();
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
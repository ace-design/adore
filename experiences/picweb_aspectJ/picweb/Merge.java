package picweb;

public class Merge {

    public String[] run(String[] d1, String[] d2) {

	String[] small, large;
	if (d1.length < d2.length) { small = d1; large = d2; } 
	else { small = d2; large = d1; };

	String[] result = new String[small.length + large.length];

	int cpt = 0;
	for(int i = 0; i < small.length; i++) {
	    result[cpt++] = small[i];
	    result[cpt++] = large[i];
	}

	for(int i = small.length; i < large.length; i++)
	    result[cpt++] = large[i];
	return result;
    }
}


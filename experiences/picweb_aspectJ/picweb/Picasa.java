package picweb;

import java.util.Random;

public class Picasa {

    public String[] getOnTag(String tag) {
	Random bag = new Random();
	String[] res = new String[4+bag.nextInt(20)];
	for(int i = 0; i < res.length; i++) {
	    res[i] = "http://picasa/" + tag + "/"+i+"/" + 
		Math.abs(bag.nextInt()) +".jpg";
	}
	return res;
    }
}
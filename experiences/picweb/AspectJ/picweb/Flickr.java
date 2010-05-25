package picweb;

import java.util.Random;

public class Flickr {

    public String[] exploreFolksonomy(String key, String tag) {
	if (! key.equals("flickr_secret_key"))
	    return new String[0];
	Random bag = new Random();
	String[] res = new String[4+bag.nextInt(20)];
	for(int i = 0; i < res.length; i++) {
	    res[i] = "http://flickr/" + tag + "/"+i+"/" + Math.abs(bag.nextInt()) +".jpg";
	}
	return res;
    }
}
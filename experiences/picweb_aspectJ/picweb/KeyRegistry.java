package picweb;

public class KeyRegistry {
    public String get(String name) {
	if (name.equals("flickr"))
	    return "flickr_secret_key";
	return "??";
    }
}
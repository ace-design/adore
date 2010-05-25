package picweb;

public class PicWeb {

    public static Flickr FLICKR = new Flickr();
    public static KeyRegistry REGISTRY = new KeyRegistry();

    public String[] get(String tag, int threshold) {
	String key = REGISTRY.get("flickr");
	String[] flickrSet = FLICKR.exploreFolksonomy(key,tag);
	return flickrSet;
    }

}
package picweb;
import java.io.*;

public class Cache {

    public boolean isValid(String tag, int delta) {
	return (new File("cache_" + tag + ".data")).exists();
    }
    public String[] getValue(String tag) {
	File f = new File("cache_"+tag+".data");
        try {
            ObjectInputStream in = 
		new ObjectInputStream(new FileInputStream(f));
            return (String[]) in.readObject();
        }
        catch (Exception e) { return null; }
    }

    public boolean memorize(String tag, String[] data) {
        File f = new File("cache_"+tag+".data");
        if (f.exists())
            f.delete();
        try {
            f.createNewFile();
            ObjectOutputStream out =
                    new ObjectOutputStream(new FileOutputStream(f));
            out.writeObject(data);
            out.close();
            return true;
        } catch(Exception e) { return false; }
    }
}

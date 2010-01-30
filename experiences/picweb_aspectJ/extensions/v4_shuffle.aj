package extensions;
import picweb.Shuffle;

public aspect v4_shuffle extends vX_pointcuts
{
    declare precedence: v1_truncate ;

    public static Shuffle SHUFFLE = new Shuffle();

    String[] around(String s, int i): callPicWeb(s,i) {
	System.out.println("===> around@shuffle: start <====");
	String[] raw = proceed(s,i);
	String[] result = SHUFFLE.run(raw);
	System.out.println("===> around@shuffle: end <====");	
	return result;
    }
    
}
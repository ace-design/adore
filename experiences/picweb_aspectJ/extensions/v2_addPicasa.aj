package extensions;

import picweb.Picasa;
import picweb.Merge;

public aspect v2_addPicasa extends vX_pointcuts
{
    declare precedence: v1_truncate, v3_cache_60;

    public static Picasa PICASA = new Picasa();
    public static Merge MERGE = new Merge();

    String[] around(String tag, int threshold): callPicWeb(tag,threshold) {
	System.out.println("===> around@addPicasa: start <====");
	String[] picasa = PICASA.getOnTag(tag);
	String[] raw = proceed(tag,threshold);
	String[] result = MERGE.run(raw,picasa);
	System.out.println("===> around@addPicasa: end <====");
	return result;
    }
}

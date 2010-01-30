package extensions;
import picweb.Cache;

public abstract aspect cache extends vX_pointcuts
{    
    declare precedence: v1_truncate;

    protected abstract int getDelta();

    public static Cache CACHE = new Cache();

    String[] around(String tag, int threshold): callPicWeb(tag,threshold) {
	System.out.println("===> around@cache: start <====");
	String[] result;
	if (CACHE.isValid(tag, getDelta()))
	    result = CACHE.getValue(tag);
	else {
	    result = proceed(tag,threshold);
	    CACHE.memorize(tag,result);
	}
	System.out.println("===> around@cache: end <====");
	return result;
    }
}

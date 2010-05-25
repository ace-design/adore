package picweb;

public class Truncate {

    public String[] run(String[] data, int limit) {
	if (data.length <= limit)
	    return data;
	String[] result = new String[limit];
	for(int i = 0; i < limit; i++)
	    result[i]= data[i];
	return result;
    }
}
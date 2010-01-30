package picweb;

import java.util.Random;

public class Shuffle {
    
    public String[] run(String[] data) {
	Random bag = new Random();
	for(int i = 0; i < data.length * data.length; i++) {
	    int a = bag.nextInt(data.length);
	    int b = bag.nextInt(data.length);
	    String tmp = data[a];
	    data[a] = data[b]; 
	    data[b] = tmp;
	}
	return data;
    }

}
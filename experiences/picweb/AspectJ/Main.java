import picweb.PicWeb;

public class Main {
    public static void main(String[] args) {

	System.out.println("### Main: Calling PicWeb ###");
	PicWeb picweb = new PicWeb();
	String[] data = picweb.get("aTag",3);

	System.out.println("### Main: Displaying Results ###");
	for(int i = 0; i < data.length; i++) {
	    System.out.println(data[i]);
	}
    }
}
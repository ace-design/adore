package foo;



aspect picasa extends factorized
{

    String[] around(String tag, int threshold): callPicWeb(tag,threshold) {

	System.out.println("===> around@picasa: start <====");
	String[] picasa = PICASA.getOnTag(tag);
	String[] raw = proceed(tag,threshold);
	String[] result = MERGE.run(raw,picasa);
	System.out.println("===> around@picasa: end <====");
	return result;
    }   
}
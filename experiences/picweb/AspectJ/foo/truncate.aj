package foo;



aspect truncate extends factorized
{
    String[] around(String s, int i): callPicWeb(s,i) {
	System.out.println("===> around@truncate: start <====");
	String[] raw = proceed(s,i);
	String[] result = TRUNCATE.run(raw,i);
	System.out.println("===> around@truncate: end <====");	
	return result;
    }
}

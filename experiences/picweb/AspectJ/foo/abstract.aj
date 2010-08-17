package foo;

import picweb.*;

abstract aspect factorized {

    declare precedence: truncate, picasa;

    protected pointcut callPicWeb(String s, int l): 
      target(PicWeb) && call(String[] get(String, int)) && args(s,l);

    protected Picasa PICASA = new Picasa();
    protected Merge MERGE = new Merge();
    protected Truncate TRUNCATE = new Truncate();

}
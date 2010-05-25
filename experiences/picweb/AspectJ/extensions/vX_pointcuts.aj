package extensions;
import picweb.PicWeb;


public abstract aspect vX_pointcuts
{
    protected pointcut callPicWeb(String s, int l): 
      target(PicWeb) && call(String[] get(String, int)) && args(s,l);
}
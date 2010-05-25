package extensions;

public aspect trace
{
    pointcut methodCall():  call(* picweb.*.*(..));

    before(): methodCall() { 
	System.out.println(" -> Entering: \t" + thisJoinPoint); 
    }

    after(): methodCall() { 
	System.out.println(" <- Leaving: \t" + thisJoinPoint); 
    }

}
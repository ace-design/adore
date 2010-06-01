package PhoneCie;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

/**
 * @date de création: 31 mai 2010 21:47:50
 * @auteur: Freddy Lallement
 */
@WebService()
public class PhoneCieService {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "getInfo")
    public GetInfoOutput getInfo(@WebParam(name = "r") GetInfoInput r) {
        //TODO write your implementation code here:
        return null;
    }

}

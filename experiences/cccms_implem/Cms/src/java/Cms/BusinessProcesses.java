package Cms;

import javax.jws.Oneway;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

/**
 * @date de création: 31 mai 2010 20:22:59
 * @auteur: Freddy Lallement
 */
@WebService()
public class BusinessProcesses {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "captureWitnessReport")
    @Oneway
    public void captureWitnessReport(@WebParam(name = "r") CaptureWitnessReportInput r) {
        //TODO write your implementation code here:
    }

    
}

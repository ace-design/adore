/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package UI;

import javax.jws.Oneway;
import javax.jws.WebService;
import javax.jws.WebMethod;
import javax.jws.WebParam;

/**
 *
 * @author savadogo
 */
@WebService()
public class UIService {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "promptPrelimInfo")
    public PromptPrelimInfoOutput promptPrelimInfo(@WebParam(name = "r")
    PromptPrelimInfoInput r) {
        //TODO write your implementation code here:
        return null;
    }

    /**
     * Web service operation
     */
    @WebMethod(operationName = "promptWitnessInfo")
    public PromptWitnessInfoOutput promptWitnessInfo(@WebParam(name = "r")
    PromptWitnessInfoInput r) {
        //TODO write your implementation code here:
        return null;
    }

    /**
     * Web service operation
     */
    @WebMethod(operationName = "promptCheckList")
    @Oneway
    public void promptCheckList() {
    }

   

}

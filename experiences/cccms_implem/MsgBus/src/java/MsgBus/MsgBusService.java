/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package MsgBus;

import javax.jws.Oneway;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

/**
 *
 * @author savadogo
 */
@WebService()
public class MsgBusService {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "send")
    @Oneway
    public void send(@WebParam(name = "r") SendInput r) {
    }

}

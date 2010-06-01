package Cms;

import javax.jws.Oneway;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

/**
 * @date de création: 31 mai 2010 20:22:07
 * @auteur: Freddy Lallement
 */
@WebService()
public class CmsService {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "assignEmergencyLvl")
    @Oneway
    public void assignEmergencyLvl(@WebParam(name = "r") AssignEmergencyLvlInput r) {
        //TODO write your implementation code here:
    }

    /**
     * Web service operation
     */
    @WebMethod(operationName = "buildCheckList")
    public BuildCheckListOutput buildCheckList(@WebParam(name = "r") BuildCheckListInput r) {
        //TODO write your implementation code here:
        return null;
    }

    /**
     * Web service operation
     */
    @WebMethod(operationName = "setWitness")
    @Oneway
    public void setWitness(@WebParam(name = "r") SetWitnessInput r) {
        //TODO write your implementation code here:
    }

    /**
     * Web service operation
     */
    @WebMethod(operationName = "ValidateWitnessInfo")
    public ValidateWitnessInfoOutput ValidateWitnessInfo(@WebParam(name = "r") ValidateWitnessInfoInput r) {
        //TODO write your implementation code here:
        return null;
    }
}

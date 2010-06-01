/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class MobileEmployee extends CmsEmployee{

  private String currentLocation;
  private String pdaNumber;
  
    public MobileEmployee(){
        super();
        this.currentLocation="";
        this.pdaNumber="";
    }

    public MobileEmployee(String name,String adress, String identification,String phone,String dateOfBirth,String gsm,String expertise,String loginId,String password, String accessRights, String status, String currentLocation,String pdaNumber){
       super(name,adress,identification,phone,dateOfBirth,gsm,expertise,loginId,password,accessRights,status);
       this.currentLocation=currentLocation;
       this.pdaNumber=pdaNumber;

    }
    
    public String getPdaNumber(){
     return   pdaNumber ;
    }

    public void setPdaNumber(String pdaNumber){
        this.pdaNumber=pdaNumber;
    }

    public String getCurrentLocation(){
        return currentLocation;
    }

   
    public void setCurrentLocation (String currentLocation){
        this.currentLocation=currentLocation;
    }
}
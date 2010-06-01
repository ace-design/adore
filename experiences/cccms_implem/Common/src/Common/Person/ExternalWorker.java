/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class ExternalWorker extends Worker{

    private String id;
    private String currentLocation;

    public ExternalWorker(){
        super();
        this.id="";
        this.currentLocation="";

    }

    public ExternalWorker(String name,String adress, String identification,String phone,String dateOfBirth,String gsm,String expertise,String id,String currentLocation){
       super(name,adress,identification,phone,dateOfBirth,gsm,expertise);
        this.id=id;
        this.currentLocation=currentLocation;
    }

    public String getId(){
        return id;
    }

     public String getCurrentLocation(){
        return currentLocation;
    }

    public void setId (String id){
        this.id=id;
    }

    public void setCurrentLocation (String currentLocation){
        this.currentLocation=currentLocation;
    }
}

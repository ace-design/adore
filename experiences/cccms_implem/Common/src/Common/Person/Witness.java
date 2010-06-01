/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class Witness  extends Person{


    public Witness(){
        super();
    }

     public Witness(String name,String adress, String identification,String phone,String dateOfBirth){
        super(name,adress,identification,phone,dateOfBirth);
    }


     public String getName(){
      return super.name;
    }

    public String getAdresse(){
      return super.adress;
    }

    public String getIdentification(){
        return super.identification;
    }

    public String getPhone() {
        return super.phone;
    }

    public String getDateOfBirth(){
        return super.dateOfBirth;
    }

     public void getAdresse(String adresse){
        super.adress = adresse;
     }

     public void setName(String name){
         super.name = name;
     }

     public void setAdress(String adress) {
         super.adress = adress;
     }

     public void setIdentification(String identification) {
         super.identification = identification;
     }

     public void getPhone(String phone) {
         super.phone = phone;
     }

     public void setDateOfBirth(String dateOfBirth) {
         super.dateOfBirth = dateOfBirth;
     }


}

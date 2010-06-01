/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

import java.io.Serializable;

/**
 *
 * @author savadogo
 */
public abstract class Person implements Serializable{
    protected String name;
    protected String adress;
    protected String identification;
    protected String phone;
    protected String dateOfBirth;

    public Person(){
       this.name = "";
       this.adress = "";
       this.identification = "";
       this.phone = "";
       this.dateOfBirth = "";
    }

    public Person(String name,String adress, String identification,String phone,String dateOfBirth){
       this.name = name;
       this.adress = adress;
       this.identification = identification;
       this.phone = phone;
       this.dateOfBirth = dateOfBirth;
    }


    public String getName(){
      return name;
    }
    
    public String getAdresse(){
      return adress;
    }

    public String getIdentification(){
        return identification;
    }

    public String getPhone() {
        return phone;
    }

    public String getDateOfBirth(){
        return dateOfBirth;
    }

     public void getAdresse(String adresse){
        this.adress = adresse;
     }
     
     public void setName(String name){
        this.name = name;
     }

     public void setAdress(String adress) {
        this.adress = adress;
     }

     public void setIdentification(String identification) {
        this.identification = identification;
     }

     public void getPhone(String phone) {
        this.phone = phone;
     }

     public void setDateOfBirth(String dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
     }

}

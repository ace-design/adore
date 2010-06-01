/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class Worker extends Person  {


 public  String gsm;
 public String expertise;

 public Worker(){
     super();
 }


 public Worker(String name,String adress, String identification,String phone,String dateOfBirth,String gsm,String expertise){
     super(name,adress,identification,phone,dateOfBirth);
     this.gsm=gsm;
     this.expertise=expertise;
 }

 public String getGsm(){
     return gsm;
 }

 public String getExpertise(){
     return expertise;
 }
 
 public void setGsm(String gsm){
     this.gsm = gsm;
 }

 public void setExpertise(String expertise){
     this.expertise = expertise;
 }



}

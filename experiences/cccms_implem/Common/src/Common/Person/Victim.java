/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class Victim extends Person {

    private boolean deceased;
    private boolean concious;
    private String mobile;
    private String injury;


    public Victim(){
        super();
        this.mobile = "";
        this.injury="";
        
    }

   public Victim(String name,String adress, String identification,String phone,String dateOfBirth, String mobile,String injury){
        super(name,adress,identification,phone,dateOfBirth);
        this.mobile = mobile;
        this.injury=injury;
    }

   public String getMobile(){
       return mobile;
   }

   public String getInjury(){
       return injury;
   }

   public void setMobile( String mobile){
       this.mobile = mobile;
   }

   public void setInjury( String injury){
       this.injury = injury;
   }

   public boolean isDeceased(){
       return deceased;
   }

   public void setDeceased(boolean deceased){
    this.deceased=deceased;
   }

   public boolean isConcious(){
       return concious;
   }

    public void setConcious(boolean concious){
    this.concious=concious;
   }

    
}

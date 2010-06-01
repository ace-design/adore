/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class CmsEmployee extends Worker {

   protected String loginId;
   protected String password;
   protected String accessRights;
   protected String status;

   public CmsEmployee(){
       super();
       this.loginId="";
       this.password="";
       this.accessRights="";
       this.status="";
   }

   public CmsEmployee(String name,String adress, String identification,String phone,String dateOfBirth,String gsm,String expertise,String loginId,String password, String accessRights, String status){
       super(name,adress,identification,phone,dateOfBirth,gsm,expertise);
       this.loginId=loginId;
       this.password=password;
       this.accessRights=accessRights;
       this.status=status;
   }


   public String getLoginId(){
       return loginId;
   }

   public void setLoginId(String loginId){
       this.loginId=loginId;
   }

   public String getPassword(){
       return password;
   }

   public void setPassword(String password){
       this.password=password;
   }

   public String getAccessRights(){
        return  accessRights;
   }

   public void setAccessRights(String accessRights){
       this.accessRights=accessRights;
   }

   public String getStatus(){
       return status;
   }

   public void setStatus(String status){
       this.status=status;
   }


}

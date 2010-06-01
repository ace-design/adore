/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Person;

/**
 *
 * @author savadogo
 */
public class SysAdmin extends CmsEmployee{


    public SysAdmin(){
        super();
    }

    public SysAdmin(String name,String adress, String identification,String phone,String dateOfBirth,String gsm,String expertise,String loginId,String password, String accessRights, String status){
       super(name,adress,identification,phone,dateOfBirth,gsm,expertise,loginId,password,accessRights,status);

    }

}

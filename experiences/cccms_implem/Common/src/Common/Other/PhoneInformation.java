package Common.Other;

import java.io.Serializable;

/**
 * @date de création: 31 mai 2010 21:05:14
 * @auteur: Freddy Lallement
 */
public class PhoneInformation implements Serializable{

    private String phoneNumber;
    // ...

    
    /**
     * @return the phoneNumber
     */
    public String getPhoneNumber() {
        return phoneNumber;
    }

    /**
     * @param telNumber the phoneNumber to set
     */
    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }
}

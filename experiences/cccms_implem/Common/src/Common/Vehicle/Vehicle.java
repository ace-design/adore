/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Vehicle;

import java.io.Serializable;

/**
 *
 * @author lallemen
 */
public abstract class Vehicle implements Serializable{

    protected String licence;
    protected String serialNumber;
    protected String color;

    public Vehicle(){
        this.licence = "";
        this.serialNumber = "";
        this.color = "";
    }

    public Vehicle(String licence, String serialNumber, String color){
        this.licence = licence;
        this.serialNumber = serialNumber;
        this.color = color;
    }
    
    /**
     * @return the licence
     */
    public String getLicence() {
        return licence;
    }

    /**
     * @param licence the licence to set
     */
    public void setLicence(String licence) {
        this.licence = licence;
    }

    /**
     * @return the serialNumber
     */
    public String getSerialNumber() {
        return serialNumber;
    }

    /**
     * @param serialNumber the serialNumber to set
     */
    public void setSerialNumber(String serialNumber) {
        this.serialNumber = serialNumber;
    }

    /**
     * @return the Color
     */
    public String getColor() {
        return color;
    }

    /**
     * @param Color the Color to set
     */
    public void setColor(String Color) {
        this.color = Color;
    }
}

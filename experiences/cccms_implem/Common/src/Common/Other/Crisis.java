/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Common.Other;

import java.io.Serializable;


/**
 *
 * @author savadogo
 */

public class Crisis implements Serializable {

    public enum Status{
        BEGINNING,
        ONGOING,
        FINISHED
    }
    private int emergencyLevel;
    private String startTime;
    private String endTime;
    private Status status;
    private String detailedInfo;
    private CrisisType type;

    /**
     * @return the emergencyLevel
     */
    public int getEmergencyLevel() {
        return emergencyLevel;
    }

    /**
     * @param emergencyLevel the emergencyLevel to set
     */
    public void setEmergencyLevel(int emergencyLevel) {
        this.emergencyLevel = emergencyLevel;
    }

    /**
     * @return the startTime
     */
    public String getStartTime() {
        return startTime;
    }

    /**
     * @param startTime the startTime to set
     */
    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the endTime
     */
    public String getEndTime() {
        return endTime;
    }

    /**
     * @param endTime the endTime to set
     */
    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the status
     */
    public Status getStatus() {
        return status;
    }

    /**
     * @param status the status to set
     */
    public void setStatus(Status status) {
        this.status = status;
    }

    /**
     * @return the detailedInfo
     */
    public String getDetailedInfo() {
        return detailedInfo;
    }

    /**
     * @param detailedInfo the detailedInfo to set
     */
    public void setDetailedInfo(String detailedInfo) {
        this.detailedInfo = detailedInfo;
    }

    /**
     * @return the type
     */
    public CrisisType getType() {
        return type;
    }

    /**
     * @param type the type to set
     */
    public void setType(CrisisType type) {
        this.type = type;
    }
    
    
}

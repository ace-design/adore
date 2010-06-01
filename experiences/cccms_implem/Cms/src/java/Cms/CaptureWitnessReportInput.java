package Cms;

import Common.Other.CrisisIdentifier;
import Common.Person.CmsEmployee;
import java.io.Serializable;

/**
 * @date de création: 31 mai 2010 20:15:30
 * @auteur: Freddy Lallement
 */
public class CaptureWitnessReportInput implements Serializable{

    private CrisisIdentifier id;
    private CmsEmployee coord;
}

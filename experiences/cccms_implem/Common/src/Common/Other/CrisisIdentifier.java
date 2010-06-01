package Common.Other;

import java.io.Serializable;

/**
 * @date de création: 31 mai 2010 21:03:31
 * @auteur: Freddy Lallement
 */
public class CrisisIdentifier  implements Serializable{

    private long id;

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @param id the id to set
     */
    public void setId(long id) {
        this.id = id;
    }
}

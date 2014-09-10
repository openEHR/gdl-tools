package se.cambio.cds.model.study.dto;

import java.io.Serializable;
import java.util.Date;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 11:56
 */
public class StudyDTO implements Serializable{
    private String studyId;
    private String studySrc;
    private Date lastUpdate;

    public StudyDTO(String studyId, String studySrc, Date lastUpdate) {
        this.studyId = studyId;
        this.studySrc = studySrc;
        this.lastUpdate = lastUpdate;
    }

    public String getStudyId() {
        return studyId;
    }

    public void setStudyId(String studyId) {
        this.studyId = studyId;
    }

    public String getStudySrc() {
        return studySrc;
    }

    public void setStudySrc(String studySrc) {
        this.studySrc = studySrc;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}

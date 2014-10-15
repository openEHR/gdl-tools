package se.cambio.cds.model.study.dto;

import se.cambio.openehr.model.util.CMElement;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.Table;
import java.util.Date;

@Entity
@Table(name="cm_study")
public class StudyDTO implements CMElement{
    @Id
    private String id;
    @Lob
    private String source;
    private Date lastUpdate;

    public StudyDTO(String id, String source, Date lastUpdate) {
        this.id = id;
        this.source = source;
        this.lastUpdate = lastUpdate;
    }

    @Override
    public String getId() {
        return getId();
    }

    @Override
    public String getSource() {
        return getSource();
    }

    @Override
    public Date getLastUpdate() {
        return lastUpdate;
    }

    @Override
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}

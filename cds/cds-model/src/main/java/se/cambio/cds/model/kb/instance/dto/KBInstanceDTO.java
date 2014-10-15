package se.cambio.cds.model.kb.instance.dto;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name="caseStudy")
public class KBInstanceDTO {
    @Id
    private String kbInstanceId;
    @Lob
    private String kbInstanceSrc;
    private Date lastUpdate;

    public KBInstanceDTO(String kbInstanceId) {
        this.kbInstanceId = kbInstanceId;
    }

    public String getKbInstanceId() {
        return kbInstanceId;
    }

    public void setKbInstanceId(String kbInstanceId) {
        this.kbInstanceId = kbInstanceId;
    }

    public String getKbInstanceSrc() {
        return kbInstanceSrc;
    }

    public void setKbInstanceSrc(String kbInstanceSrc) {
        this.kbInstanceSrc = kbInstanceSrc;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}

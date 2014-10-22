package se.cambio.cm.model.kb.instance.dto;

import se.cambio.cm.model.util.CMElement;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name="cm_kb_instance")
public class KBInstanceDTO implements CMElement{
    @Id
    private String id;
    @Lob
    private String source;
    private Date lastUpdate;

    public KBInstanceDTO() { }

    @Override
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}

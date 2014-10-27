package se.cambio.cm.model.orderset.dto;

import se.cambio.cm.model.util.CMElement;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name="cm_orderset")
public class OrderSetDTO implements CMElement{
    @Id
    private String id;
    @Column(columnDefinition="TEXT")
    private String source;
    private Date lastUpdate;

    public OrderSetDTO() {}

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Date getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
    }
}

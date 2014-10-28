package se.cambio.cm.model.app.dto;

import se.cambio.cm.model.util.CMElement;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name="cm_cds_app")
public class CDSAppDTO implements CMElement {
    private static final long serialVersionUID = 2012054542L;
    @Id
    private String id;
    @Column(columnDefinition="TEXT")
    private String source;
    private Date lastUpdate;

    public CDSAppDTO() {}

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
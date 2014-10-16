package se.cambio.cds.model.app.dto;

import se.cambio.openehr.model.util.CMElement;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.Table;
import java.util.Date;

@Entity
@Table(name="cm_cds_app")
public class CDSAppDTO implements CMElement {
    private static final long serialVersionUID = 2012054542L;
    @Id
    private String id;
    @Lob
    private String source;
    private Date lastUpdate;

    public CDSAppDTO() {}

    @Override
    public String getId() {
        return getId();
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getSource() {
        return getSource();
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
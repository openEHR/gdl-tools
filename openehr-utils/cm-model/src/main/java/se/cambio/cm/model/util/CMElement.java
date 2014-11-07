package se.cambio.cm.model.util;

import java.io.Serializable;
import java.util.Date;

public interface CMElement extends Serializable{
    public String getId();
    public String getFormat();
    public String getSource();
    public Date getLastUpdate();
    public void setId(String id);
    public void setFormat(String format);
    public void setSource(String source);
    public void setLastUpdate(Date lastUpdate);
}

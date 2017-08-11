package se.cambio.cm.model.util;

import java.io.Serializable;
import java.util.Date;

public interface CMElement extends Serializable {
    String getId();

    String getFormat();

    String getSource();

    Date getLastUpdate();

    void setId(String id);

    void setFormat(String format);

    void setSource(String source);

    void setLastUpdate(Date lastUpdate);
}

package se.cambio.cm.model.cm.element.dao;

import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Date;

public class CMElementBuilder<E extends CMElement> {

    private String id;
    private String format;
    private String source;
    private Date lastUpdate;

    public CMElementBuilder setId(String id) {
        this.id = id;
        return this;
    }

    public CMElementBuilder setFormat(String format) {
        this.format = format;
        return this;
    }


    public CMElementBuilder setSource(String source) {
        this.source = source;
        return this;
    }

    public CMElementBuilder setLastUpdate(Date lastUpdate) {
        this.lastUpdate = lastUpdate;
        return this;
    }

    public E createCMElement(Class<E> cmElementClass) throws InternalErrorException {
        try {
            checkMissingAttributes();
            E cmElement = cmElementClass.newInstance();
            cmElement.setId(id);
            cmElement.setFormat(format);
            cmElement.setSource(source);
            cmElement.setLastUpdate(lastUpdate);
            return cmElement;
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    private void checkMissingAttributes() throws InternalErrorException {
        if (id == null){
            throw new InternalErrorException(new Exception("No id specified for cmElement"));
        }
        if (format == null){
            throw new InternalErrorException(new Exception("No format specified for cmElement"));
        }
        if (source == null){
            throw new InternalErrorException(new Exception("No source specified for cmElement"));
        }
        if (lastUpdate == null){
            throw new InternalErrorException(new Exception("No lastUpdate specified for cmElement"));
        }
    }
}

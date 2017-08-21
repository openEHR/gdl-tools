package se.cambio.cm.model.util;

import java.util.Collection;

public class CMType {
    private final String id;
    private Class<? extends CMElement> cmElementClass;
    private Collection<String> fileExtensions;

    public CMType(String id, Class<? extends CMElement> cmElementClass, Collection<String> fileExtensions) {
        this.id = id;
        this.cmElementClass = cmElementClass;
        this.fileExtensions = fileExtensions;
    }

    public Class<? extends CMElement> getCmElementClass() {
        return cmElementClass;
    }

    public Collection<String> getFileExtensions() {
        return fileExtensions;
    }

    public String getId() {
        return id;
    }
}

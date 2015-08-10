package se.cambio.openehr.util;

import java.io.File;

public class CmFolder {
    File folder;

    public CmFolder(File folder) {
        this.folder = folder;
    }

    public File getFolder() {
        return folder;
    }

    public void setFolder(File folder) {
        this.folder = folder;
    }
}

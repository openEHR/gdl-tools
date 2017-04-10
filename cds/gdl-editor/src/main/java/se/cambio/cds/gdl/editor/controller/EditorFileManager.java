package se.cambio.cds.gdl.editor.controller;

import se.cambio.openehr.util.UserConfigurationManager;

import java.io.File;

public class EditorFileManager {
    private File lastFolderLoaded = null;
    private File lastFileLoaded = null;
    private UserConfigurationManager userConfigurationManager;

    public EditorFileManager(UserConfigurationManager userConfigurationManager) {
        this.userConfigurationManager = userConfigurationManager;
    }

    public void setLastFolderLoaded(File lastFolderLoaded) {
        this.lastFolderLoaded = lastFolderLoaded;
    }

    public File getLastFileLoaded() {
        return lastFileLoaded;
    }

    public void setLastFileLoaded(File lastFileLoaded) {
        this.lastFileLoaded = lastFileLoaded;
    }


    public File getLastFolderLoaded() {
        if (lastFolderLoaded == null) {
            lastFolderLoaded = userConfigurationManager.getGuidesFolder().getFolder();
        }
        return lastFolderLoaded;
    }
}

package se.cambio.cds.gdl.editor.controller;

import org.springframework.context.ApplicationContext;
import se.cambio.cds.gdl.editor.controller.sw.LoadGuideFromFileRSW;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.File;

public class GuidelineLoadManager {

    private WindowManager windowManager;
    private final EditorManager editorManager;
    private EditorFileManager editorFileManager;
    private GuidelineEditorManager guidelineEditorManager;
    private ApplicationContext applicationContext;


    public GuidelineLoadManager(
            WindowManager windowManager,
            EditorManager editorManager,
            EditorFileManager editorFileManager,
            GuidelineEditorManager guidelineEditorManager,
            ApplicationContext applicationContext) {
        this.windowManager = windowManager;
        this.editorManager = editorManager;
        this.editorFileManager = editorFileManager;
        this.guidelineEditorManager = guidelineEditorManager;
        this.applicationContext = applicationContext;
    }

    public void loadGuide(File guide) {
        MainMenuBar mainMenuBar = applicationContext.getBean(MainMenuBar.class);
        GdlEditorFactory gdlEditorFactory = applicationContext.getBean(GdlEditorFactory.class);
        new LoadGuideFromFileRSW(
                windowManager,
                editorManager,
                editorFileManager,
                guidelineEditorManager,
                gdlEditorFactory,
                mainMenuBar,
                guide).execute();
    }

    public void showLoadDialog() {
        JFileChooser fileChooser = new JFileChooser(editorFileManager.getLastFolderLoaded());
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                GDLEditorLanguageManager.getMessage("Guide"), "gdl");
        fileChooser.setDialogTitle(GDLEditorLanguageManager.getMessage("LoadGuide"));
        fileChooser.setFileFilter(filter);
        int result = fileChooser.showOpenDialog(windowManager.getMainWindow());
        if (result != JFileChooser.CANCEL_OPTION) {
            File guide = fileChooser.getSelectedFile();
            windowManager.setBusy(GDLEditorLanguageManager.getMessage("Loading") + "...");
            loadGuide(guide);
        }
    }
}

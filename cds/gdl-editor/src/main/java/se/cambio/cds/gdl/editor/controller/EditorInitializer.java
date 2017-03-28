package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.gdl.editor.view.dialog.DialogEditor;
import se.cambio.cds.gdl.editor.view.frame.EditorFrame;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;

import java.awt.*;

public class EditorInitializer {

    private EditorManager editorManager;
    private MainMenuBar mainMenuBar;

    public EditorInitializer(EditorManager editorManager, MainMenuBar mainMenuBar) {
        this.editorManager = editorManager;
        this.mainMenuBar = mainMenuBar;
    }

    public EditorFrame createEditorFrame() {
        EditorFrame ef = new EditorFrame(editorManager, mainMenuBar);
        editorManager.setEditorViewer(ef);
        editorManager.setExitOnClose(true);
        return ef;
    }


    public DialogEditor createEditorDialog(Window owner) {
        DialogEditor ed = new DialogEditor(owner, editorManager, mainMenuBar);
        editorManager.setEditorViewer(ed);
        editorManager.setExitOnClose(false);
        return ed;
    }




}

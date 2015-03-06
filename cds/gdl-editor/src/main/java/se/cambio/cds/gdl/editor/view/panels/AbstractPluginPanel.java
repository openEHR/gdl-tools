package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.view.swing.panel.interfaces.PluginPanelI;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;

/**
 * User: Iago.Corbal
 * Date: 2014-07-02
 * Time: 11:15
 */
public abstract class AbstractPluginPanel extends JPanel implements PluginPanelI, RefreshablePanel {

    private GDLEditor gdlEditor;

    public AbstractPluginPanel() {

    }

    public void setGdlEditor(GDLEditor gdlEditor) {
        this.gdlEditor = gdlEditor;
    }

    public Guide getGuide() {
        return gdlEditor.getEntity();
    }

    public String getLanguage() {
        return gdlEditor.getCurrentLanguageCode();
    }
}

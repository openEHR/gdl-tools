package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.controller.guide.GuideExportPlugin;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;

public class SourcePanel extends JPanel implements RefreshablePanel {

    private static final long serialVersionUID = 1L;
    private JScrollPane mainScrollPanel;
    private GuideExportPlugin guideExportPlugin;
    private GDLEditor gdlEditor;

    SourcePanel(GuideExportPlugin guideExportPlugin, GDLEditor gdlEditor) {
        this.guideExportPlugin = guideExportPlugin;
        this.gdlEditor = gdlEditor;
        init();
    }

    public void init() {
        this.setLayout(new BorderLayout());
        this.add(getMainScrollPanel());
    }

    private JScrollPane getMainScrollPanel() {
        if (mainScrollPanel == null) {
            mainScrollPanel = new JScrollPane();
        }
        return mainScrollPanel;
    }

    private JTextArea buildTextArea() {
        JTextArea textArea = new JTextArea();
        if (gdlEditor != null) {
            String source = guideExportPlugin.getSource(gdlEditor.getEntity());
            textArea.setText(source);
        }
        textArea.setEditable(false);
        return textArea;
    }

    @Override
    public void refresh() {
        getMainScrollPanel().setViewportView(buildTextArea());
    }
}
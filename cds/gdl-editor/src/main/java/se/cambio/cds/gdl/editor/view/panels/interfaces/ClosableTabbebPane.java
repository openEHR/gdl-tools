package se.cambio.cds.gdl.editor.view.panels.interfaces;

import javax.swing.JTabbedPane;

public interface ClosableTabbebPane {
    public JTabbedPane getTabbedPane();
    public void deleteTab(int index);
}

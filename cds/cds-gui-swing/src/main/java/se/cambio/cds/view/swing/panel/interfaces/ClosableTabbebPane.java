package se.cambio.cds.view.swing.panel.interfaces;

import javax.swing.JTabbedPane;

public interface ClosableTabbebPane {
    public JTabbedPane getTabbedPane();
    public void deleteTab(int index);
}

package se.cambio.cds.view.swing.panel.interfaces;

import javax.swing.JTabbedPane;

public interface ClosableTabbebPane {
    JTabbedPane getTabbedPane();

    void deleteTab(int index);
}

package se.cambio.cds.view.swing.panel;

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

    private Guide _guide;
    private String _language;

    public void setGuide(Guide guide){
        _guide = guide;
    }

    public Guide getGuide() {
        return _guide;
    }

    public void setLanguage(String language){
        _language = language;
    }

    public String getLanguage() {
        return _language;
    }
}

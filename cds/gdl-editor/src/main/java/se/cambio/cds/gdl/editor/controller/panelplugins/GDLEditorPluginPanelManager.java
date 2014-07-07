package se.cambio.cds.gdl.editor.controller.panelplugins;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.editor.util.GDLEditorConfigurationParametersManager;
import se.cambio.cds.view.swing.panel.AbstractPluginPanel;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-07-02
 * Time: 11:22
 */
public class GDLEditorPluginPanelManager {
    private static GDLEditorPluginPanelManager _instance;
    private Map<String, AbstractPluginPanel> pluginPanelMap = null;

    private GDLEditorPluginPanelManager(){
        String gdlEditorPlugins = null;
        pluginPanelMap = new HashMap<String, AbstractPluginPanel>();
        try {
            gdlEditorPlugins = GDLEditorConfigurationParametersManager.getParameter(GDLEditorConfigurationParametersManager.GDL_PLUGINS_KEY);
        } catch (MissingConfigurationParameterException e) {
            Logger.getLogger(GDLEditorPluginPanelManager.class).info("No gdl editor plugins found.");
        }
        if (gdlEditorPlugins!=null && !gdlEditorPlugins.trim().isEmpty()){
            try{
                String[] gdlEditorPluginsClasses = gdlEditorPlugins.split(",");
                for(String gdlEditorPluginClass: gdlEditorPluginsClasses){
                    gdlEditorPluginClass = gdlEditorPluginClass.trim();
                    Class pluginClass = Class.forName(gdlEditorPluginClass);
                    AbstractPluginPanel abstractPluginPanel = (AbstractPluginPanel)pluginClass.newInstance();
                    pluginPanelMap.put(gdlEditorPluginClass, abstractPluginPanel);
                }
            }catch(Exception e){
                ExceptionHandler.handle(e);
            }
        }
    }

    public static Collection<AbstractPluginPanel> getPluginPanels(){
        return getDelegate().pluginPanelMap.values();
    }

    public static GDLEditorPluginPanelManager getDelegate(){
        if (_instance == null) {
            _instance = new GDLEditorPluginPanelManager();
        }
        return _instance;
    }
}

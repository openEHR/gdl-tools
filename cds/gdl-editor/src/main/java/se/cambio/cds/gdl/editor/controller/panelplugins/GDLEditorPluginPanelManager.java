package se.cambio.cds.gdl.editor.controller.panelplugins;

import se.cambio.cds.gdl.editor.util.GDLEditorConfig;
import se.cambio.cds.gdl.editor.view.panels.AbstractPluginPanel;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.ExceptionHandler;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class GDLEditorPluginPanelManager {
    private static GDLEditorPluginPanelManager _instance;
    private Map<String, AbstractPluginPanel> pluginPanelMap = null;

    private GDLEditorPluginPanelManager() {
        GDLEditorConfig gdlEditorConfig = BeanProvider.getBean(GDLEditorConfig.class);
        String gdlEditorPlugins = gdlEditorConfig.getGdlEditorPlugins();
        pluginPanelMap = new HashMap<>();
        if (gdlEditorPlugins != null && !gdlEditorPlugins.trim().isEmpty()) {
            try {
                String[] gdlEditorPluginsClasses = gdlEditorPlugins.split(",");
                for (String gdlEditorPluginClass : gdlEditorPluginsClasses) {
                    gdlEditorPluginClass = gdlEditorPluginClass.trim();
                    Class pluginClass = Class.forName(gdlEditorPluginClass);
                    AbstractPluginPanel abstractPluginPanel = (AbstractPluginPanel) pluginClass.newInstance();
                    pluginPanelMap.put(gdlEditorPluginClass, abstractPluginPanel);
                }
            } catch (Exception e) {
                ExceptionHandler.handle(e);
            }
        }
    }

    public static Collection<AbstractPluginPanel> getPluginPanels() {
        return getDelegate().pluginPanelMap.values();
    }

    private static GDLEditorPluginPanelManager getDelegate() {
        if (_instance == null) {
            _instance = new GDLEditorPluginPanelManager();
        }
        return _instance;
    }
}

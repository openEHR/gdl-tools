package se.cambio.cds.gdl.editor.configuration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.*;
import org.springframework.core.env.Environment;
import se.cambio.cds.configuration.CdsCoreConfiguration;
import se.cambio.cds.configuration.DroolsConfiguration;
import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.controller.guide.GuideExportPlugin;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.converters.drools.DroolsGuideExportPlugin;
import se.cambio.cds.gdl.editor.controller.*;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.cds.gdl.graph.configuration.GdlGraphConfiguration;
import se.cambio.cds.gdl.graph.view.panel.GdlGraphManager;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GuideImporter;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.cds.view.swing.DvSwingManager;
import se.cambio.cds.view.swing.configuration.CdsGuiSwingConfiguration;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.openehr.configuration.OpenEhrSwingConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.TerminologyDialogManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.ImportManager;
import se.cambio.openehr.view.util.WindowManager;

@Configuration
@PropertySources({
        @PropertySource(value = "classpath:default-gdl-editor-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:conf/gdl-editor-config.properties", ignoreResourceNotFound = true)
})
@Import({DroolsConfiguration.class, CdsCoreConfiguration.class, CdsGuiSwingConfiguration.class, GdlGraphConfiguration.class})
public class GdlEditorConfiguration {

    private static final String GDL_PLUGINS_KEY = "gdl-editor.plugins";

    private Environment environment;

    @Autowired
    public GdlEditorConfiguration(Environment environment) {
        this.environment = environment;
    }

    @Bean
    GdlEditorFactory gdlEditorFactory(
            WindowManager windowManager,
            ArchetypeManager archetypeManager,
            TerminologyService terminologyService,
            GuideImporter guideImporter,
            ElementInstanceCollectionManager elementInstanceCollectionManager,
            DvSwingManager dvSwingManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ImportManager importManager,
            TerminologyDialogManager terminologyDialogManager,
            GuideExportPluginDirectory guideExportPluginDirectory,
            GuidelineEditorManager guidelineEditorManager,
            GuideHTMLExporter guideHTMLExporter,
            EditorFileManager editorFileManager,
            DVPanelFactory dvPanelFactory,
            GuidelineLoadManager guidelineLoadManager,
            GdlGraphManager gdlGraphManager,
            GuideExportPlugin guideExportPlugin,
            CdsDataManager cdsDataManager,
            RuleEngineService ruleEngineService) {
        return new GdlEditorFactory(
                windowManager,
                archetypeManager,
                terminologyService,
                guideImporter,
                elementInstanceCollectionManager,
                dvSwingManager,
                archetypeReferencesManager,
                importManager,
                terminologyDialogManager,
                guideExportPluginDirectory,
                guidelineEditorManager,
                guideHTMLExporter,
                editorFileManager,
                dvPanelFactory,
                guidelineLoadManager,
                gdlGraphManager,
                guideExportPlugin,
                cdsDataManager,
                ruleEngineService);
    }

    @Bean
    MainMenuBar mainMenuBar(
            ImportManager importManager,
            EditorManager editorManager,
            GuideHTMLExporter guideHTMLExporter,
            GdlEditorFactory gdlEditorFactory,
            EditorFileManager editorFileManager,
            GuidelineLoadManager guidelineLoadManager,
            UserConfigurationManager userConfigurationManager) {
        return new MainMenuBar(
                importManager,
                guideHTMLExporter,
                editorManager,
                gdlEditorFactory,
                editorFileManager,
                guidelineLoadManager,
                userConfigurationManager);
    }

    @Bean
    EditorManager editorManager(WindowManager windowManager) {
        return new EditorManager(windowManager);
    }

    @Bean
    GuidelineEditorManager guidelineEditorManager(WindowManager windowManager) {
        return new GuidelineEditorManager(windowManager);
    }

    @Bean
    EditorInitializer editorInitializer(EditorManager editorManager, MainMenuBar mainMenuBar) {
        return new EditorInitializer(editorManager, mainMenuBar);
    }

    @Bean
    GuideExportPluginDirectory guideExportPluginDirectory(DroolsGuideExportPlugin droolsGuideExportPlugin) {
        return new GuideExportPluginDirectory(droolsGuideExportPlugin);
    }

    @Bean
    EditorFileManager editorFileManager(UserConfigurationManager userConfigurationManager) {
        return new EditorFileManager(userConfigurationManager);
    }

    @Bean
    GuidelineLoadManager guidelineLoadManager(
            WindowManager windowManager,
            EditorManager editorManager,
            EditorFileManager editorFileManager,
            GuidelineEditorManager guidelineEditorManager,
            ApplicationContext applicationContext) {
        return new GuidelineLoadManager(
                windowManager,
                editorManager,
                editorFileManager,
                guidelineEditorManager,
                applicationContext);
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */
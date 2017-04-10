package se.cambio.cds.gdl.editor.controller;

import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.controller.guide.GuideExportPlugin;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.graph.view.panel.GdlGraphManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GuideImporter;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.cds.view.swing.DvSwingManager;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.TerminologyDialogManager;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.ImportManager;
import se.cambio.openehr.view.util.WindowManager;

public class GdlEditorFactory {

    private WindowManager windowManager;
    private final ArchetypeManager archetypeManager;
    private final TerminologyService terminologyService;
    private final GuideImporter guideImporter;
    private final ElementInstanceCollectionManager elementInstanceCollectionManager;
    private final DvSwingManager dvSwingManager;
    private final ArchetypeReferencesManager archetypeReferencesManager;
    private final ImportManager importManager;
    private final GuideExportPluginDirectory guideExportPluginDirectory;
    private GuidelineEditorManager guidelineEditorManager;
    private TerminologyDialogManager terminologyDialogManager;
    private GuideHTMLExporter guideHTMLExporter;
    private EditorFileManager editorFileManager;
    private DVPanelFactory dbPanelFactory;
    private GuidelineLoadManager guidelineLoadManager;
    private GdlGraphManager gdlGraphManager;
    private GuideExportPlugin guideExportPlugin;
    private final CdsDataManager cdsDataManager;
    private final RuleEngineService ruleEngineService;

    public GdlEditorFactory(
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
            DVPanelFactory dbPanelFactory,
            GuidelineLoadManager guidelineLoadManager,
            GdlGraphManager gdlGraphManager,
            GuideExportPlugin guideExportPlugin,
            CdsDataManager cdsDataManager,
            RuleEngineService ruleEngineService) {
        this.windowManager = windowManager;
        this.archetypeManager = archetypeManager;
        this.terminologyService = terminologyService;
        this.guideImporter = guideImporter;
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
        this.dvSwingManager = dvSwingManager;
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.importManager = importManager;
        this.terminologyDialogManager = terminologyDialogManager;
        this.editorFileManager = editorFileManager;
        this.guideExportPluginDirectory = guideExportPluginDirectory;
        this.guidelineEditorManager = guidelineEditorManager;
        this.guideHTMLExporter = guideHTMLExporter;
        this.dbPanelFactory = dbPanelFactory;
        this.guidelineLoadManager = guidelineLoadManager;
        this.gdlGraphManager = gdlGraphManager;
        this.guideExportPlugin = guideExportPlugin;
        this.cdsDataManager = cdsDataManager;
        this.ruleEngineService = ruleEngineService;
    }

    public GDLEditor createGdlEditor(Guide guide, EditorViewer editorViewer) {
        GDLEditor gdlEditor = new GDLEditor(
                windowManager,
                archetypeManager,
                terminologyService,
                guideImporter,
                elementInstanceCollectionManager,
                dvSwingManager,
                archetypeReferencesManager,
                importManager,
                terminologyDialogManager,
                editorViewer,
                editorFileManager,
                guideExportPluginDirectory,
                guidelineEditorManager,
                guidelineLoadManager,
                guideHTMLExporter,
                dbPanelFactory,
                gdlGraphManager,
                guideExportPlugin,
                cdsDataManager,
                ruleEngineService);
        gdlEditor.setGuideline(guide);
        return gdlEditor;
    }

}

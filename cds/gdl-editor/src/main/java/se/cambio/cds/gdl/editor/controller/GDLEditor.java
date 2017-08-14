package se.cambio.cds.gdl.editor.controller;

import org.apache.commons.lang.SerializationUtils;
import org.apache.commons.lang.StringUtils;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.controller.guide.GuideExportPlugin;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.formgen.view.dialog.CDSFormGenDialog;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.editor.controller.sw.CheckForChangesOnGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.CheckGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.CompileGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.SaveGuideOnFileRSW;
import se.cambio.cds.gdl.editor.util.DefinitionDependencyChecker;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogNameInsert;
import se.cambio.cds.gdl.editor.view.panels.GDLEditorMainPanel;
import se.cambio.cds.gdl.editor.view.panels.GDLPanel;
import se.cambio.cds.gdl.graph.view.panel.GdlGraphManager;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GuideImporter;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.cds.view.swing.DvSwingManager;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.guide.dto.GuideDTOBuilder;
import se.cambio.cm.model.util.CMTypeFormat;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.TerminologyDialogManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice.MessageType;
import se.cambio.openehr.view.dialogs.InfoDialog;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.ImportManager;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.util.*;
import java.util.List;

public class GDLEditor implements EditorController<Guide> {

    private static String UNKNOWN_GUIDE_ID = "unknown";
    private static String GT_HEADER = "gt";
    public static Collection<String> SUPPORTED_EXTENSIONS = Collections.singleton("gdl");
    private GDLEditorMainPanel gdlEditorMainPanel = null;
    private ResourceDescription resourceDescription = null;
    private GuideOntology guideOntology = null;
    private ReadableGuide readableGuide = null;
    private ReadableRule ruleAtEdit = null;
    private Map<String, TermDefinition> termDefinitions = null;
    private Map<String, TermBinding> termBindings = null;
    private Language language;
    private Map<String, String> originalAuthor;
    private List<String> otherContributors;
    private Map<String, ResourceDescriptionItem> details;
    private Map<String, String> otherDetails;
    private InfoDialog infoDialog;
    private String guideId = UNKNOWN_GUIDE_ID;
    private String conceptGTCode;
    private String currentGuideLanguageCode = null;
    private String originalGuide = null;
    private RefreshablePanel lastRefreshedPanel = null;
    private Logger logger = LoggerFactory.getLogger(GDLEditor.class);
    private WindowManager windowManager;
    private ArchetypeManager archetypeManager;
    private TerminologyService terminologyService;
    private final GuideImporter guideImporter;
    private final ElementInstanceCollectionManager elementInstanceCollectionManager;
    private final DvSwingManager dvSwingManager;
    private ArchetypeReferencesManager archetypeReferencesManager;
    private ImportManager importManager;
    private RuleElementEditor ruleElementEditor;
    private TerminologyDialogManager terminologyDialogManager;
    private EditorViewer editorViewer;
    private EditorFileManager editorFileManager;
    private GuideExportPluginDirectory guideExportPluginDirectory;
    private GuidelineEditorManager guidelineEditorManager;
    private RuleLineCloner ruleLineCloner;
    private GuideHTMLExporter guideHTMLExporter;
    private DVPanelFactory dbPanelFactory;
    private GdlGraphManager gdlGraphManager;
    private GuidelineLoadManager guidelineLoadManager;
    private Boolean active;
    private GuideExportPlugin guideExportPlugin;
    private final CdsDataManager cdsDataManager;
    private final RuleEngineService ruleEngineService;

    GDLEditor(
            WindowManager windowManager,
            ArchetypeManager archetypeManager,
            TerminologyService terminologyService,
            GuideImporter guideImporter,
            ElementInstanceCollectionManager elementInstanceCollectionManager,
            DvSwingManager dvSwingManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ImportManager importManager,
            TerminologyDialogManager terminologyDialogManager,
            EditorViewer editorViewer,
            EditorFileManager editorFileManager,
            GuideExportPluginDirectory guideExportPluginDirectory,
            GuidelineEditorManager guidelineEditorManager,
            GuidelineLoadManager guidelineLoadManager,
            GuideHTMLExporter guideHTMLExporter,
            DVPanelFactory dbPanelFactory,
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
        this.editorViewer = editorViewer;
        this.editorFileManager = editorFileManager;
        this.guideExportPluginDirectory = guideExportPluginDirectory;
        this.guidelineEditorManager = guidelineEditorManager;
        this.guidelineLoadManager = guidelineLoadManager;
        this.guideHTMLExporter = guideHTMLExporter;
        this.dbPanelFactory = dbPanelFactory;
        this.gdlGraphManager = gdlGraphManager;
        this.guideExportPlugin = guideExportPlugin;
        this.cdsDataManager = cdsDataManager;
        this.ruleEngineService = ruleEngineService;
        this.ruleLineCloner = new RuleLineCloner(this);
        this.active = true;
    }

    void setGuideline(Guide guide) {
        if (guide == null) {
            guide = new Guide();
        }
        setEntity(guide);
    }


    public void init() {
        editorViewer.setContent(getEditorPanel());
        new CheckForChangesOnGuideSW(this).execute();
    }

    public String getTitle() {
        return getAppName() + " - " + getConceptName();
    }

    private String getConceptName() {
        String conceptName = getGTName(getConceptGTCode());
        if (conceptName == null) {
            conceptName = GDLEditorLanguageManager.getMessage("Guide");
        }
        return conceptName;
    }

    private String getAppName() {
        return GDLEditorLanguageManager.getMessage("GDLEditor");
    }

    public boolean checkRuleLineDelete(RuleLine ruleLine) {
        if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
            return checkArchetypeReferenceRemoval((ArchetypeInstantiationRuleLine) ruleLine);
        } else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
            ArchetypeElementInstantiationRuleLine aeirl = (ArchetypeElementInstantiationRuleLine) ruleLine;
            return checkArchetypeReferenceRemoval(aeirl);
        } else {
            return true;
        }
    }

    private boolean checkArchetypeReferenceRemoval(ArchetypeInstantiationRuleLine airl) {
        if (DefinitionDependencyChecker.isBeingUsed(airl, this)) {
            JOptionPane.showMessageDialog(
                    windowManager.getMainWindow(),
                    GDLEditorLanguageManager.getMessage("ReferenceBeingUsedMsg"),
                    GDLEditorLanguageManager.getMessage("ReferenceBeingUsedTitle"),
                    JOptionPane.WARNING_MESSAGE);
            return false;
        } else {
            return true;
        }
    }

    private boolean checkArchetypeReferenceRemoval(
            ArchetypeElementInstantiationRuleLine aeirl) {
        if (DefinitionDependencyChecker.isBeingUsed(aeirl, this)) {
            JOptionPane.showMessageDialog(
                    windowManager.getMainWindow(),
                    GDLEditorLanguageManager.getMessage("ReferenceBeingUsedMsg"),
                    GDLEditorLanguageManager.getMessage("ReferenceBeingUsedTitle"),
                    JOptionPane.WARNING_MESSAGE);
            return false;
        } else {
            return true;
        }
    }

    public GDLEditorMainPanel getEditorPanel() {
        if (gdlEditorMainPanel == null) {
            gdlEditorMainPanel = new GDLEditorMainPanel(this, guidelineLoadManager, guideHTMLExporter, gdlGraphManager, guideExportPlugin);
        }
        return gdlEditorMainPanel;
    }

    public void saveAs() {
        gdlEditorMainPanel.requestFocus();
        runIfOkWithEditorState(() -> new SaveGuideOnFileRSW(windowManager, null, this, editorFileManager).execute());
    }

    @Override
    public void entitySaved() {
        updateOriginal();
        getEditorPanel().getSaveButton().setEnabled(false);
    }

    public void save() {
        gdlEditorMainPanel.requestFocus();
        runIfOkWithEditorState(() -> new SaveGuideOnFileRSW(windowManager, editorFileManager.getLastFileLoaded(), this, editorFileManager).execute());
    }

    public void generateForm() {
        gdlEditorMainPanel.requestFocus();
        Runnable runnable = () -> {
            if (!hasActiveRules()) {
                JOptionPane.showMessageDialog(windowManager.getMainWindow(), GDLEditorLanguageManager.getMessage("PleaseInsertRulesBeforeGeneratingForm"), GDLEditorLanguageManager.getMessage("GenerateForm"), JOptionPane.WARNING_MESSAGE);
                return;
            }
            String gdlGuide = getSerializedEntity();
            if (gdlGuide != null) {
                CompileGuideSW sw = new CompileGuideSW(this) {
                    protected void done() {
                        getController().compilationFinished(getErrorMsg());
                        if (getErrorMsg() == null) {
                            generateDialogForm(getCompiledGuide(), getGuide());
                        }
                    }
                };
                sw.execute();
                setBusy(GDLEditorLanguageManager.getMessage("Compiling") + "...");
            }
        };
        runIfOkWithEditorState(runnable);
    }

    private void generateDialogForm(byte[] compiledGuide, Guide guide) throws InternalErrorException {
        String gdlGuide = getSerializedEntity();
        if (compiledGuide != null && gdlGuide != null) {
            GuideDTO guideDTO =
                    new GuideDTOBuilder()
                            .setId(getEntityId())
                            .setFormat(CMTypeFormat.GDL_FORMAT.getFormat())
                            .setSource(gdlGuide)
                            .setGuideObject(SerializationUtils.serialize(guide))
                            .setCompiledGuide(compiledGuide)
                            .setLastUpdate(Calendar.getInstance().getTime())
                            .createGuideDTO();
            FormGeneratorController formGenerator =
                    new FormGeneratorController(
                            guideDTO, getCurrentLanguageCode(),
                            guideImporter, elementInstanceCollectionManager,
                            archetypeManager, dvSwingManager,
                            cdsDataManager,
                            ruleEngineService);
            Date date = archetypeManager.getUserConfigurationManager().getCurrentDateTime();
            if (date != null) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(date);
                formGenerator.setCurrentDate(cal);
            }
            CDSFormGenDialog dialog = new CDSFormGenDialog(windowManager.getMainWindow());
            dialog.setFormGeneratorController(formGenerator);
            dialog.setVisible(true);
        }
    }

    public void compilationFinished(String msg) {
        setFree();
        if (msg != null) {
            JOptionPane.showMessageDialog(
                    windowManager.getMainWindow(), msg,
                    GDLEditorLanguageManager.getMessage("Error"),
                    JOptionPane.ERROR_MESSAGE);
        }
    }

    public ReadableRule createNewRule() {
        ruleAtEdit = null;
        DialogNameInsert dialog = new DialogNameInsert(
                windowManager.getMainWindow(),
                GDLEditorLanguageManager.getMessage("RuleName"), "");
        if (dialog.getAnswer()) {
            String nextGtCode = createNextLocalCode();
            ruleAtEdit = new ReadableRule(getCurrentTermDefinition(), nextGtCode, readableGuide);
            setGTName(nextGtCode, dialog.getValue());
            getRenderableRules().put(ruleAtEdit.getGTCode(), ruleAtEdit);
        }
        return ruleAtEdit;
    }

    public void ruleEdit(ReadableRule rule) {
        ruleAtEdit = rule;
        getEditorPanel().loadRuleView(rule);
    }

    public LinkedHashMap<String, ReadableRule> getRenderableRules() {
        return getReadableGuide().getReadableRules();
    }

    private boolean hasActiveRules() {
        boolean hasActiveRules = false;
        Iterator<ReadableRule> i = getRenderableRules().values().iterator();
        while (i.hasNext() && !hasActiveRules) {
            hasActiveRules = !i.next().isCommented();
        }
        return hasActiveRules;
    }

    public void changeCommentRule(ReadableRule rule, boolean comment) {
        rule.setCommented(comment);
    }

    public void goBackToGuide() {
        getEditorPanel().loadGuideView();
    }

    public ResourceDescription getResourceDescription() {
        if (resourceDescription == null) {
            resourceDescription = new ResourceDescription();
            String DRAFT = "Author draft";
            resourceDescription.setLifecycleState(DRAFT);
            initResourceDescription();
        }
        return resourceDescription;
    }

    private void initResourceDescription() {
        getOriginalAuthor();
        getOtherContributors();
        getResourceDescriptionItem(getCurrentLanguageCode());
        getOtherDetails();
        getKeywords();
    }

    private Map<String, String> getOriginalAuthor() {
        if (originalAuthor == null) {
            originalAuthor = new HashMap<>();
            getResourceDescription().setOriginalAuthor(originalAuthor);
        }
        return originalAuthor;
    }

    private List<String> getOtherContributors() {
        if (otherContributors == null) {
            otherContributors = new ArrayList<>();
            getResourceDescription().setOtherContributors(otherContributors);
        }
        return otherContributors;
    }

    public Map<String, ResourceDescriptionItem> getDetails() {
        if (details == null) {
            details = new HashMap<>();
            getResourceDescription().setDetails(details);
        }
        return details;
    }

    private ResourceDescriptionItem getResourceDescriptionItem(String lang) {
        ResourceDescriptionItem resourceDescriptionItem = getDetails().get(lang);
        if (resourceDescriptionItem == null) {
            resourceDescriptionItem = new ResourceDescriptionItem();
            if (!getOriginalLanguageCode().equals(lang)) {
                ResourceDescriptionItem originalResourceDescriptionItem = getDetails().get(getOriginalLanguageCode());
                copyResourceDescriptionItemContent(originalResourceDescriptionItem, resourceDescriptionItem);
            }
            getDetails().put(lang, resourceDescriptionItem);
        }
        return resourceDescriptionItem;
    }

    private Map<String, String> getOtherDetails() {
        if (otherDetails == null) {
            otherDetails = new HashMap<>();
            getResourceDescription().setOtherDetails(otherDetails);
        }
        return otherDetails;
    }

    private ResourceDescriptionItem getResourceDescriptionItem() {
        return getResourceDescription().getDetails()
                .computeIfAbsent(getCurrentLanguageCode(), k -> new ResourceDescriptionItem());
    }

    public List<String> getKeywords() {
        List<String> keywords = getResourceDescriptionItem().getKeywords();
        if (keywords == null) {
            keywords = new ArrayList<>();
            getResourceDescriptionItem().setKeywords(keywords);
        }
        return keywords;
    }

    public RuleLineCollection getPreconditionRuleLines() {
        return getReadableGuide().getPreconditionRuleLines();
    }

    public RuleLineCollection getDefaultActions() {
        return getReadableGuide().getDefaultActions();
    }

    public RuleLineCollection getDefinitionRuleLines() {
        return getReadableGuide().getDefinitionRuleLines();
    }

    public ReadableGuide getReadableGuide() {
        if (readableGuide == null) {
            readableGuide = new ReadableGuide(getCurrentTermDefinition(), archetypeManager, archetypeReferencesManager);
        }
        return readableGuide;
    }

    public RuleLineCollection getConditionRuleLines() {
        return ruleAtEdit.getConditionRuleLines();
    }

    public RuleLineCollection getActionsRuleLines() {
        return ruleAtEdit.getActionRuleLines();
    }

    public void editRuleElement(RuleLineElementWithValue<?> ruleLineElementWithValue) {
        getRuleElementEditor().edit(ruleLineElementWithValue);
    }

    public void runIfOKToExit(final Runnable pendingRunnable) {
        GDLEditor gdlEditor = this;
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                if (isModified()) {
                    int response = JOptionPane.showConfirmDialog(
                            windowManager.getMainWindow(),
                            GDLEditorLanguageManager.getMessage("SavingChangesMessage"),
                            GDLEditorLanguageManager.getMessage("SavingChanges"),
                            JOptionPane.YES_NO_CANCEL_OPTION);
                    if (response == JOptionPane.YES_OPTION) {
                        SaveGuideOnFileRSW rsw = new SaveGuideOnFileRSW(
                                windowManager,
                                editorFileManager.getLastFileLoaded(),
                                gdlEditor,
                                editorFileManager) {
                            protected void done() {
                                super.done();
                                if (getFile() != null) {
                                    pendingRunnable.run();
                                }
                            }
                        };
                        rsw.execute();
                    } else if (response == JOptionPane.NO_OPTION) {
                        pendingRunnable.run();
                    }
                } else {
                    pendingRunnable.run();
                }
            }
        };
        runIfOkWithEditorState(runnable);
    }

    private GuideOntology getGuideOntology() {
        if (guideOntology == null) {
            guideOntology = new GuideOntology();
            guideOntology.setTermDefinitions(getTermDefinitions());
            guideOntology.setTermBindings(getTermBindings());
        }
        return guideOntology;
    }

    private Map<String, TermDefinition> getTermDefinitions() {
        if (termDefinitions == null) {
            termDefinitions = new HashMap<>();
            String lang = getOriginalLanguageCode();
            TermDefinition td = new TermDefinition();
            td.setTerms(new HashMap<>());
            termDefinitions.put(lang, td);
        }
        return termDefinitions;
    }

    public Map<String, TermBinding> getTermBindings() {
        if (termBindings == null) {
            termBindings = new HashMap<>();
        }
        return termBindings;
    }

    public Collection<String> getSupportedLanguageCodes() {
        return getTermDefinitions().keySet();
    }

    private TermDefinition getTermDefinition(String language) {
        TermDefinition termDefinition = getTermDefinitions().get(language);
        if (termDefinition == null) {
            termDefinition = new TermDefinition();
            termDefinition.setId(language);
            termDefinition.setTerms(new HashMap<>());
            getTermDefinitions().put(language, termDefinition);
        }
        return termDefinition;
    }

    public TermDefinition getCurrentTermDefinition() {
        return getTermDefinition(getCurrentLanguageCode());
    }

    private Language getLanguage() {
        if (language == null) {
            language = new Language();
        }
        return language;
    }

    public String getCurrentLanguageCode() {
        if (currentGuideLanguageCode == null) {
            String editorLanguage = archetypeManager.getUserConfigurationManager().getLanguage();
            if (getSupportedLanguageCodes().contains(editorLanguage)) {
                currentGuideLanguageCode = editorLanguage;
            } else {
                currentGuideLanguageCode = getOriginalLanguageCode();
            }
        }
        return currentGuideLanguageCode;
    }

    private String getOriginalLanguageCode() {
        CodePhrase originalLanuageCodePhrase = getLanguage().getOriginalLanguage();
        if (originalLanuageCodePhrase == null) {
            String LANGUAGE_TERMINOLOGY = "ISO_639-1";
            originalLanuageCodePhrase = new CodePhrase(
                    LANGUAGE_TERMINOLOGY, archetypeManager.getUserConfigurationManager().getLanguage());
            getLanguage().setOriginalLanguage(originalLanuageCodePhrase);
        }
        return getLanguage().getOriginalLanguage().getCodeString();
    }

    public String createNextLocalCode() {
        return createNextGTCode(true);
    }

    String createNextGTCode(boolean generateTerm) {
        String gtCode = generateNextGTCode(generateTerm);
        if (generateTerm) {
            getTerm(gtCode).setText(null);
            getTerm(gtCode).setDescription(null);
        }
        return gtCode;
    }

    private String generateNextGTCode(boolean generateTerm) {
        String nextGTCode = GT_HEADER
                + StringUtils.leftPad("" + (getNextTermNumber()), 4, "0");
        if (generateTerm) {
            for (String langCode : getTermDefinitions().keySet()) {
                getTerm(langCode, nextGTCode);
            }
        }
        return nextGTCode;
    }

    private int getNextTermNumber() {
        TermDefinition termDefinition = getTermDefinition(getLanguage()
                .getOriginalLanguage().getCodeString());
        int termCount = 0;
        if (termDefinition.getTerms() != null) {
            Set<String> gtCodes = new HashSet<>();
            gtCodes.addAll(termDefinition.getTerms().keySet());
            for (RuleLine ruleLine : getDefinitionRuleLines().getRuleLines()) {
                if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
                    gtCodes.add(((ArchetypeInstantiationRuleLine) ruleLine).getGTCode());
                }
            }
            for (String gtCode : gtCodes) {
                try {
                    int codeNum = Integer.parseInt(gtCode.substring(2));
                    if (codeNum > termCount) {
                        termCount = codeNum;
                    }
                } catch (Exception ex) {
                    logger.warn("Unable to parse code '{}'", gtCode);
                }
            }
        }
        return ++termCount;
    }

    private String getConceptGTCode() {
        if (conceptGTCode == null) {
            conceptGTCode = createNextLocalCode();
        }
        return conceptGTCode;
    }

    public Map<String, Term> getCurrentTermsMap() {
        return getTermsMap(getCurrentLanguageCode());
    }

    @Override
    public Collection<String> getUsedCodes() {
        Collection<String> gtCodesUsed = new ArrayList<>();
        gtCodesUsed.addAll(getGTCodesUsedInDefinitions());
        gtCodesUsed.addAll(getGTCodesUsedInBindings());
        return gtCodesUsed;
    }

    private Map<String, Term> getTermsMap(String langCode) {
        Map<String, Term> terms = getTermDefinition(langCode).getTerms();
        if (terms == null) {
            terms = new HashMap<>();
            getTermDefinition(langCode).setTerms(terms);
        }
        return terms;
    }

    Term getTerm(String gtCode) {
        return getTerm(getCurrentLanguageCode(), gtCode);
    }

    private Term getTerm(String langCode, String gtCode) {
        Term term = getCurrentTermsMap().get(gtCode);
        if (term == null) {
            Term originalTerm = getTermDefinition(getOriginalLanguageCode())
                    .getTerms().get(gtCode);
            term = GuidelineEditorManager.getTermToDifferentLanguage(gtCode, originalTerm,
                    getOriginalLanguageCode());
            getTermsMap(langCode).put(gtCode, term);
        }
        return getTermsMap(langCode).get(gtCode);
    }

    public Term getConceptTerm() {
        return getCurrentTermsMap().get(getConceptGTCode());
    }

    public String getGTName(String gtCode) {
        return getTerm(gtCode).getText();
    }

    public void setGTName(String gtCode, String text) {
        Term term = getTerm(gtCode);
        term.setText(text);
    }

    private Guide constructCurrentGuide() throws IllegalStateException {
        GuideDefinition guideDefinition = getGuideDefinition();
        insertPreconditions(guideDefinition);
        insertDefaultActions(guideDefinition);
        insertRules(guideDefinition);
        return getGuide(guideDefinition);
    }

    private GuideDefinition getGuideDefinition() {
        GuideDefinition guideDefinition = new GuideDefinition();
        for (RuleLine ruleLine : getDefinitionRuleLines().getRuleLines()) {
            if (!ruleLine.isCommented()) {
                insertDefinitionRuleLine(guideDefinition, ruleLine);
            }
        }
        return guideDefinition;
    }

    private void insertDefinitionRuleLine(GuideDefinition guideDefinition, RuleLine ruleLine) {
        if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
            ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) ruleLine;
            ArchetypeReference ar = airl.getArchetypeReference();
            if (ar != null) {
                Map<String, ElementBinding> elementMap = new HashMap<>();
                List<ExpressionItem> predicateStatements = new ArrayList<>();
                for (RuleLine ruleLineAux : airl.getChildrenRuleLines().getRuleLines()) {
                    if (!ruleLineAux.isCommented()) {
                        insertElementInstantiationRuleLinesAndPredicates(elementMap, predicateStatements, ruleLineAux);
                    }
                }
                ArchetypeBinding archetypeBinding = new ArchetypeBinding();
                archetypeBinding.setArchetypeId(ar.getIdArchetype());
                archetypeBinding.setDomain(ar.getIdDomain());
                archetypeBinding.setTemplateId(ar.getIdTemplate());
                archetypeBinding.setElements(elementMap);
                archetypeBinding.setPredicateStatements(predicateStatements);
                archetypeBinding.setId(airl.getGTCode());
                guideDefinition.getArchetypeBindings().put(archetypeBinding.getId(), archetypeBinding);
            }
        }
    }

    private void insertElementInstantiationRuleLinesAndPredicates(Map<String, ElementBinding> elementMap, List<ExpressionItem> predicateStatements, RuleLine ruleLineAux) {
        if (ruleLineAux instanceof ArchetypeElementInstantiationRuleLine) {
            ArchetypeElementInstantiationRuleLine aeirl = (ArchetypeElementInstantiationRuleLine) ruleLineAux;
            ArchetypeElementVO archetypeElementVO = aeirl
                    .getArchetypeElementRuleLineDefinitionElement()
                    .getValue();
            String elementGTCode = aeirl.getGTCode();
            if (archetypeElementVO != null) {
                ElementBinding element = new ElementBinding(
                        elementGTCode,
                        archetypeElementVO.getPath());
                elementMap.put(elementGTCode, element);
            }
        } else if (ruleLineAux instanceof WithElementPredicateAttributeDefinitionRuleLine) {
            WithElementPredicateAttributeDefinitionRuleLine wepadrl = (WithElementPredicateAttributeDefinitionRuleLine) ruleLineAux;
            predicateStatements.add(wepadrl.toExpressionItem());
        } else if (ruleLineAux instanceof WithElementPredicateExpressionDefinitionRuleLine) {
            WithElementPredicateExpressionDefinitionRuleLine wepedrl = (WithElementPredicateExpressionDefinitionRuleLine) ruleLineAux;
            predicateStatements.add(wepedrl.toExpressionItem());
        } else if (ruleLineAux instanceof WithElementPredicateFunctionDefinitionRuleLine) {
            WithElementPredicateFunctionDefinitionRuleLine wepfdrl = (WithElementPredicateFunctionDefinitionRuleLine) ruleLineAux;
            predicateStatements.add(wepfdrl.toExpressionItem());
        } else if (ruleLineAux instanceof WithElementPredicateExistsDefinitionRuleLine) {
            WithElementPredicateExistsDefinitionRuleLine wepedrl = (WithElementPredicateExistsDefinitionRuleLine) ruleLineAux;
            predicateStatements.add(wepedrl.toExpressionItem());
        }
    }

    private Guide getGuide(GuideDefinition guideDefinition) {
        Guide guide = new Guide();
        guide.setId(getEntityId());

        String GDL_VERSION = "0.1";
        guide.setGdlVersion(GDL_VERSION);
        guide.setLanguage(getLanguage());
        guide.setConcept(getConceptGTCode());
        guide.setDescription(getResourceDescription());
        guide.setDefinition(guideDefinition);
        guide.setOntology(getGuideOntology());

        if (!getTermDefinitions().isEmpty()) {
            cleanRedundantTerms();
        }
        if (!getTermBindings().isEmpty()) {
            cleanRedundantTerms();
            updateInvalidData();
        }
        return guide;
    }

    private void insertRules(GuideDefinition guideDefinition) {
        int priority = getRenderableRules().size();
        for (ReadableRule readableRule : getRenderableRules().values()) {
            if (!readableRule.isCommented()) {
                String gtCode = readableRule.getGTCode();
                Rule rule = new Rule();
                rule.setId(gtCode);
                guideDefinition.getRules().put(gtCode, rule);
                rule.setWhenStatements(convertToExpressionItems(readableRule.getConditionRuleLines().getRuleLines()));
                rule.setThenStatements(convertToAssignmentExpressionItems(readableRule.getActionRuleLines().getRuleLines()));
                rule.setPriority(priority--);
            }
        }
    }

    private void insertDefaultActions(GuideDefinition guideDefinition) {
        List<AssignmentExpression> defaultActions = convertToAssignmentExpressionItems(getDefaultActions().getRuleLines());
        guideDefinition.setDefaultActionExpressions(defaultActions);
    }

    private void insertPreconditions(GuideDefinition guideDefinition) {
        List<ExpressionItem> preConditions = convertToExpressionItems(getPreconditionRuleLines().getRuleLines());
        guideDefinition.setPreConditionExpressions(preConditions);
    }

    public Guide getEntity() {
        try {
            return constructCurrentGuide();
        } catch (IllegalStateException ex) {
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    windowManager.getMainWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuide"),
                    ex.getMessage(), MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    public void setEntity(Guide guide) {
        checkGuideArchetypesAndTemplates(guide);
        initGuideVars();
        if (guide.getId() != null) {
            guideId = guide.getId();
        }
        resourceDescription = guide.getDescription();
        if (resourceDescription != null) {
            originalAuthor = resourceDescription.getOriginalAuthor();
            details = resourceDescription.getDetails();
            otherContributors = resourceDescription.getOtherContributors();
            otherDetails = resourceDescription.getOtherDetails();
        }
        language = guide.getLanguage();
        conceptGTCode = guide.getConcept();
        guideOntology = guide.getOntology();
        termDefinitions = getGuideOntology().getTermDefinitions();
        if (termDefinitions == null) {
            termDefinitions = new HashMap<>();
        } else {
            guidelineEditorManager.check(getOriginalLanguageCode(), getTermDefinitions());
        }
        termBindings = getGuideOntology().getTermBindings();
        if (termBindings == null) {
            termBindings = new HashMap<>();
        }
        guideOntology.setTermDefinitions(termDefinitions);
        guideOntology.setTermBindings(termBindings);
        updateReadableGuide(guide);
        initResourceDescription();
        updateOriginal();
    }

    private void updateReadableGuide(Guide guide) {
        try {
            readableGuide = guideImporter.importGuide(guide, getCurrentLanguageCode());
        } catch (InternalErrorException ex) {
            throw new RuntimeException(ex);
        }
    }

    private void updateInvalidData() {
        Set<String> codesStringSet = getTermBindings().keySet();
        for (String codeString : codesStringSet) {
            if (getTermBindings().get(codeString).getBindings() == null) {
                getTermBindings().get(codeString).setBindings(new HashMap<>());
            }
        }
    }

    private void updateOriginal() {
        originalGuide = getSerializedEntity();
    }

    public String getSerializedEntity() {
        Guide guide = getEntity();
        if (guide != null) {
            return guidelineEditorManager.serializeGuide(guide);
        } else {
            return null;
        }
    }

    private List<ExpressionItem> convertToExpressionItems(
            Collection<RuleLine> ruleLines) {
        List<ExpressionItem> list = new ArrayList<>();
        for (RuleLine ruleLine : ruleLines) {
            if (!ruleLine.isCommented()) {
                ExpressionItem ei = ((ExpressionRuleLine) ruleLine).toExpressionItem();
                if (ei != null) {
                    list.add(ei);
                }
            }
        }
        return list;
    }

    private List<AssignmentExpression> convertToAssignmentExpressionItems(Collection<RuleLine> ruleLines) {
        List<AssignmentExpression> list = new ArrayList<>();
        for (RuleLine ruleLine : ruleLines) {
            if (!ruleLine.isCommented()) {
                AssignmentExpression assignmentExpression =
                        ((AssignmentExpressionRuleLine) ruleLine).toAssignmentExpression();
                list.add(assignmentExpression);
            }
        }
        return list;
    }

    @Override
    public String getEntityId() {
        return guideId;
    }

    @Override
    public void setEntityId(String idGuide) {
        guideId = idGuide;
    }

    public void changeLanguage(String language) {
        Guide guide = getEntity();
        if (guide != null) {
            TermDefinition originalTermDefinition = getTermDefinitions().get(getOriginalLanguageCode());
            TermDefinition termDefinition = getTermDefinition(language);
            for (String gtCode : originalTermDefinition.getTerms().keySet()) {
                Term term = termDefinition.getTerms().get(gtCode);
                if (term == null || term.getText() == null) {
                    term = GuidelineEditorManager.getTermToDifferentLanguage(
                            gtCode,
                            originalTermDefinition.getTerms().get(gtCode),
                            getOriginalLanguageCode());
                    termDefinition.getTerms().put(gtCode, term);
                }
                if (term == null || term.getText() == null) {
                    term = GuidelineEditorManager.getTermToDifferentLanguage(
                            gtCode,
                            getTermDefinition(currentGuideLanguageCode).getTerms().get(gtCode),
                            currentGuideLanguageCode);
                    termDefinition.getTerms().put(gtCode, term);
                }
            }
            currentGuideLanguageCode = language;
            getResourceDescriptionItem(language);
            updateReadableGuide(guide);
            updateRuleAtEdit();
            refreshCurrentTab();
        }
    }

    private void updateRuleAtEdit() {
        if (ruleAtEdit != null) {
            ruleAtEdit = readableGuide.getReadableRules().get(ruleAtEdit.getGTCode());
        }
    }

    private void copyResourceDescriptionItemContent(ResourceDescriptionItem originalResourceDescriptionItem, ResourceDescriptionItem resourceDescriptionItem) {
        if (originalResourceDescriptionItem.equals(resourceDescriptionItem)) {
            return;
        }
        String originalLanguage = getOriginalLanguageCode();
        String languagePrefix = "*(" + originalLanguage + ") ";
        String use = originalResourceDescriptionItem.getUse();
        if (use != null) {
            resourceDescriptionItem.setUse(languagePrefix + use);
        }
        String misuse = originalResourceDescriptionItem.getMisuse();
        if (misuse != null) {
            resourceDescriptionItem.setMisuse(languagePrefix + misuse);
        }
        String purpose = originalResourceDescriptionItem.getPurpose();
        if (purpose != null) {
            resourceDescriptionItem.setPurpose(languagePrefix + purpose);
        }
        resourceDescriptionItem.setCopyright(originalResourceDescriptionItem.getCopyright());
        for (String keyword : originalResourceDescriptionItem.getKeywords()) {
            resourceDescriptionItem.getKeywords().add(languagePrefix + keyword);
        }
    }

    private void refreshCurrentTab() {
        getEditorPanel().refresh();
    }

    public void tabChanged(Component comp) {
        final RefreshablePanel refreshablePanel;
        if (comp instanceof RefreshablePanel && comp != lastRefreshedPanel) {
            refreshablePanel = (RefreshablePanel) comp;
        } else {
            refreshablePanel = null;
        }
        runIfOkWithEditorState(comp, () -> SwingUtilities.invokeLater(() -> refreshPanel(refreshablePanel)));
    }

    private void refreshPanel(RefreshablePanel refreshablePanel) {
        if (refreshablePanel != null) {
            refreshablePanel.refresh();
            lastRefreshedPanel = refreshablePanel;
        }
    }

    private void runIfOkWithEditorState(Runnable pendingRunnable) {
        Component component = getEditorPanel().getGuidePanel().getGuideEditorTabPane().getSelectedComponent();
        runIfOkWithEditorState(component, pendingRunnable);
    }

    private void runIfOkWithEditorState(Component currentPanel, Runnable pendingRunnable) {
        if (lastRefreshedPanel instanceof GDLPanel) {
            GDLPanel gdlPanel = (GDLPanel) lastRefreshedPanel;
            String guideStr = gdlPanel.getGuideStr();
            new CheckGuideSW(this, guideStr, pendingRunnable).execute();
        } else if (currentPanel instanceof GDLPanel) {
            try {
                String guideStr = null;
                Guide guide = getEntity();
                if (guide != null) {
                    guideStr = guidelineEditorManager.serializeGuide(guide);
                }
                if (guideStr != null) {
                    new CheckGuideSW(this, guideStr, pendingRunnable).execute();
                } else {
                    loadLastTab();
                }
            } catch (Exception ex) {
                loadLastTab();
            }
        } else {
            pendingRunnable.run();
        }
    }

    public void gdlEditingChecked(Guide guide, boolean checkOk, String msg, Runnable pendingRunnable) {
        if (checkOk && guide != null) {
            updateGuide(guide);
            if (pendingRunnable != null) {
                pendingRunnable.run();
            }
        } else {
            DialogLongMessageNotice dialog =
                    new DialogLongMessageNotice(
                            windowManager.getMainWindow(),
                            GDLEditorLanguageManager.getMessage("IgnoreGDLSourceChangesTitle"),
                            GDLEditorLanguageManager.getMessage("IgnoreGDLSourceChanges"),
                            msg,
                            MessageType.WARNING_WITH_CANCEL
                    );
            dialog.setVisible(true);
            if (dialog.getAnswer()) {
                if (pendingRunnable != null) {
                    pendingRunnable.run();
                }
            } else {
                loadLastTab();
            }
        }
    }

    public void updateGuide(Guide guide) {
        String auxOriginalGuide = originalGuide;
        setEntity(guide);
        originalGuide = auxOriginalGuide;
    }

    private void loadLastTab() {
        Component component = (Component) lastRefreshedPanel;
        gdlEditorMainPanel.getGuidePanel().getGuideEditorTabPane().setSelectedComponent(component);
    }

    private void initGuideVars() {
        guideId = UNKNOWN_GUIDE_ID;
        resourceDescription = null;
        originalAuthor = null;
        details = null;
        otherContributors = null;
        otherDetails = null;
        language = null;
        conceptGTCode = null;
        guideOntology = null;
        termDefinitions = null;
        termBindings = null;
        readableGuide = null;
        ruleAtEdit = null;
    }

    public boolean isModified() {
        String serializedGuide = null;
        try {
            Guide guide = constructCurrentGuide();
            serializedGuide = GuideUtil.serializeGuide(guide);
        } catch (Exception exception) {
            logger.error("Guideline not serializable while checking for modifications.", exception);
        }
        return originalGuide != null && !originalGuide.equals(serializedGuide);
    }

    private void cleanRedundantTerms() {
        Collection<String> gtCodesUsed = getGTCodesUsedInGuide();
        for (TermDefinition termDefinition : getTermDefinitions().values()) {
            Collection<Term> terms = new ArrayList<>(termDefinition
                    .getTerms().values());
            for (Term term : terms) {
                if (!gtCodesUsed.contains(term.getId())) {
                    termDefinition.getTerms().remove(term.getId());
                }
            }
        }
    }

    private Collection<String> getGTCodesUsedInGuide() {
        TermDefinition td =
                getTermDefinitions().get(getCurrentLanguageCode());
        if (td != null) {
            return td.getTerms().keySet();
        } else {
            return Collections.emptyList();
        }
    }

    public Collection<String> getGTCodesUsedInDefinitions() {
        Collection<String> gtCodes = new ArrayList<>();
        gtCodes.add(getConceptGTCode());
        gtCodes.addAll(getRenderableRules().keySet());
        return gtCodes;
    }

    private Collection<String> getGTCodesUsedInBindings() {
        Collection<String> gtCodes = new HashSet<>();
        for (TermBinding termBinding : getTermBindings().values()) {
            gtCodes.addAll(termBinding.getBindings().keySet());
        }
        return gtCodes;
    }

    public void saveCompiledGuideAsObject(byte[] compiledGuide, Guide guide) {
        String idGuide = getEntityId();
        if (idGuide == null) {
            idGuide = GDLEditorLanguageManager.getMessage("Guide");
        }
        if (compiledGuide != null) {
            String guideSource = getSerializedEntity();
            if (guideSource != null) {
                JFileChooser fileChooser = new JFileChooser();
                FileNameExtensionFilter filter = new FileNameExtensionFilter(
                        GDLEditorLanguageManager.getMessage("Guide"), "guide");
                fileChooser.setDialogTitle(GDLEditorLanguageManager
                        .getMessage("SaveGuideAsObjectSD"));
                fileChooser.setFileFilter(filter);
                File file = new File(fileChooser.getFileSystemView()
                        .getDefaultDirectory() + "/" + idGuide + ".guide");
                fileChooser.setSelectedFile(file);
                int result = fileChooser.showSaveDialog(windowManager.getMainWindow());
                File guideFile = fileChooser.getSelectedFile();
                if (result != JFileChooser.CANCEL_OPTION) {
                    idGuide = guideFile.getName();
                    if (idGuide.endsWith(".guide")) {
                        idGuide = idGuide
                                .substring(0, idGuide.length() - 6);
                    }
                    GuideDTO guideDTO =
                            new GuideDTOBuilder()
                                    .setId(idGuide)
                                    .setFormat(guideSource)
                                    .setSource(CMTypeFormat.GDL_FORMAT.getFormat())
                                    .setGuideObject(SerializationUtils.serialize(guide))
                                    .setCompiledGuide(compiledGuide)
                                    .setLastUpdate(Calendar.getInstance().getTime())
                                    .createGuideDTO();
                    try (ObjectOutputStream output = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(guideFile)))) {
                        output.writeObject(guideDTO);
                    } catch (Exception ex) {
                        logger.error("Error building guideline: " + idGuide, ex);
                    }
                }
            }
        }
    }

    public ArchetypeInstantiationRuleLine addArchetypeReference(
            boolean showOnlyCDS) {
        ArchetypeInstantiationRuleLine airl = new ArchetypeInstantiationRuleLine();
        airl.setGTCode(createNextGTCode(false));
        getRuleElementEditor().editArchetype(
                airl.getArchetypeReferenceRuleLineDefinitionElement(),
                showOnlyCDS);
        if (airl.getArchetypeReferenceRuleLineDefinitionElement().getValue() != null) {
            getDefinitionRuleLines().add(airl);
            return airl;
        } else {
            return null;
        }
    }

    public ArchetypeElementInstantiationRuleLine addArchetypeElement(
            ArchetypeInstantiationRuleLine airl) {
        ArchetypeElementInstantiationRuleLine aeirl = new ArchetypeElementInstantiationRuleLine(airl);
        aeirl.getGTCodeRuleLineElement().setValue(createNextLocalCode());
        editRuleElement(aeirl.getArchetypeElementRuleLineDefinitionElement());
        if (aeirl.getArchetypeElementRuleLineDefinitionElement().getValue() != null) {
            return aeirl;
        } else {
            aeirl.detachFromParent();
            return null;
        }
    }

    public void setBusy(String description) {
        getInfoDialog().changeLoadingText(description);
        getInfoDialog().start();
    }

    private InfoDialog getInfoDialog() {
        if (infoDialog == null) {
            infoDialog = new InfoDialog(windowManager.getMainWindow());
        }
        return infoDialog;
    }

    private void setFree() {
        getInfoDialog().stop();
    }

    private void checkGuideArchetypesAndTemplates(Guide guide)
            throws InternalErrorException, InstanceNotFoundException {
        if (guide == null || guide.getDefinition() == null
                || guide.getDefinition().getArchetypeBindings() == null) {
            return;
        }
        for (ArchetypeBinding archetypeBinding : guide.getDefinition().getArchetypeBindings().values()) {
            String archetypeId = archetypeBinding.getArchetypeId();
            String templateId = archetypeBinding.getTemplateId();
            if (templateId == null) {
                try {
                    archetypeManager.getArchetypes().getCMElement(archetypeId);
                } catch (InstanceNotFoundException ex) {
                    File selectedFile = new File(archetypeId + ".adl");
                    int result = importManager.showImportArchetypeDialogAndAddToRepo(windowManager.getMainWindow(), selectedFile);
                    if (result == JFileChooser.CANCEL_OPTION) {
                        throw new InternalErrorException(new Exception("Archetype '" + archetypeId + "' not found."));
                    }
                }
            } else {
                try {
                    archetypeManager.getTemplates().getCMElement(templateId);
                } catch (InstanceNotFoundException ex) {
                    File selectedFile = new File(templateId + ".oet");
                    int result = importManager.showImportTemplateDialog(
                            windowManager.getMainWindow(), selectedFile);
                    if (result == JFileChooser.CANCEL_OPTION) {
                        throw new InternalErrorException(new Exception("Template '" + templateId + "' not found."));
                    }
                }
            }
        }
    }

    public void setOnlyGDLSourceEditing(boolean onlyGDLSourceEditing) {
        getEditorPanel().getGuidePanel().getGuideEditorTabPane().setEnabled(!onlyGDLSourceEditing);
        getEditorPanel().getAddRuleButton().setEnabled(!onlyGDLSourceEditing);
        getEditorPanel().getCreateBindingButton().setEnabled(!onlyGDLSourceEditing);
        getEditorPanel().getGenerateFormButton().setEnabled(!onlyGDLSourceEditing);
    }

    private RuleElementEditor getRuleElementEditor() {
        if (ruleElementEditor == null) {
            ruleElementEditor = new RuleElementEditor(
                    windowManager,
                    archetypeReferencesManager,
                    archetypeManager,
                    dbPanelFactory,
                    this,
                    importManager);
        }
        return ruleElementEditor;
    }

    @Override
    public String getEntityName() {
        return GDLEditorLanguageManager.getMessage("Guide");
    }

    @Override
    public Collection<String> getSupportedEntityExtensions() {
        return SUPPORTED_EXTENSIONS;
    }

    @Override
    public void updateTerm(Term term) {
        getTermsMap(getCurrentLanguageCode()).put(term.getId(), term);
    }

    public TerminologyService getTerminologyService() {
        return terminologyService;
    }

    public TerminologyDialogManager getTerminologyDialogManager() {
        return terminologyDialogManager;
    }

    public GuideImporter getGuideImporter() {
        return guideImporter;
    }

    public RuleLine cloneRuleLine(RuleLine ruleLine) {
        return this.ruleLineCloner.clone(ruleLine);
    }

    public GuideExportPluginDirectory getGuideExportPluginDirectory() {
        return guideExportPluginDirectory;
    }

    public GuidelineEditorManager getGuidelineEditorManager() {
        return guidelineEditorManager;
    }

    public Window getEditorWindow() {
        return windowManager.getMainWindow();
    }

    public EditorFileManager getEditorFileManager() {
        return editorFileManager;
    }

    public WindowManager getWindowManager() {
        return windowManager;
    }

    public Boolean isActive() {
        return active;
    }

    public Boolean close() {
        active = false;
        return true;
    }
}
/*
 * ***** BEGIN LICENSE BLOCK ***** Version: MPL 2.0/GPL 2.0/LGPL 2.1
 * 
 * The contents of this file are subject to the Mozilla Public License Version
 * 2.0 (the 'License'); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 * 
 * 
 * The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 * Portions created by the Initial Developer are Copyright (C) 2012-2013 the
 * Initial Developer. All Rights Reserved.
 * 
 * Contributor(s):
 * 
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 * 
 * ***** END LICENSE BLOCK *****
 */
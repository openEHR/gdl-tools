package se.cambio.cds.gdl.editor.controller;

import difflib.Delta;
import difflib.DiffUtils;
import difflib.Patch;
import org.apache.commons.lang.SerializationUtils;
import org.apache.commons.lang.StringUtils;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.formgen.view.dialog.CDSFormGenDialog;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.controller.sw.CheckForChangesOnGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.CheckGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.CompileGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.SaveGuideOnFileRSW;
import se.cambio.cds.gdl.editor.util.DefinitionDependencyChecker;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogNameInsert;
import se.cambio.cds.gdl.editor.view.panels.GDLEditorMainPanel;
import se.cambio.cds.gdl.editor.view.panels.GDLPanel;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.GuideImporter;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.guide.dto.GuideDTOBuilder;
import se.cambio.cm.model.util.CMTypeFormat;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice.MessageType;
import se.cambio.openehr.view.dialogs.InfoDialog;
import se.cambio.openehr.view.util.ImportUtils;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;

public class GDLEditor implements EditorController<Guide> {

    private static String UNKNOWN_GUIDE_ID = "unknown";
    private static String GT_HEADER = "gt";
    private GDLEditorMainPanel _gdlEditorMainPanel = null;
    private ResourceDescription _resourceDescription = null;
    private GuideOntology _guideOntology = null;
    private ReadableGuide _readableGuide = null;
    private ReadableRule _ruleAtEdit = null;
    private Map<String, TermDefinition> _termDefinitions = null;
    private Map<String, TermBinding> _termBindings = null;
    private Language _language;
    private Map<String, String> _originalAuthor;
    private List<String> _otherContributors;
    private Map<String, ResourceDescriptionItem> _details;
    private Map<String, String> _otherDetails;
    private InfoDialog _infoDialog;
    private String guideId = UNKNOWN_GUIDE_ID;
    private String conceptGTCode;
    private String currentGuideLanguageCode = null;
    private String originalGuide = null;
    private RefreshablePanel _lastRefreshedPanel = null;
    private Logger logger = LoggerFactory.getLogger(GDLEditor.class);

    public GDLEditor() {
    }

    public GDLEditor(Guide guide) {
        if (guide == null) {
            guide = new Guide();
        }
        setEntity(guide);
    }

    private static Term getTermToDifferentLanguage(String gtCode,
                                                   Term originalTerm, String originalLanguage) {
        Term newTerm = new Term();
        newTerm.setId(gtCode);
        String text = null;
        String description = null;
        String languagePrefix = "*(" + originalLanguage + ") ";
        if (originalTerm != null && originalTerm.getText() != null
                && !originalTerm.getText().isEmpty()) {
            text = languagePrefix + originalTerm.getText();
        }
        if (originalTerm != null && originalTerm.getDescription() != null
                && !originalTerm.getDescription().isEmpty()) {
            description = languagePrefix + originalTerm.getDescription();
        }
        newTerm.setText(text);
        newTerm.setDescription(description);
        return newTerm;
    }

    private static String serializeGuide(Guide guide) {
        try {
            return GuideUtil.serializeGuide(guide);
        } catch (Exception e) {
            e.printStackTrace();
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    EditorManager.getActiveEditorWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuide"),
                    e.getMessage(), MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    public static Guide parseGuide(InputStream input) {
        try {
            return GuideUtil.parseGuide(input);
        } catch (Exception e) {
            e.printStackTrace();
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    EditorManager.getActiveEditorWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorParsingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorParsingGuide"),
                    e.getMessage(), MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    private void check(String originalLang, Map<String, TermDefinition> termDefinitionMap) {
        TermDefinition originalTermDefinition =
                termDefinitionMap.get(originalLang);
        for (Map.Entry<String, TermDefinition> langCode : termDefinitionMap.entrySet()) {
            if (!langCode.getKey().equals(originalLang)) {
                TermDefinition td = langCode.getValue();
                for (String gtCode : originalTermDefinition.getTerms().keySet()) {
                    if (!td.getTerms().containsKey(gtCode)) {
                        logger.warn(
                                "Language '" + langCode
                                        + "' does not contain gt code '"
                                        + gtCode + "'. Will be added...");
                        Term term = getTermToDifferentLanguage(gtCode,
                                originalTermDefinition.getTerms().get(gtCode),
                                originalLang);
                        td.getTerms().put(gtCode, term);
                    }
                }
            }
        }
    }

    public static boolean checkParsedGuide(String guideSrc, Guide guide) {
        String guideSrcAux = GDLEditor.serializeGuide(guide);
        if (guide != null) {
            Patch patch = DiffUtils.diff(stringToLines(guideSrc), stringToLines(guideSrcAux));
            if (patch.getDeltas().isEmpty()) {
                return true;
            } else {
                StringBuilder diff = new StringBuilder();
                for (Delta delta : patch.getDeltas()) {
                    diff.append("-------------------------------------\n");
                    diff.append(" line:").append(delta.getOriginal().getPosition()).append(1).append("\n");
                    diff.append(" original:").append(delta.getOriginal().getLines()).append("\n");
                    diff.append(" revised:").append(delta.getRevised().getLines()).append("\n");
                }
                DialogLongMessageNotice dialog =
                        new DialogLongMessageNotice(
                                EditorManager.getActiveEditorWindow(),
                                GDLEditorLanguageManager.getMessage("ErrorLoadingGuideT"),
                                GDLEditorLanguageManager.getMessage("ErrorLoadingGuide"),
                                diff.toString(),
                                MessageType.WARNING_WITH_CANCEL
                        );
                dialog.setVisible(true);
                return dialog.getAnswer();
            }
        } else {
            return false;
        }
    }

    private static List<String> stringToLines(String str) {
        final List<String> lines = new ArrayList<>();
        for (String string : str.split("\n")) {
            lines.add(string.trim());
        }
        return lines;
    }

    public void init() {
        EditorManager.getActiveEditorViewer().setContent(getEditorPanel());
        EditorManager.getMainMenuBar().refreshLanguageMenu();
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
                    EditorManager.getActiveEditorWindow(),
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
                    EditorManager.getActiveEditorWindow(),
                    GDLEditorLanguageManager.getMessage("ReferenceBeingUsedMsg"),
                    GDLEditorLanguageManager.getMessage("ReferenceBeingUsedTitle"),
                    JOptionPane.WARNING_MESSAGE);
            return false;
        } else {
            return true;
        }
    }

    public GDLEditorMainPanel getEditorPanel() {
        if (_gdlEditorMainPanel == null) {
            _gdlEditorMainPanel = new GDLEditorMainPanel(this);
        }
        return _gdlEditorMainPanel;
    }

    public void saveAs() {
        _gdlEditorMainPanel.requestFocus();
        runIfOkWithEditorState(() -> new SaveGuideOnFileRSW(null).execute());
    }

    @Override
    public void entitySaved() {
        updateOriginal();
        getEditorPanel().getSaveButton().setEnabled(false);
    }

    public void save() {
        _gdlEditorMainPanel.requestFocus();
        runIfOkWithEditorState(() -> new SaveGuideOnFileRSW(EditorManager.getLastFileLoaded()).execute());
    }

    public void generateForm() {
        _gdlEditorMainPanel.requestFocus();
        Runnable runnable = () -> {
            if (!hasActiveRules()) {
                JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("PleaseInsertRulesBeforeGeneratingForm"), GDLEditorLanguageManager.getMessage("GenerateForm"), JOptionPane.WARNING_MESSAGE);
                return;
            }
            String gdlGuide = getSerializedEntity();
            if (gdlGuide != null) {
                CompileGuideSW sw = new CompileGuideSW() {
                    protected void done() {
                        getController().compilationFinished(getErrorMsg());
                        if (getErrorMsg() == null) {
                            try {
                                generateDialogForm(getCompiledGuide(), getGuide());
                            } catch (InternalErrorException e) {
                                ExceptionHandler.handle(e);
                            }
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
        EditorController controller = EditorManager.getActiveEditorController();
        String gdlGuide = controller.getSerializedEntity();
        if (compiledGuide != null && gdlGuide != null) {
            GuideDTO guideDTO =
                    new GuideDTOBuilder()
                            .setId(controller.getEntityId())
                            .setFormat(CMTypeFormat.GDL_FORMAT.getFormat())
                            .setSource(gdlGuide)
                            .setGuideObject(SerializationUtils.serialize(guide))
                            .setCompiledGuide(compiledGuide)
                            .setLastUpdate(Calendar.getInstance().getTime())
                            .createGuideDTO();
            FormGeneratorController formGenerator =
                    new FormGeneratorController(guideDTO, controller.getCurrentLanguageCode());
            Date date = UserConfigurationManager.instance().getCurrentDateTime();
            if (date != null) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(date);
                formGenerator.setCurrentDate(cal);
            }
            CDSFormGenDialog dialog = new CDSFormGenDialog(EditorManager.getActiveEditorWindow());
            dialog.setFormGeneratorController(formGenerator);
            dialog.setVisible(true);
        }
    }

    public void compilationFinished(String msg) {
        setFree();
        if (msg != null) {
            JOptionPane.showMessageDialog(
                    EditorManager.getActiveEditorWindow(), msg,
                    GDLEditorLanguageManager.getMessage("Error"),
                    JOptionPane.ERROR_MESSAGE);
        }
    }

    public ReadableRule createNewRule() {
        _ruleAtEdit = null;
        DialogNameInsert dialog = new DialogNameInsert(
                EditorManager.getActiveEditorWindow(),
                GDLEditorLanguageManager.getMessage("RuleName"), "");
        if (dialog.getAnswer()) {
            String nextGtCode = createNextLocalCode();
            _ruleAtEdit = new ReadableRule(getCurrentTermDefinition(), nextGtCode, _readableGuide);
            setGTName(nextGtCode, dialog.getValue());
            getRenderableRules().put(_ruleAtEdit.getGTCode(), _ruleAtEdit);
        }
        return _ruleAtEdit;
    }

    public void ruleEdit(ReadableRule rule) {
        _ruleAtEdit = rule;
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
        if (_resourceDescription == null) {
            _resourceDescription = new ResourceDescription();
            String DRAFT = "Author draft";
            _resourceDescription.setLifecycleState(DRAFT);
            initResourceDescription();
        }
        return _resourceDescription;
    }

    private void initResourceDescription() {
        getOriginalAuthor();
        getOtherContributors();
        getResourceDescriptionItem(getCurrentLanguageCode());
        getOtherDetails();
        getKeywords();
    }

    private Map<String, String> getOriginalAuthor() {
        if (_originalAuthor == null) {
            _originalAuthor = new HashMap<>();
            getResourceDescription().setOriginalAuthor(_originalAuthor);
        }
        return _originalAuthor;
    }

    private List<String> getOtherContributors() {
        if (_otherContributors == null) {
            _otherContributors = new ArrayList<>();
            getResourceDescription().setOtherContributors(_otherContributors);
        }
        return _otherContributors;
    }

    public Map<String, ResourceDescriptionItem> getDetails() {
        if (_details == null) {
            _details = new HashMap<>();
            getResourceDescription().setDetails(_details);
        }
        return _details;
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
        if (_otherDetails == null) {
            _otherDetails = new HashMap<>();
            getResourceDescription().setOtherDetails(_otherDetails);
        }
        return _otherDetails;
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
        if (_readableGuide == null) {
            _readableGuide = new ReadableGuide(getCurrentTermDefinition(), ArchetypeManager.getInstance());
        }
        return _readableGuide;
    }

    public RuleLineCollection getConditionRuleLines() {
        return _ruleAtEdit.getConditionRuleLines();
    }

    public RuleLineCollection getActionsRuleLines() {
        return _ruleAtEdit.getActionRuleLines();
    }

    public void editRuleElement(RuleLineElementWithValue<?> ruleLineElementWithValue) {
        try {
            RuleElementEditor.edit(ruleLineElementWithValue);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        }
    }

    public void runIfOKToExit(final Runnable pendingRunnable) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                if (isModified()) {
                    int response = JOptionPane.showConfirmDialog(
                            EditorManager.getActiveEditorWindow(),
                            GDLEditorLanguageManager.getMessage("SavingChangesMessage"),
                            GDLEditorLanguageManager.getMessage("SavingChanges"),
                            JOptionPane.YES_NO_CANCEL_OPTION);
                    if (response == JOptionPane.YES_OPTION) {
                        SaveGuideOnFileRSW rsw = new SaveGuideOnFileRSW(
                                EditorManager.getLastFileLoaded()) {
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
        if (_guideOntology == null) {
            _guideOntology = new GuideOntology();
            _guideOntology.setTermDefinitions(getTermDefinitions());
            _guideOntology.setTermBindings(getTermBindings());
        }
        return _guideOntology;
    }

    private Map<String, TermDefinition> getTermDefinitions() {
        if (_termDefinitions == null) {
            _termDefinitions = new HashMap<>();
            String lang = getOriginalLanguageCode();
            TermDefinition td = new TermDefinition();
            td.setTerms(new HashMap<>());
            _termDefinitions.put(lang, td);
        }
        return _termDefinitions;
    }

    public Map<String, TermBinding> getTermBindings() {
        if (_termBindings == null) {
            _termBindings = new HashMap<>();
        }
        return _termBindings;
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
        if (_language == null) {
            _language = new Language();
        }
        return _language;
    }

    public String getCurrentLanguageCode() {
        if (currentGuideLanguageCode == null) {
            String editorLanguage = UserConfigurationManager.instance().getLanguage();
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
                    LANGUAGE_TERMINOLOGY, UserConfigurationManager.instance().getLanguage());
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
                } catch (Exception e) {
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
            term = getTermToDifferentLanguage(gtCode, originalTerm,
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
        for (ReadableRule renderableRule : getRenderableRules().values()) {
            if (!renderableRule.isCommented()) {
                String gtCode = renderableRule.getGTCode();
                Rule rule = new Rule();
                rule.setId(gtCode);
                guideDefinition.getRules().put(gtCode, rule);
                rule.setWhenStatements(convertToExpressionItems(renderableRule.getConditionRuleLines().getRuleLines()));
                rule.setThenStatements(convertToAssignmentExpressionItems(renderableRule.getActionRuleLines().getRuleLines()));
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
        } catch (IllegalStateException e) {
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    EditorManager.getActiveEditorWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuide"),
                    e.getMessage(), MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    public void setEntity(Guide guide) {
        try {
            checkGuideArchetypesAndTemplates(guide);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        }
        initGuideVars();
        if (guide.getId() != null) {
            guideId = guide.getId();
        }
        _resourceDescription = guide.getDescription();
        if (_resourceDescription != null) {
            _originalAuthor = _resourceDescription.getOriginalAuthor();
            _details = _resourceDescription.getDetails();
            _otherContributors = _resourceDescription.getOtherContributors();
            _otherDetails = _resourceDescription.getOtherDetails();
        }
        _language = guide.getLanguage();
        conceptGTCode = guide.getConcept();
        _guideOntology = guide.getOntology();
        _termDefinitions = getGuideOntology().getTermDefinitions();
        if (_termDefinitions == null) {
            _termDefinitions = new HashMap<>();
        } else {
            check(getOriginalLanguageCode(), getTermDefinitions());
        }
        _termBindings = getGuideOntology().getTermBindings();
        if (_termBindings == null) {
            _termBindings = new HashMap<>();
        }
        _guideOntology.setTermDefinitions(_termDefinitions);
        _guideOntology.setTermBindings(_termBindings);
        generateGTCodesForArchetypeBindings(guide);
        updateReadableGuide(guide);
        initResourceDescription();
        updateOriginal();
    }

    private void updateReadableGuide(Guide guide) {
        try {
            GuideImporter guideImporter = new GuideImporter(ArchetypeManager.getInstance());
            _readableGuide = guideImporter.importGuide(guide, getCurrentLanguageCode());
        } catch (InternalErrorException e) {
            throw new RuntimeException(e);
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
            return serializeGuide(guide);
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
                    term = getTermToDifferentLanguage(
                            gtCode,
                            originalTermDefinition.getTerms().get(gtCode),
                            getOriginalLanguageCode());
                    termDefinition.getTerms().put(gtCode, term);
                }
                if (term == null || term.getText() == null) {
                    term = getTermToDifferentLanguage(
                            gtCode,
                            getTermDefinition(currentGuideLanguageCode).getTerms().get(gtCode),
                            currentGuideLanguageCode);
                    termDefinition.getTerms().put(gtCode, term);
                }
            }
            currentGuideLanguageCode = language;
            getResourceDescriptionItem(language);
            EditorManager.getMainMenuBar().refreshLanguageMenu();
            updateReadableGuide(guide);
            updateRuleAtEdit();
            refreshCurrentTab();
        }
    }

    private void updateRuleAtEdit() {
        if (_ruleAtEdit != null) {
            _ruleAtEdit = _readableGuide.getReadableRules().get(_ruleAtEdit.getGTCode());
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
        if (comp instanceof RefreshablePanel && comp != _lastRefreshedPanel) {
            refreshablePanel = (RefreshablePanel) comp;
        } else {
            refreshablePanel = null;
        }
        runIfOkWithEditorState(comp, () -> SwingUtilities.invokeLater(() -> refreshPanel(refreshablePanel)));
    }

    private void refreshPanel(RefreshablePanel refreshablePanel) {
        if (refreshablePanel != null) {
            refreshablePanel.refresh();
            _lastRefreshedPanel = refreshablePanel;
        }
    }

    private void runIfOkWithEditorState(Runnable pendingRunnable) {
        Component comp = null;
        try {
            comp = getEditorPanel().getGuidePanel().getGuideEditorTabPane().getSelectedComponent();
        } catch (Exception e) {
            ExceptionHandler.handle(e);   //TODO
        }
        runIfOkWithEditorState(comp, pendingRunnable);
    }

    private void runIfOkWithEditorState(Component currentPanel, Runnable pendingRunnable) {
        if (_lastRefreshedPanel instanceof GDLPanel) {
            GDLPanel gdlPanel = (GDLPanel) _lastRefreshedPanel;
            String guideStr = gdlPanel.getGuideStr();
            new CheckGuideSW(this, guideStr, pendingRunnable).execute();
        } else if (currentPanel instanceof GDLPanel) {
            try {
                String guideStr = null;
                Guide guide = getEntity();
                if (guide != null) {
                    guideStr = serializeGuide(guide);
                }
                if (guideStr != null) {
                    new CheckGuideSW(this, guideStr, pendingRunnable).execute();
                } else {
                    loadLastTab();
                }
            } catch (Exception e) {
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
                            EditorManager.getActiveEditorWindow(),
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
        Component component = (Component) _lastRefreshedPanel;
        _gdlEditorMainPanel.getGuidePanel().getGuideEditorTabPane().setSelectedComponent(component);
    }

    private void initGuideVars() {
        guideId = UNKNOWN_GUIDE_ID;
        _resourceDescription = null;
        _originalAuthor = null;
        _details = null;
        _otherContributors = null;
        _otherDetails = null;
        _language = null;
        conceptGTCode = null;
        _guideOntology = null;
        _termDefinitions = null;
        _termBindings = null;
        _readableGuide = null;
        _ruleAtEdit = null;
    }

    private void generateGTCodesForArchetypeBindings(Guide guide) {
        if (guide.getDefinition() != null && guide.getDefinition().getArchetypeBindings() != null) {
            List<String> abCodes = new ArrayList<>();
            abCodes.addAll(guide.getDefinition().getArchetypeBindings().keySet());
            Collections.sort(abCodes);
            int gtNum = getNextTermNumber();
            for (String abCode : abCodes) {
                if (abCode.startsWith(GuideDefinition.ARCHETYPE_BINDING_PREFIX)) {
                    ArchetypeBinding archetypeBinding = guide.getDefinition().getArchetypeBindings().get(abCode);
                    String gtCode = GT_HEADER + StringUtils.leftPad("" + (gtNum++), 4, "0");
                    guide.getDefinition().getArchetypeBindings().remove(abCode);
                    archetypeBinding.setId(gtCode);
                    guide.getDefinition().getArchetypeBindings().put(gtCode, archetypeBinding);
                }
            }
        }
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
        EditorController controller = EditorManager.getActiveEditorController();
        String idGuide = controller.getEntityId();
        if (idGuide == null) {
            idGuide = GDLEditorLanguageManager.getMessage("Guide");
        }
        if (compiledGuide != null) {
            try {
                String guideSource = controller.getSerializedEntity();
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
                    int result = fileChooser.showSaveDialog(EditorManager
                            .getActiveEditorWindow());
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
                        } catch (Exception e) {
                            ExceptionHandler.handle(e);
                        }
                    }
                }
            } catch (Exception e) {
                ExceptionHandler.handle(e);
            }
        }
    }

    public ArchetypeInstantiationRuleLine addArchetypeReference(
            boolean showOnlyCDS) {
        ArchetypeInstantiationRuleLine airl = new ArchetypeInstantiationRuleLine();
        airl.setGTCode(createNextGTCode(false));
        RuleElementEditor.editArchetype(
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
        if (_infoDialog == null) {
            _infoDialog = new InfoDialog(EditorManager.getActiveEditorWindow());
        }
        return _infoDialog;
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
                    ArchetypeManager.getInstance().getArchetypes().getCMElement(archetypeId);
                } catch (InstanceNotFoundException e) {
                    File selectedFile = new File(archetypeId + ".adl");
                    int result = ImportUtils.showImportArchetypeDialogAndAddToRepo(EditorManager.getActiveEditorWindow(), selectedFile);
                    if (result == JFileChooser.CANCEL_OPTION) {
                        throw new InternalErrorException(new Exception("Archetype '" + archetypeId + "' not found."));
                    }
                }
            } else {
                try {
                    ArchetypeManager.getInstance().getTemplates().getCMElement(templateId);
                } catch (InstanceNotFoundException e) {
                    File selectedFile = new File(templateId + ".oet");
                    int result = ImportUtils.showImportTemplateDialog(
                            EditorManager.getActiveEditorWindow(), selectedFile);
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
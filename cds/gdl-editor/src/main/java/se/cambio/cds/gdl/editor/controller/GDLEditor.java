package se.cambio.cds.gdl.editor.controller;

import difflib.Delta;
import difflib.DiffUtils;
import difflib.Patch;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.formgen.controller.FormGeneratorController;
import se.cambio.cds.formgen.view.dialog.CDSFormGenDialog;
import se.cambio.cds.gdl.editor.controller.sw.CheckGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.CompileGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.SaveGuideOnFileRSW;
import se.cambio.cds.gdl.editor.util.DefinitionDependencyChecker;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogNameInsert;
import se.cambio.cds.gdl.editor.view.dialog.DialogTerminologyIdSelection;
import se.cambio.cds.gdl.editor.view.panels.GDLEditorMainPanel;
import se.cambio.cds.gdl.editor.view.panels.GDLPanel;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.util.GuideImporter;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
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

public class GDLEditor {

    private static String UNKOWN_GUIDE_ID = "unknown";
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
    private String _idGuide = UNKOWN_GUIDE_ID;
    private String _conceptGTCode;
    private String _currentGuideLanguageCode = null;
    private String _originalGuide = null;
    private RefreshablePanel _lastRefreshedPanel = null;

    private static String LANGUAGE_TERMINOLOGY = "ISO_639-1";
    private static String GT_HEADER = "gt";//TODO Link to model
    private static String GDL_VERSION = "0.1";
    private static String DRAFT = "Author draft";

    public GDLEditor(Guide guide) {
        try {
            setGuide(guide);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e); //TODO
        }
    }

    public void init() {
        EditorManager.getActiveEditorViewer().setContent(getEditorPanel());
        EditorManager.getMainMenuBar().refreshLanguageMenu();
    }

    public String getTitle() {
        String conceptName = getGTName(getConceptGTCode());
        if (conceptName != null) {
            return conceptName;
        } else {
            return GDLEditorLanguageManager.getMessage("Guide");
        }
    }

    public boolean checkRuleLineDelete(RuleLine ruleLine) {
        if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
            return checkArchetypeReferenceRemoval((ArchetypeInstantiationRuleLine) ruleLine);
        } else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine) {
            return checkArchetypeReferenceRemoval((ArchetypeElementInstantiationRuleLine) ruleLine);
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

    public void saveGuideAs(){
        _gdlEditorMainPanel.requestFocus();
        runIfOkWithEditorState(new Runnable() {
            @Override
            public void run() {
                new SaveGuideOnFileRSW(null).execute();
            }
        });
    }

    public void saveGuide() {
        _gdlEditorMainPanel.requestFocus();
        runIfOkWithEditorState(new Runnable() {
            @Override
            public void run() {
                new SaveGuideOnFileRSW(EditorManager.getLastFileLoaded()).execute();
            }
        });
    }

    public void generateForm(){
        _gdlEditorMainPanel.requestFocus();
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                if (!hasActiveRules()){
                    JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("PleaseInsertRulesBeforeGeneratingForm"), GDLEditorLanguageManager.getMessage("GenerateForm"), JOptionPane.WARNING_MESSAGE);
                    return;
                }
                String gdlGuide = serializeCurrentGuide();
                if (gdlGuide!=null){
                    CompileGuideSW sw = new CompileGuideSW(){
                        protected void done() {
                            getController().compilationFinished(getErrorMsg());
                            if (getErrorMsg()==null){
                                generateDialogForm(getCompiledGuide(), getGuide());
                            }
                        }
                    };
                    sw.execute();
                    setBusy(GDLEditorLanguageManager.getMessage("Compiling") + "...");
                }
            }
        };
        runIfOkWithEditorState(runnable);

    }

    private void generateDialogForm(byte[] compiledGuide, Guide guide){
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        String gdlGuide = controller.serializeCurrentGuide();
        if (compiledGuide!=null && gdlGuide!=null){
            GuideDTO guideDTO =
                    new GuideDTO(
                            controller.getIdGuide(),
                            gdlGuide,
                            IOUtils.getBytes(guide),
                            compiledGuide,
                            true,
                            Calendar.getInstance().getTime());
            FormGeneratorController formGenerator =
                    new FormGeneratorController(guideDTO, controller.getCurrentGuideLanguageCode());
            Date date = UserConfigurationManager.getCustomDate();
            if (date!=null){
                Calendar cal = Calendar.getInstance();
                cal.setTime(date);
                formGenerator.setCurrentDate(cal);
            }
            CDSFormGenDialog dialog = new CDSFormGenDialog(EditorManager.getActiveEditorWindow());
            dialog.setFormGeneratorController(formGenerator);
            //dialog.addWindowListener(new CloseAction());
            dialog.setVisible(true);
        }
    }

    public void compile() {
        new CompileGuideSW().execute();
        setBusy(GDLEditorLanguageManager.getMessage("Compiling"));
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
            _ruleAtEdit = new ReadableRule(getCurrentTermDefinition(),
                    createNextGTCode());
            setGTName(_ruleAtEdit.getGTCode(), dialog.getValue());
            getRenderableRules().put(_ruleAtEdit.getGTCode(), _ruleAtEdit);
        }
        return _ruleAtEdit;
    }

    public String createNewTerminologyBinding() {
        DialogTerminologyIdSelection dialog = new DialogTerminologyIdSelection(
                EditorManager.getActiveEditorWindow(), this);
        dialog.setVisible(true);
        String terminologyId = dialog.getSelectedObject();
        if (terminologyId != null) {
            getTermBindings().put(
                    terminologyId,
                    new TermBinding(terminologyId,
                            new HashMap<String, Binding>()));
        }
        return terminologyId;
    }

    public void ruleEdit(ReadableRule rule) {
        _ruleAtEdit = rule;
        getEditorPanel().loadRuleView(rule);
    }

    public LinkedHashMap<String, ReadableRule> getRenderableRules() {
        return getReadableGuide().getReadableRules();
    }

    public boolean hasActiveRules() {
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
            _resourceDescription.setLifecycleState(DRAFT);
            initResourceDescription();
        }
        return _resourceDescription;
    }

    private void initResourceDescription() {
        getOriginalAuthor();
        getOtherContributors();
        getResourceDescriptionItem(getCurrentGuideLanguageCode());
        getOtherDetails();
        getKeywords();
    }

    public Map<String, String> getOriginalAuthor() {
        if (_originalAuthor == null) {
            _originalAuthor = new HashMap<String, String>();
            getResourceDescription().setOriginalAuthor(_originalAuthor);
        }
        return _originalAuthor;
    }

    public List<String> getOtherContributors() {
        if (_otherContributors == null) {
            _otherContributors = new ArrayList<String>();
            getResourceDescription().setOtherContributors(_otherContributors);
        }
        return _otherContributors;
    }

    public Map<String, ResourceDescriptionItem> getDetails() {
        if (_details == null) {
            _details = new HashMap<String, ResourceDescriptionItem>();
            getResourceDescription().setDetails(_details);
        }
        return _details;
    }

    private ResourceDescriptionItem getResourceDescriptionItem(String lang){
        ResourceDescriptionItem resourceDescriptionItem = getDetails().get(lang);
        if (resourceDescriptionItem==null){
            resourceDescriptionItem = new ResourceDescriptionItem();
            getDetails().put(lang, resourceDescriptionItem);
        }
        return resourceDescriptionItem;
    }

    public Map<String, String> getOtherDetails() {
        if (_otherDetails == null) {
            _otherDetails = new HashMap<String, String>();
            getResourceDescription().setOtherDetails(_otherDetails);
        }
        return _otherDetails;
    }

    public ResourceDescriptionItem getResourceDescriptionItem() {
        ResourceDescriptionItem resourceDescriptionItem =
                getResourceDescription().getDetails().get(getCurrentGuideLanguageCode());
        if (resourceDescriptionItem == null) {
            resourceDescriptionItem = new ResourceDescriptionItem();
            getResourceDescription().getDetails().put(
                    getCurrentGuideLanguageCode(), resourceDescriptionItem);
        }
        return resourceDescriptionItem;
    }

    public List<String> getKeywords() {
        List<String> keywords = getResourceDescriptionItem().getKeywords();
        if (keywords == null) {
            keywords = new ArrayList<String>();
            getResourceDescriptionItem().setKeywords(keywords);
        }
        return keywords;
    }

    public List<RuleLine> getPreconditionRuleLines() {
        return getReadableGuide().getPreconditionRuleLines();
    }

    public List<RuleLine> getDefinitionRuleLines() {
        return getReadableGuide().getDefinitionRuleLines();
    }

    public ReadableGuide getReadableGuide() {
        if (_readableGuide == null) {
            _readableGuide = new ReadableGuide(getCurrentTermDefinition());
        }
        return _readableGuide;
    }

    public List<RuleLine> getConditionRuleLines() {
        return _ruleAtEdit.getConditionRuleLines();
    }

    public List<RuleLine> getActionsRuleLines() {
        return _ruleAtEdit.getActionRuleLines();
    }

    public void editRuleElement(
            RuleLineElementWithValue<?> ruleLineElementWithValue) {
        RuleElementEditor.edit(ruleLineElementWithValue);
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
                            JOptionPane.INFORMATION_MESSAGE);
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
                    }else if (response == JOptionPane.NO_OPTION) {
                        pendingRunnable.run();
                    }
                } else {
                    pendingRunnable.run();
                }
            }
        };
        runIfOkWithEditorState(runnable);
    }

    private GuideOntology getGuideOntology(){
        if (_guideOntology==null){
            _guideOntology = new GuideOntology();
            _guideOntology.setTermDefinitions(getTermDefinitions());
            _guideOntology.setTermBindings(getTermBindings());
        }
        return _guideOntology;
    }

    private Map<String, TermDefinition> getTermDefinitions() {
        if (_termDefinitions == null) {
            _termDefinitions = new HashMap<String, TermDefinition>();
            String lang = getOriginalLanguageCode();
            TermDefinition td = new TermDefinition();
            td.setTerms(new HashMap<String, Term>());
            _termDefinitions.put(lang, td);
        }
        return _termDefinitions;
    }

    public Map<String, TermBinding> getTermBindings() {
        if (_termBindings == null) {
            _termBindings = new HashMap<String, TermBinding>();
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
            termDefinition.setTerms(new HashMap<String, Term>());
            getTermDefinitions().put(language, termDefinition);
        }
        return termDefinition;
    }

    public TermDefinition getCurrentTermDefinition() {
        return getTermDefinition(getCurrentGuideLanguageCode());
    }

    private Language getLanguage() {
        if (_language == null) {
            _language = new Language(
                    new CodePhrase(LANGUAGE_TERMINOLOGY,
                            UserConfigurationManager.getLanguage()),
                    new HashMap<String, TranslationDetails>());
        }
        return _language;
    }

    public String getCurrentGuideLanguageCode() {
        if (_currentGuideLanguageCode == null) {
            String editorLanguage = UserConfigurationManager.getLanguage();
            if (getSupportedLanguageCodes().contains(editorLanguage)) {
                _currentGuideLanguageCode = editorLanguage;
            } else {
                _currentGuideLanguageCode = getOriginalLanguageCode();
            }
        }
        return _currentGuideLanguageCode;
    }

    public String getOriginalLanguageCode() {
        return getLanguage().getOriginalLanguage().getCodeString();
    }

    public void setCurrentGuideLanguageCode(String languageCode) {
        _currentGuideLanguageCode = languageCode;
    }

    public String createNextGTCode() {
        String gtCode = generateNextGTCode();
        getTerm(gtCode).setText(null);
        getTerm(gtCode).setDescription(null);
        return gtCode;
    }

    private String generateNextGTCode() {
        String nextGTCode = GT_HEADER
                + StringUtils.leftPad("" + (getNextTermNumber()), 4, "0");
        // Generate codes for all terminologies
        for (String langCode : getTermDefinitions().keySet()) {
            getTerm(langCode, nextGTCode);
        }
        return nextGTCode;
    }

    private int getNextTermNumber(){
        TermDefinition termDefinition = getTermDefinition(getLanguage()
                .getOriginalLanguage().getCodeString());
        int termCount = 0;
        if (termDefinition.getTerms() != null) {
            for (String gtCode : termDefinition.getTerms().keySet()) {
                try {
                    int codeNum = Integer.parseInt(gtCode.substring(2));
                    if (codeNum > termCount) {
                        termCount = codeNum;
                    }
                } catch (Exception e) {
                    Logger.getLogger(this.getClass()).warn(
                            "Unable to parse code '" + gtCode + "'");
                }
            }
        }
        return ++termCount;
    }

    public String getConceptGTCode() {
        if (_conceptGTCode == null) {
            _conceptGTCode = createNextGTCode();
        }
        return _conceptGTCode;
    }

    public Map<String, Term> getCurrentTermsMap() {
        return getTermsMap(getCurrentGuideLanguageCode());
    }

    public Map<String, Term> getTermsMap(String langCode) {
        Map<String, Term> terms = getTermDefinition(langCode).getTerms();
        if (terms == null) {
            terms = new HashMap<String, Term>();
            getTermDefinition(langCode).setTerms(terms);
        }
        return terms;
    }

    public Term getTerm(String gtCode) {
        return getTerm(getCurrentGuideLanguageCode(), gtCode);
    }

    public Term getTerm(String langCode, String gtCode) {
        Term term = getCurrentTermsMap().get(gtCode);
        if (term == null) {
            Term originalTerm = getTermDefinition(getOriginalLanguageCode())
                    .getTerms().get(gtCode);
            term = getTermToDifferenLanguage(gtCode, originalTerm,
                    getOriginalLanguageCode());
            getTermsMap(langCode).put(gtCode, term);
        }
        return getTermsMap(langCode).get(gtCode);
    }

    private static Term getTermToDifferenLanguage(String gtCode,
                                                  Term originalTerm, String originalLanguage) {
        Term newTerm = new Term();
        newTerm.setId(gtCode);
        String text = null;
        String description = null;
        if (originalTerm != null && originalTerm.getText() != null
                && !originalTerm.getText().isEmpty()) {
            text = "*" + originalTerm.getText() + " (" + originalLanguage + ")";
        }
        if (originalTerm != null && originalTerm.getDescription() != null
                && !originalTerm.getDescription().isEmpty()) {
            description = "*" + originalTerm.getDescription() + " ("
                    + originalLanguage + ")";
        }
        newTerm.setText(text);
        newTerm.setDescription(description);
        return newTerm;
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

    public String getGTDescription(String gtCode) {
        return getTerm(gtCode).getDescription();
    }

    private Guide constructCurrentGuide() throws IllegalStateException {
        GuideDefinition guideDefinition = new GuideDefinition(
                new ArrayList<ArchetypeBinding>(), new ArrayList<String>(),
                new HashMap<String, Rule>());
        // Insert definition
        for (RuleLine ruleLine : getDefinitionRuleLines()) {
            if (!ruleLine.isCommented()) {
                if (ruleLine instanceof ArchetypeInstantiationRuleLine) {
                    ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) ruleLine;
                    ArchetypeReference ar = airl.getArchetypeReference();
                    if (ar != null) {
                        Map<String, ElementBinding> elementMap = new HashMap<String, ElementBinding>();
                        List<ExpressionItem> predicateStatements = new ArrayList<ExpressionItem>();
                        for (RuleLine ruleLineAux : airl.getChildrenRuleLines()) {
                            if (!ruleLineAux.isCommented()) {
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
                        }
                        ArchetypeBinding archetypeBinding = new ArchetypeBinding();
                        // archetypeBinding.setId(archetypeGTCode);
                        archetypeBinding.setArchetypeId(ar.getIdArchetype());
                        archetypeBinding.setDomain(ar.getIdDomain());
                        archetypeBinding.setTemplateId(ar.getIdTemplate());
                        archetypeBinding.setElements(elementMap);
                        archetypeBinding.setPredicateStatements(predicateStatements);
                        guideDefinition.getArchetypeBindings().add(archetypeBinding);
                    }
                }
            }
        }

        // Insert preconditions
        List<ExpressionItem> preConditions = convertToExpressionItems(getPreconditionRuleLines());
        guideDefinition.setPreConditionExpressions(preConditions);

        // Insert rules
        int priority = getRenderableRules().size();
        for (ReadableRule renderableRule : getRenderableRules().values()) {
            if (!renderableRule.isCommented()) {
                String gtCode = renderableRule.getGTCode();
                Rule rule = new Rule();
                rule.setId(gtCode);
                guideDefinition.getRules().put(gtCode, rule);
                rule.setWhenStatements(convertToExpressionItems(renderableRule
                        .getConditionRuleLines()));
                rule.setThenStatements(convertToAssigmentExpressionItems(renderableRule
                        .getActionRuleLines()));
                rule.setPriority(priority--);
            }
        }
        Guide guide = new Guide();
        guide.setId(getIdGuide());
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

    public Guide getGuide() {
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

    private void updateInvalidData() {
        Set<String> codesStringSet = getTermBindings().keySet();
        for (String codeString : codesStringSet) {
            if (getTermBindings().get(codeString).getBindings() == null) {
                getTermBindings().get(codeString).setBindings(
                        new HashMap<String, Binding>());
            }
        }
    }

    public void updateOriginal() {
        _originalGuide = serializeCurrentGuide();
    }

    public String serializeCurrentGuide() {
        Guide guide = getGuide();
        if (guide != null) {
            return serializeGuide(guide);
        } else {
            return null;
        }
    }

    public static String serializeGuide(Guide guide) {
        try {
            return GuideUtil.serializeGuide(guide);
        } catch (Exception e) {
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
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    EditorManager.getActiveEditorWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorParsingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorParsingGuide"),
                    e.getMessage(), MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    public List<ExpressionItem> convertToExpressionItems(
            Collection<RuleLine> ruleLines) {
        List<ExpressionItem> list = new ArrayList<ExpressionItem>();
        for (RuleLine ruleLine : ruleLines) {
            if (!ruleLine.isCommented()) {
                ExpressionItem ei = ((ExpressionRuleLine) ruleLine)
                        .toExpressionItem();
                if (ei != null) {
                    list.add(ei);
                }
            }
        }
        return list;
    }

    public List<AssignmentExpression> convertToAssigmentExpressionItems(
            Collection<RuleLine> ruleLines) {
        List<AssignmentExpression> list = new ArrayList<AssignmentExpression>();
        for (RuleLine ruleLine : ruleLines) {
            if (!ruleLine.isCommented()) {
                list.add(((AssignmentExpressionRuleLine) ruleLine)
                        .toAssignmentExpression());
            }
        }
        return list;
    }

    public String getIdGuide() {
        return _idGuide;
    }

    public void setIdGuide(String idGuide) {
        _idGuide = idGuide;
    }

    public void changeLanguage(String language) {
        Guide guide = getGuide();
        if (guide != null) {
            TermDefinition originalTermDefinition = getTermDefinitions().get(getOriginalLanguageCode());
            TermDefinition termDefinition = getTermDefinition(language);
            for (String gtCode : originalTermDefinition.getTerms().keySet()) {
                Term term = termDefinition.getTerms().get(gtCode);
                if (term==null){
                    term = getTermToDifferenLanguage(
                            gtCode,
                            originalTermDefinition.getTerms().get(gtCode),
                            getOriginalLanguageCode());
                    termDefinition.getTerms().put(gtCode, term);
                }
            }
            _currentGuideLanguageCode = language;
            getResourceDescriptionItem(language);

            //Update definitions
            GuideImporter.updateTermDefinitions(_readableGuide, termDefinition);
            //GDLEditor editor = new GDLEditor(guide, language);
            //EditorManager.initController(this);
            EditorManager.getMainMenuBar().refreshLanguageMenu();
            refreshCurrentTab();
        }
    }

    public void refreshCurrentTab() {
        getEditorPanel().refresh();
    }

    public void tabChanged(Component comp){
        final RefreshablePanel refreshablePanel;
        if (comp instanceof RefreshablePanel && comp!=_lastRefreshedPanel){
            refreshablePanel = (RefreshablePanel)comp;
        }else{
            refreshablePanel = null;
        }
        runIfOkWithEditorState(comp, new Runnable() {
            @Override
            public void run() {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        refreshPanel(refreshablePanel);
                    }
                });
            }
        });
    }

    private void refreshPanel(RefreshablePanel refreshablePanel){
        if (refreshablePanel!=null){
            refreshablePanel.refresh();
            _lastRefreshedPanel = refreshablePanel;
        }
    }

    private void runIfOkWithEditorState(Runnable pendingRunnable){
        Component comp = null;
        try{
            comp = getEditorPanel().getGuidePanel().getGuideEditorTabPane().getSelectedComponent();
        }catch (Exception e){
            ExceptionHandler.handle(e);   //TODO
        }
        runIfOkWithEditorState(comp, pendingRunnable);
    }

    private void runIfOkWithEditorState(Component currentPanel, Runnable pendingRunnable){
        if (_lastRefreshedPanel instanceof GDLPanel/* &&
                _lastRefreshedPanel!=currentPanel*/){
            //Take care of possible changes made
            GDLPanel gdlPanel = (GDLPanel)_lastRefreshedPanel;
            String guideStr = gdlPanel.getGuideStr();
            new CheckGuideSW(this, guideStr, pendingRunnable).execute();
        }else{
            pendingRunnable.run();
        }
    }

    public void gdlEditingChecked(Guide guide, boolean checkOk, Runnable pendingRunnable){
//        setFree();
        if (checkOk){
            String auxOriginalGuide = _originalGuide;
            try {
                setGuide(guide);
                _originalGuide = auxOriginalGuide;
                if (pendingRunnable!=null){
                    pendingRunnable.run();
                }
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }else{
            int answer = JOptionPane.showConfirmDialog(
                    EditorManager.getActiveEditorWindow(),
                    GDLEditorLanguageManager.getMessage("IgnoreGDLSourceChanges"),
                    GDLEditorLanguageManager.getMessage("IgnoreGDLSourceChangesTitle"),
                    JOptionPane.YES_NO_OPTION);
            if (answer==JOptionPane.YES_OPTION){
                if (pendingRunnable!=null){
                    pendingRunnable.run();
                }
            }else{
                GDLPanel gdlPanel = (GDLPanel)_lastRefreshedPanel;
                _gdlEditorMainPanel.getGuidePanel().getGuideEditorTabPane().setSelectedComponent(gdlPanel);
            }
        }
    }

    private void initGuideVars(){
        _idGuide = UNKOWN_GUIDE_ID;
        _resourceDescription = null;
        _originalAuthor = null;
        _details = null;
        _otherContributors = null;
        _otherDetails = null;
        _language = null;
        _conceptGTCode = null;
        _guideOntology = null;
        _termDefinitions = null;
        _termBindings = null;
        _readableGuide = null;
        _ruleAtEdit = null;
    }

    public boolean setGuide(Guide guide) throws InternalErrorException {
        try {
            checkGuideArchetypesAndTemplates(guide);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
            return false;
        }
        initGuideVars();
        if (guide.getId()!=null){
            _idGuide = guide.getId();
        }
        _resourceDescription = guide.getDescription();
        if (_resourceDescription != null) {
            _originalAuthor = _resourceDescription.getOriginalAuthor();
            _details = _resourceDescription.getDetails();
            _otherContributors = _resourceDescription.getOtherContributors();
            _otherDetails = _resourceDescription.getOtherDetails();
        }
        _language = guide.getLanguage();
        _conceptGTCode = guide.getConcept();
        _guideOntology = guide.getOntology();
        _termDefinitions = getGuideOntology().getTermDefinitions();
        if (_termDefinitions==null){
            _termDefinitions = new HashMap<String, TermDefinition>();
        }else{
            check(getOriginalLanguageCode(), getTermDefinitions());
        }
        _termBindings = getGuideOntology().getTermBindings();
        if (_termBindings==null){
            _termBindings = new HashMap<String, TermBinding>();
        }
        _guideOntology.setTermDefinitions(_termDefinitions);
        _guideOntology.setTermBindings(_termBindings);
        _readableGuide =
                GuideImporter.importGuide(guide, getCurrentGuideLanguageCode());
        initResourceDescription();
        updateOriginal();
        return true;
    }

    // Check if all elements inside map have the same amount of gtcodes as the
    // ones in the original language, if not, add them
    private static void check(String originalLang,
                              Map<String, TermDefinition> termDefinitionMap) {
        TermDefinition originalTermDefinition =
                termDefinitionMap.get(originalLang);
        for (String langCode : termDefinitionMap.keySet()) {
            if (!langCode.equals(originalLang)) {
                TermDefinition td = termDefinitionMap.get(langCode);
                for (String gtCode : originalTermDefinition.getTerms().keySet()) {
                    if (!td.getTerms().containsKey(gtCode)) {
                        Logger.getLogger(GDLEditor.class).warn(
                                "Language '" + langCode
                                        + "' does not contain gt code '"
                                        + gtCode + "'. Will be added...");
                        Term term = getTermToDifferenLanguage(gtCode,
                                originalTermDefinition.getTerms().get(gtCode),
                                originalLang);
                        td.getTerms().put(gtCode, term);
                    }
                }
            }
        }
    }

    public boolean isModified() {
        String serializedGuide = null;
        try {
            Guide guide = constructCurrentGuide();
            serializedGuide = GuideUtil.serializeGuide(guide);
        } catch (Exception e) {
            // Guide has errors, so it will trigger as modified (serializedGuide
            // = null)
            // If user decides to save the guide, errors will be displayed (if
            // not, ignored)
        }
        if (_originalGuide != null && !_originalGuide.equals(serializedGuide)) {
            return true;
        } else {
            return false;
        }
    }

    public void cleanRedundantTerms() {
        Collection<String> gtCodesUsed = getGTCodesUsedInGuide();
        for (TermDefinition termDefinition : getTermDefinitions().values()) {
            Collection<Term> terms = new ArrayList<Term>(termDefinition
                    .getTerms().values());
            for (Term term : terms) {
                if (!gtCodesUsed.contains(term.getId())) {
                    termDefinition.getTerms().remove(term.getId());
                }
            }
        }
    }

    public Collection<String> getGTCodesUsedInGuide() {
        TermDefinition td =
                getTermDefinitions().get(getCurrentGuideLanguageCode());
        if (td != null) {
            return td.getTerms().keySet();
        } else {
            return Collections.emptyList();
        }
    }

    public Collection<String> getGTCodesUsedInDefinitions() {
        Collection<String> gtCodes = new ArrayList<String>();
        gtCodes.add(getConceptGTCode());
        gtCodes.addAll(getRenderableRules().keySet());
        /* TODO Allow reuse of archetype element codes
        for (RuleLine ruleLine : getDefinitionRuleLines()) {
            for (RuleLine ruleLineAux : ruleLine.getChildrenRuleLines()) {
                if (ruleLineAux instanceof ArchetypeElementInstantiationRuleLine) {
                    gtCodes.add(((ArchetypeElementInstantiationRuleLine) ruleLineAux)
                            .getGTCode());
                }
            }
        }*/
        return gtCodes;
    }

    public Collection<String> getGTCodesUsedInBindings() {
        Collection<String> gtCodes = new HashSet<String>();
        for (TermBinding termBinding : getTermBindings().values()) {
            gtCodes.addAll(termBinding.getBindings().keySet());
        }
        return gtCodes;
    }

    public void saveCompiledGuideAsObject(byte[] compiledGuide, Guide guide) {
        GDLEditor controller = EditorManager.getActiveGDLEditor();
        String idGuide = controller.getIdGuide();
        if (idGuide == null) {
            idGuide = GDLEditorLanguageManager.getMessage("Guide");
        }
        if (compiledGuide != null) {
            try {
                String guideSource = controller.serializeCurrentGuide();
                if (guideSource != null) {
                    JFileChooser fileChooser = new JFileChooser();
                    FileNameExtensionFilter filter = new FileNameExtensionFilter(
                            GDLEditorLanguageManager.getMessage("Guide"),
                            new String[] { "guide" });
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
                        GuideDTO guideDTO = new GuideDTO(idGuide,
                                guideSource,
                                IOUtils.getBytes(guide),
                                compiledGuide,
                                true,
                                Calendar.getInstance().getTime());
                        ObjectOutputStream output = new ObjectOutputStream(
                                new BufferedOutputStream(new FileOutputStream(
                                        guideFile)));
                        try {
                            output.writeObject(guideDTO);
                        } catch (Exception e) {
                            ExceptionHandler.handle(e);
                        } finally {
                            output.close();
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
        // airl.getGTCodeRuleLineElement().setValue(EditorManager.getActiveGDLEditor().createNextGTCode());
        airl.setTermDefinition(getCurrentTermDefinition());
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
        ArchetypeElementInstantiationRuleLine aeirl = new ArchetypeElementInstantiationRuleLine(
                airl);
        aeirl.getGTCodeRuleLineElement().setValue(
                EditorManager.getActiveGDLEditor().createNextGTCode());
        aeirl.setTermDefinition(getCurrentTermDefinition());
        editRuleElement(aeirl.getArchetypeElementRuleLineDefinitionElement());
        if (aeirl.getArchetypeElementRuleLineDefinitionElement().getValue() != null) {
            return aeirl;
        } else {
            aeirl.detachFromParent();
            return null;
        }
    }

    public void setBusy(String description) {
        // getEditorPanel().setBusy(true);
        getInfoDialog().changeLoadingText(description);
        getInfoDialog().start();
    }

    public void changeBusyText(String description) {
        getInfoDialog().changeLoadingText(description);
    }

    private InfoDialog getInfoDialog() {
        if (_infoDialog == null) {
            _infoDialog = new InfoDialog(EditorManager.getActiveEditorWindow());
        }
        return _infoDialog;
    }

    public void setFree() {
        getInfoDialog().stop();
    }

    private void checkGuideArchetypesAndTemplates(Guide guide)
            throws InternalErrorException {
        if (guide == null || guide.getDefinition() == null
                || guide.getDefinition().getArchetypeBindings() == null) {
            return;
        }
        for (ArchetypeBinding archetypeBinding : guide.getDefinition()
                .getArchetypeBindings()) {
            String archetypeId = archetypeBinding.getArchetypeId();
            String templateId = archetypeBinding.getTemplateId();
            if (templateId == null) {
                if (Archetypes.getArchetypeDTO(archetypeId) == null) {
                    int result = ImportUtils.showImportArchetypeDialogAndAddToRepo(
                            EditorManager.getActiveEditorWindow(), new File(
                            archetypeId + ".adl"));
                    if (result == JFileChooser.CANCEL_OPTION) {
                        throw new InternalErrorException(new Exception(
                                "Archetype '" + archetypeId + "' not found."));
                    }
                }
            } else {
                if (Templates.getTemplateDTO(templateId) == null) {
                    int result = ImportUtils.showImportTemplateDialog(
                            EditorManager.getActiveEditorWindow(), new File(
                            templateId + ".oet"));
                    if (result == JFileChooser.CANCEL_OPTION) {
                        throw new InternalErrorException(new Exception(
                                "Template '" + templateId + "' not found."));
                    }
                }
            }
        }
    }
    public static boolean checkParsedGuide(String guideSrc, Guide guide){
        String guideSrcAux = GDLEditor.serializeGuide(guide);
        if (guide!=null){
            if (guideSrc.equals(guideSrcAux)){
                return true;
            }else{
                Patch patch = DiffUtils.diff(stringToLines(guideSrc), stringToLines(guideSrcAux));

                StringBuffer diff = new StringBuffer();

                for (Delta delta : patch.getDeltas()) {
                    diff.append("-------------------------------------\n");
                    diff.append(" line:"+delta.getOriginal().getPosition()+1+"\n");
                    diff.append(" original:"+delta.getOriginal().getLines()+"\n");
                    diff.append(" revised:"+delta.getRevised().getLines()+"\n");
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
                boolean result = dialog.getAnswer();
                if (result){
                    return true;
                }else{
                    return false;
                }
            }
        }else{
            return false;
        }
    }

    private static List<String> stringToLines(String str) {
        final List<String> lines = new ArrayList<String>();
        for (String string : str.split("\n")) {
            lines.add(string.trim());
        }
        return lines;
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
package se.cambio.cds.gdl.editor.controller;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.formgen.view.dialog.InfoDialog;
import se.cambio.cds.gdl.editor.controller.sw.CompileGuideSW;
import se.cambio.cds.gdl.editor.controller.sw.SaveGuideOnFileRSW;
import se.cambio.cds.gdl.editor.util.DefinitionDependencyChecker;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.dialog.DialogNameInsert;
import se.cambio.cds.gdl.editor.view.dialog.DialogTerminologyIdSelection;
import se.cambio.cds.gdl.editor.view.panels.GDLEditorMainPanel;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.Binding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.GuideDefinition;
import se.cambio.cds.gdl.model.GuideOntology;
import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermBinding;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.TranslationDetails;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.readable.GuideImporter;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.AssignmentExpressionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ExpressionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.WithElementPredicateAttributeDefinitionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.WithElementPredicateExpressionDefinitionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.util.ExceptionHandler;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.openehr.view.applicationobjects.Templates;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice.MessageType;
import se.cambio.cds.openehr.view.util.ImportUtils;
import se.cambio.cds.util.exceptions.InternalErrorException;

public class GDLEditor {

    private GDLEditorMainPanel _gdlEditorMainPanel = null;
    private ResourceDescription _resourceDescription = null;
    private ReadableGuide _readableGuide = null;
    private ReadableRule _ruleAtEdit = null;
    private Map<String, TermDefinition> _termDefinitions = null;
    private Map<String, TermBinding> _termBindings = null;
    private int _termCount = 0;
    private Language _language;
    private Map<String, String> _originalAuthor;
    private List<String> _otherContributors;
    private Map<String, ResourceDescriptionItem> _details;
    private Map<String, String> _otherDetails;
    private static String LANGUAGE_TERMINOLOGY = "ISO_639-1";
    private InfoDialog _infoDialog;
    private String _idGuide="unkown";
    private String _conceptGTCode;
    private String _currentGuideLanguageCode = null;
    private String _originalGuide = null;

    private static String GT_HEADER = "gt";//TODO Link to model
    private static String GDL_VERSION = "0.1";
    private static String DRAFT = "Author draft";

    public GDLEditor(){
	updateOriginal();
    }

    public GDLEditor(Guide guide){
	setGuide(guide);
    }

    public void init(){
	EditorManager.getActiveEditorViewer().setContent(getEditorPanel());
	EditorManager.getMainMenuBar().refreshLanguageMenu();
    }

    public String getTitle(){
	String conceptName = getGTName(getConceptGTCode());
	if (conceptName!=null){
	    return conceptName;
	}else{
	    return LanguageManager.getMessage("Guide");
	}
    }

    public boolean checkRuleLineDelete(RuleLine ruleLine){
	if (ruleLine instanceof ArchetypeInstantiationRuleLine){
	    return checkArchetypeReferenceRemoval((ArchetypeInstantiationRuleLine)ruleLine);
	}else if (ruleLine instanceof ArchetypeElementInstantiationRuleLine){
	    return checkArchetypeReferenceRemoval((ArchetypeElementInstantiationRuleLine)ruleLine);
	}else{
	    return true;
	}
    }

    private boolean checkArchetypeReferenceRemoval(ArchetypeInstantiationRuleLine airl){
	if (DefinitionDependencyChecker.isBeingUsed(airl, this)){
	    JOptionPane.showMessageDialog(
		    EditorManager.getActiveEditorWindow(),
		    LanguageManager.getMessage("ReferenceBeingUsedMsg"),
		    LanguageManager.getMessage("ReferenceBeingUsedTitle"),
		    JOptionPane.WARNING_MESSAGE);
	    return false;
	}else{
	    return true;
	}
    }

    private boolean checkArchetypeReferenceRemoval(ArchetypeElementInstantiationRuleLine aeirl){
	if (DefinitionDependencyChecker.isBeingUsed(aeirl, this)){
	    JOptionPane.showMessageDialog(
		    EditorManager.getActiveEditorWindow(),
		    LanguageManager.getMessage("ReferenceBeingUsedMsg"),
		    LanguageManager.getMessage("ReferenceBeingUsedTitle"),
		    JOptionPane.WARNING_MESSAGE);
	    return false;
	}else{
	    return true;
	}
    }

    public GDLEditorMainPanel getEditorPanel(){
	if (_gdlEditorMainPanel==null){
	    _gdlEditorMainPanel = new GDLEditorMainPanel(this);
	}
	return _gdlEditorMainPanel;
    }


    public void saveGuide(){
	_gdlEditorMainPanel.requestFocus();
	new SaveGuideOnFileRSW(EditorManager.getLastFileLoaded()).execute();
    }

    public void compile(){
	new CompileGuideSW(this).execute();
	setBusy(LanguageManager.getMessage("Compiling"));
    }

    public void compilationFinished(String msg){	
	setFree();
	if (msg!=null){
	    JOptionPane.showMessageDialog(
		    EditorManager.getActiveEditorWindow(), msg, LanguageManager.getMessage("Error"),JOptionPane.ERROR_MESSAGE);
	}
    }

    public ReadableRule createNewRule(){
	_ruleAtEdit = null;
	DialogNameInsert dialog = 
		new DialogNameInsert(
			EditorManager.getActiveEditorWindow(), 
			LanguageManager.getMessage("RuleName"), 
			"");
	if (dialog.getAnswer()){
	    _ruleAtEdit = new ReadableRule(getCurrentTermDefinition(), createNextGTCode());
	    setGTName(_ruleAtEdit.getGTCode(),dialog.getValue());
	    getRenderableRules().put(_ruleAtEdit.getGTCode(), _ruleAtEdit);
	}
	return _ruleAtEdit;
    }

    public String createNewTerminology() {
	DialogTerminologyIdSelection dialog = 
		new DialogTerminologyIdSelection(EditorManager.getActiveEditorWindow(), this);
	dialog.setVisible(true);
	String terminologyId = dialog.getSelectedObject();
	if (terminologyId!=null){
	    getTermBindings().put(terminologyId, new TermBinding(terminologyId, new HashMap<String, Binding>()));
	}
	return terminologyId;
    }

    public void ruleEdit(ReadableRule rule){
	_ruleAtEdit = rule;
	getEditorPanel().loadRuleView(rule);
    }

    public LinkedHashMap<String, ReadableRule> getRenderableRules(){
	return getReadableGuide().getReadableRules();
    }

    public boolean hasActiveRules(){
	boolean hasActiveRules = false;
	Iterator<ReadableRule> i = getRenderableRules().values().iterator();
	while(i.hasNext() && !hasActiveRules){
	    hasActiveRules = !i.next().isCommented();
	}
	return hasActiveRules;
    }

    public void changeCommentRule(ReadableRule rule, boolean comment){
	rule.setCommented(comment);
    }

    public void goBackToGuide(){
	getEditorPanel().loadGuideView();
    }

    public ResourceDescription getResourceDescription(){
	if (_resourceDescription==null){
	    _resourceDescription =  new ResourceDescription();
	    _resourceDescription.setLifecycleState(DRAFT);
	    initResourceDescription();
	}
	return _resourceDescription;
    }


    private void initResourceDescription(){
	getOriginalAuthor();
	getOtherContributors();
	getDetails();
	getOtherDetails();
	getKeywords();
    }

    public Map<String, String> getOriginalAuthor(){
	if (_originalAuthor==null){
	    _originalAuthor = new HashMap<String, String>();
	    getResourceDescription().setOriginalAuthor(_originalAuthor);
	}
	return _originalAuthor;
    }

    public List<String> getOtherContributors(){
	if (_otherContributors==null){
	    _otherContributors = new ArrayList<String>();
	    getResourceDescription().setOtherContributors(_otherContributors);
	}
	return _otherContributors;
    }

    public Map<String,ResourceDescriptionItem> getDetails(){
	if (_details==null){
	    _details = new HashMap<String,ResourceDescriptionItem>();
	    ResourceDescriptionItem resourceDescriptionItem = new ResourceDescriptionItem();
	    _details.put(getCurrentGuideLanguageCode(), resourceDescriptionItem);
	    getResourceDescription().setDetails(_details);
	}
	return _details;
    }

    public Map<String,String> getOtherDetails(){
	if (_otherDetails==null){
	    _otherDetails = new HashMap<String,String>();
	    getResourceDescription().setOtherDetails(_otherDetails);
	}
	return _otherDetails;
    }

    public ResourceDescriptionItem getResourceDescriptionItem(){
	ResourceDescriptionItem resourceDescriptionItem = getResourceDescription().getDetails().get(getCurrentGuideLanguageCode());
	if (resourceDescriptionItem==null){
	    resourceDescriptionItem = new ResourceDescriptionItem();
	    getResourceDescription().getDetails().put(getCurrentGuideLanguageCode(), resourceDescriptionItem);
	}
	return resourceDescriptionItem;
    }

    public List<String> getKeywords(){
	List<String> keywords = getResourceDescriptionItem().getKeywords();
	if (keywords==null){
	    keywords = new ArrayList<String>();
	    getResourceDescriptionItem().setKeywords(keywords);
	}
	return keywords;
    }

    public List<RuleLine> getPreconditionRuleLines(){
	return getReadableGuide().getPreconditionRuleLines();
    }

    public List<RuleLine> getDefinitionRuleLines(){
	return getReadableGuide().getDefinitionRuleLines();
    }

    public ReadableGuide getReadableGuide(){
	if (_readableGuide==null){
	    _readableGuide = new ReadableGuide(getCurrentTermDefinition());
	}
	return _readableGuide;
    }

    public List<RuleLine> getConditionRuleLines(){
	return _ruleAtEdit.getConditionRuleLines();
    }

    public List<RuleLine> getActionsRuleLines(){
	return _ruleAtEdit.getActionRuleLines();
    }
    public void editRuleElement(RuleLineElementWithValue<?> ruleLineElementWithValue){
	RuleElementEditor.edit(ruleLineElementWithValue);
    }

    public boolean isOKToExit(){
	if (isModified()){
	    int response = JOptionPane.showConfirmDialog(EditorManager.getActiveEditorWindow(), LanguageManager.getMessage("SavingChangesMessage"), LanguageManager.getMessage("SavingChanges"), JOptionPane.INFORMATION_MESSAGE);
	    if (response==JOptionPane.CANCEL_OPTION){
		return false;
	    }else{
		if (response==JOptionPane.YES_OPTION){
		    SaveGuideOnFileRSW rsw = new SaveGuideOnFileRSW(EditorManager.getLastFileLoaded()){
			protected void done() {
			    super.done();
			    if (getFile()!=null){
				EditorManager.closeEditor();
			    }
			}
		    };
		    rsw.execute();
		    return false;
		}
		return true;
	    }
	}else{
	    return true;
	}
    }

    private Map<String, TermDefinition> getTermDefinitions(){
	if (_termDefinitions==null){
	    _termDefinitions = new HashMap<String, TermDefinition>();
	}
	return _termDefinitions;
    }

    public Map<String, TermBinding> getTermBindings(){
	if (_termBindings==null){
	    _termBindings = new HashMap<String, TermBinding>();
	}
	return _termBindings;
    }

    public Collection<String> getSupportedLanguageCodes(){
	return getTermDefinitions().keySet();
    }

    private TermDefinition getTermDefinition(String language){
	TermDefinition termDefinition = getTermDefinitions().get(language);
	if (termDefinition==null){
	    termDefinition = new TermDefinition();
	    termDefinition.setId(language);
	    termDefinition.setTerms(new HashMap<String, Term>());
	    getTermDefinitions().put(language, termDefinition);
	}
	return termDefinition;
    }

    public TermDefinition getCurrentTermDefinition(){
	return getTermDefinition(getCurrentGuideLanguageCode());
    }

    private Language getLanguage(){
	if (_language==null){
	    _language =new Language(
		    new CodePhrase(LANGUAGE_TERMINOLOGY, LanguageManager.getLanguage()),
		    new HashMap<String, TranslationDetails>());
	}
	return _language;
    }

    public String getCurrentGuideLanguageCode(){
	if (_currentGuideLanguageCode==null){
	    String editorLanguage = LanguageManager.getLanguage();
	    if (getSupportedLanguageCodes().contains(editorLanguage)){
		_currentGuideLanguageCode = editorLanguage;
	    }else{
		_currentGuideLanguageCode = getLanguage().getOriginalLanguage().getCodeString();
	    }
	}
	return _currentGuideLanguageCode;
    }

    public String getOriginalLanguageCode(){
	return getLanguage().getOriginalLanguage().getCodeString();
    }

    public void setCurrentGuideLanguageCode(String languageCode){
	_currentGuideLanguageCode = languageCode;
    }

    public String createNextGTCode(){
	Collection<String> gtCodesInUse = getGTCodesUsedInGuide();
	String gtCode = generateNextGTCode();
	if(gtCodesInUse.contains(gtCode)){
	    _termCount++;
	    gtCode = generateNextGTCode();
	    getTerm(gtCode);
	}else{
	    getTerm(gtCode).setText(null);
	    getTerm(gtCode).setDescription(null);
	}

	return gtCode;
    }

    private String generateNextGTCode(){
	return GT_HEADER+StringUtils.leftPad(""+(_termCount), 4, "0");
    }

    public String getConceptGTCode(){
	if (_conceptGTCode==null){
	    _conceptGTCode = createNextGTCode();
	}
	return _conceptGTCode;
    }


    public Map<String, Term> getCurrentTermsMap(){
	Map<String, Term> terms = getCurrentTermDefinition().getTerms();
	if (terms==null){
	    terms = new HashMap<String, Term>();
	    getCurrentTermDefinition().setTerms(terms);
	}
	return terms;
    }

    public Term getTerm(String gtCode){
	Term term = getCurrentTermsMap().get(gtCode);
	if (term==null){
	    Term originalTerm = getTermDefinition(getOriginalLanguageCode()).getTerms().get(gtCode);
	    term = getTermToDifferenLanguage(gtCode, originalTerm, getOriginalLanguageCode());
	    getCurrentTermsMap().put(gtCode, term);
	}
	return getCurrentTermsMap().get(gtCode);
    }

    private static Term getTermToDifferenLanguage(String gtCode, Term originalTerm, String originalLanguage){
	Term newTerm = new Term();
	newTerm.setId(gtCode);
	String text = null;
	String description = null;
	if (originalTerm!=null && originalTerm.getText()!=null && !originalTerm.getText().isEmpty()){
	    text = "*"+originalTerm.getText()+" ("+originalLanguage+")";
	}
	if (originalTerm!=null && originalTerm.getDescription()!=null && !originalTerm.getDescription().isEmpty()){
	    description = "*"+originalTerm.getDescription()+" ("+originalLanguage+")";
	}
	newTerm.setText(text);
	newTerm.setDescription(description);
	return newTerm;
    }

    public Term getConceptTerm(){
	return getCurrentTermsMap().get(getConceptGTCode());
    }

    public String getGTName(String gtCode){
	return getTerm(gtCode).getText();
    }

    public void setGTName(String gtCode, String text){
	Term term = getTerm(gtCode);
	term.setText(text);
    }

    public String getGTDescription(String gtCode){
	return getTerm(gtCode).getDescription();
    }

    private Guide constructCurrentGuide() throws IllegalStateException{
	GuideDefinition guideDefinition = 
		new GuideDefinition(
			new ArrayList<ArchetypeBinding>(), 
			new ArrayList<String>(), 
			new HashMap<String, Rule>());
	//Insert definition
	for (RuleLine ruleLine : getDefinitionRuleLines()) {
	    if (!ruleLine.isCommented()){
		if (ruleLine instanceof ArchetypeInstantiationRuleLine){
		    ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine)ruleLine;
		    ArchetypeReference ar = airl.getArchetypeReference();
		    if (ar!=null){
			Map<String, ElementBinding> elementMap = new HashMap<String, ElementBinding>();
			List<ExpressionItem> predicateStatements = new ArrayList<ExpressionItem>();
			for (RuleLine ruleLineAux : airl.getChildrenRuleLines()) {
			    if (!ruleLineAux.isCommented()){
				if (ruleLineAux instanceof ArchetypeElementInstantiationRuleLine){
				    ArchetypeElementInstantiationRuleLine aeirl = 
					    (ArchetypeElementInstantiationRuleLine) ruleLineAux;
				    ArchetypeElementVO archetypeElementVO = 
					    aeirl.getArchetypeElementRuleLineDefinitionElement().getValue();
				    String elementGTCode = aeirl.getGTCode();
				    if (archetypeElementVO!=null){
					ElementBinding element = 
						new ElementBinding(
							elementGTCode, 
							archetypeElementVO.getPath());
					elementMap.put(elementGTCode, element);
				    }
				}else if (ruleLineAux instanceof WithElementPredicateAttributeDefinitionRuleLine){
				    WithElementPredicateAttributeDefinitionRuleLine wepadrl =
					    (WithElementPredicateAttributeDefinitionRuleLine)ruleLineAux;
				    predicateStatements.add(wepadrl.toExpressionItem());
				}else if (ruleLineAux instanceof WithElementPredicateExpressionDefinitionRuleLine){
				    WithElementPredicateExpressionDefinitionRuleLine wepedrl =
					    (WithElementPredicateExpressionDefinitionRuleLine)ruleLineAux;
				    predicateStatements.add(wepedrl.toExpressionItem());
				}
			    }
			}
			String af = ar.getAggregationFunction();
			ArchetypeBinding archetypeBinding = new ArchetypeBinding();
			//archetypeBinding.setId(archetypeGTCode);
			archetypeBinding.setArchetypeId(ar.getIdArchetype());
			archetypeBinding.setDomain(ar.getIdDomain());
			archetypeBinding.setTemplateId(ar.getIdTemplate());
			archetypeBinding.setFunction(af);
			archetypeBinding.setElements(elementMap);
			archetypeBinding.setPredicateStatements(predicateStatements);
			guideDefinition.getArchetypeBindings().add(archetypeBinding);
		    }
		}
	    }
	}

	//Insert preconditions
	List<ExpressionItem> preConditions = convertToExpressionItems(getPreconditionRuleLines());
	guideDefinition.setPreConditionExpressions(preConditions);

	//Insert rules
	int priority = getRenderableRules().size();
	for (ReadableRule renderableRule : getRenderableRules().values()) {
	    if (!renderableRule.isCommented()){
		String gtCode = renderableRule.getGTCode();
		Rule rule = new Rule();
		rule.setId(gtCode);
		guideDefinition.getRules().put(gtCode, rule);
		rule.setWhenStatements(convertToExpressionItems(renderableRule.getConditionRuleLines()));
		rule.setThenStatements(convertToAssigmentExpressionItems(renderableRule.getActionRuleLines()));
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

	if (!getTermDefinitions().isEmpty()){
	    cleanRedundantTerms();
	    if(guide.getOntology() == null) {
		guide.setOntology(new GuideOntology());
	    }
	    guide.getOntology().setTermDefinitions(getTermDefinitions());
	}
	if (!getTermBindings().isEmpty()){
	    cleanRedundantTerms();
	    updateInvalidData();
	    if(guide.getOntology() == null) {
		guide.setOntology(new GuideOntology());
	    }
	    guide.getOntology().setTermBindings(getTermBindings());
	}
	return guide;
    }

    public Guide getGuide(){
	try{
	    return constructCurrentGuide();
	}catch(IllegalStateException e){
	    DialogLongMessageNotice dialog = 
		    new DialogLongMessageNotice(
			    EditorManager.getActiveEditorWindow(),
			    LanguageManager.getMessage("ErrorSerializingGuideT"),
			    LanguageManager.getMessage("ErrorSerializingGuide"),
			    e.getMessage(),
			    MessageType.ERROR
			    );
	    dialog.setVisible(true);
	    return null;
	}
    }

    private void updateInvalidData(){
	Set<String> codesStringSet = getTermBindings().keySet();
	for (String codeString : codesStringSet) {
	    if (getTermBindings().get(codeString).getBindings() == null) {
		getTermBindings().get(codeString).setBindings(new HashMap<String, Binding>());
	    }
	}
    }

    public void updateOriginal(){
	_originalGuide = serializeCurrentGuide();
    }

    public String serializeCurrentGuide(){
	Guide guide = getGuide();
	if (guide!=null){
	    return serializeGuide(guide);
	}else{
	    return null;
	}
    }

    public static String serializeGuide(Guide guide){
	try {  
	    return GuideUtil.serializeGuide(guide);
	} catch (Exception e) {
	    DialogLongMessageNotice dialog = 
		    new DialogLongMessageNotice(
			    EditorManager.getActiveEditorWindow(),
			    LanguageManager.getMessage("ErrorSerializingGuideT"),
			    LanguageManager.getMessage("ErrorSerializingGuide"),
			    e.getMessage(),
			    MessageType.ERROR
			    );
	    dialog.setVisible(true);
	    return null;
	}
    }

    public static Guide parseGuide(InputStream input){
	try {
	    return GuideUtil.parseGuide(input);
	} catch (Exception e) {
	    DialogLongMessageNotice dialog = 
		    new DialogLongMessageNotice(
			    EditorManager.getActiveEditorWindow(),
			    LanguageManager.getMessage("ErrorParsingGuideT"),
			    LanguageManager.getMessage("ErrorParsingGuide"),
			    e.getMessage(),
			    MessageType.ERROR
			    );
	    dialog.setVisible(true);
	    return null;
	}
    }

    public List<ExpressionItem> convertToExpressionItems(Collection<RuleLine> ruleLines){
	List<ExpressionItem> list = new ArrayList<ExpressionItem>();
	for (RuleLine ruleLine : ruleLines) {
	    if (!ruleLine.isCommented()){
		ExpressionItem ei = ((ExpressionRuleLine)ruleLine).toExpressionItem();
		if (ei!=null){
		    list.add(ei);
		}
	    }
	}
	return list;
    }

    public List<AssignmentExpression> convertToAssigmentExpressionItems(Collection<RuleLine> ruleLines){
	List<AssignmentExpression> list = new ArrayList<AssignmentExpression>();
	for (RuleLine ruleLine : ruleLines) {
	    if (!ruleLine.isCommented()){
		list.add(((AssignmentExpressionRuleLine)ruleLine).toAssignmentExpression());
	    }
	}
	return list;
    }

    public String getIdGuide(){
	return _idGuide;
    }

    public void setIdGuide(String idGuide){
	_idGuide = idGuide;
    }

    public void changeLanguage(String language){
	GDLEditor editor = new GDLEditor();
	editor.setCurrentGuideLanguageCode(language);
	Guide guide = getGuide();
	if (guide!=null){
	    TermDefinition originalTermDefinition = guide.getOntology().getTermDefinitions().get(editor.getOriginalLanguageCode());
	    TermDefinition currentTermDefinition = guide.getOntology().getTermDefinitions().get(language);
	    if (currentTermDefinition==null){
		currentTermDefinition = new TermDefinition();
		currentTermDefinition.setTerms(new HashMap<String, Term>());
		guide.getOntology().getTermDefinitions().put(language, currentTermDefinition);
	    }
	    for (String gtCode : originalTermDefinition.getTerms().keySet()) {
		Term term = currentTermDefinition.getTerms().get(gtCode);
		if (term==null){
		    term = getTermToDifferenLanguage(
			    gtCode, 
			    originalTermDefinition.getTerms().get(gtCode), 
			    editor.getOriginalLanguageCode());
		    currentTermDefinition.getTerms().put(gtCode, term);
		}
	    }
	    editor.setGuide(guide);
	    try {
		EditorManager.initController(editor);
	    } catch (InternalErrorException e) {
		ExceptionHandler.handle(e);
	    }
	    setCurrentTab(editor);
	}
    }

    public void setCurrentTab(GDLEditor editor){
	int index = _gdlEditorMainPanel.getGuidePanel().getGuideEditorTabPane().getSelectedIndex();
	editor._gdlEditorMainPanel.getGuidePanel().getGuideEditorTabPane().setSelectedIndex(index);
    }

    public boolean setGuide(Guide guide){
	try{
	    checkGuideArchetypesAndTemplates(guide);
	}catch(InternalErrorException e){
	    ExceptionHandler.handle(e);
	    return false;
	}
	_idGuide = guide.getId();
	_resourceDescription = guide.getDescription();
	if (_resourceDescription!=null){
	    _originalAuthor = _resourceDescription.getOriginalAuthor();
	    _details = _resourceDescription.getDetails();
	    _otherContributors = _resourceDescription.getOtherContributors();
	    _otherDetails = _resourceDescription.getOtherDetails();
	}
	_conceptGTCode = guide.getConcept();
	_termDefinitions = guide.getOntology().getTermDefinitions();
	_termBindings = guide.getOntology().getTermBindings();
	_language = guide.getLanguage();
	_readableGuide = GuideImporter.importGuide(guide, getCurrentGuideLanguageCode());
	initResourceDescription();
	countTermDefinitions();
	updateOriginal();
	return true;
    }

    public boolean isModified(){
	String serializedGuide = null;
	try{
	    Guide guide = constructCurrentGuide();
	    serializedGuide = GuideUtil.serializeGuide(guide);
	}catch(Exception e){
	    //Guide has errors, so it will trigger as modified (serializedGuide = null)
	    //If user decides to save the guide, errors will be displayed (if not, ignored)
	}
	if (_originalGuide!=null &&  !_originalGuide.equals(serializedGuide)){
	    return true;
	}else{
	    return false;
	}
    }

    private void countTermDefinitions(){
	TermDefinition termDefinition = getTermDefinition(getLanguage().getOriginalLanguage().getCodeString());
	if (termDefinition.getTerms()!=null){
	    for (String gtCode : termDefinition.getTerms().keySet()) {
		try{
		    int codeNum = Integer.parseInt(gtCode.substring(2));
		    if (codeNum>_termCount){
			_termCount = codeNum;
		    }
		}catch(Exception e){
		    Logger.getLogger(this.getClass()).warn("Unable to parse code '"+gtCode+"'");
		}
	    }
	}
    }

    public void cleanRedundantTerms(){
	Collection<String> gtCodesUsed = getGTCodesUsedInGuide();
	for (TermDefinition termDefinition : getTermDefinitions().values()) {
	    Collection<Term> terms = new ArrayList<Term>(termDefinition.getTerms().values());
	    for (Term term : terms) {
		if (!gtCodesUsed.contains(term.getId())){
		    termDefinition.getTerms().remove(term.getId());
		}
	    }
	}
    }

    public Collection<String> getGTCodesUsedInGuide(){
	TermDefinition td = getTermDefinitions().get(getCurrentGuideLanguageCode());
	if (td!=null){
	    return td.getTerms().keySet();
	}else{
	    return Collections.emptyList();
	}
    }

    public Collection<String> getGTCodesUsedInDefinitions(){
	Collection<String> gtCodes = new ArrayList<String>();
	gtCodes.add(getConceptGTCode());
	gtCodes.addAll(getRenderableRules().keySet());
	for (RuleLine ruleLine : getDefinitionRuleLines()) {
	    for (RuleLine ruleLineAux : ruleLine.getChildrenRuleLines()) {
		if (ruleLineAux instanceof ArchetypeElementInstantiationRuleLine){
		    gtCodes.add(((ArchetypeElementInstantiationRuleLine)ruleLineAux).getGTCode());
		}
	    }
	}
	return gtCodes;
    }

    public Collection<String> getGTCodesUsedInBindings(){
	Collection<String> gtCodes = new HashSet<String>();
	for (TermBinding termBinding : getTermBindings().values()) {
	    gtCodes.addAll(termBinding.getBindings().keySet());
	}
	return gtCodes;
    }
    
    public void saveCompiledGuideAsObject(byte[] compiledGuide){
	GDLEditor controller = EditorManager.getActiveGDLEditor();
	String idGuide = controller.getIdGuide();
	if (idGuide==null){
	    idGuide = LanguageManager.getMessage("Guide");
	}
	if (compiledGuide!=null){
	    try {
		String guideSource = controller.serializeCurrentGuide();
		if (guideSource!=null){
		    JFileChooser fileChooser = new JFileChooser();
		    FileNameExtensionFilter filter = 
			    new FileNameExtensionFilter(
				    LanguageManager.getMessage("Guide"),new String[]{"guide"});
		    fileChooser.setDialogTitle(LanguageManager.getMessage("SaveGuideAsObjectSD"));
		    fileChooser.setFileFilter(filter);
		    File file = new File(
			    fileChooser.getFileSystemView().getDefaultDirectory()+"/"+
				    idGuide+".guide");
		    fileChooser.setSelectedFile(file);
		    int result = fileChooser.showSaveDialog(EditorManager.getActiveEditorWindow());
		    File guideFile = fileChooser.getSelectedFile();
		    if (result != JFileChooser.CANCEL_OPTION){
			idGuide = guideFile.getName();
			if (idGuide.endsWith(".guide")){
			    idGuide = idGuide.substring(0, idGuide.length()-6);
			}
			GuideDTO guideDTO = new GuideDTO(
				idGuide, controller.getTitle(), controller.getTitle(), 
				guideSource, compiledGuide);
			ObjectOutputStream output = 
				new ObjectOutputStream(
					new BufferedOutputStream(new FileOutputStream(guideFile)));
			try {
			    output.writeObject(guideDTO);
			} catch (Exception e) {
			    ExceptionHandler.handle(e);
			}finally{
			    output.close();
			}
		    }
		}
	    }catch(Exception e){
		ExceptionHandler.handle(e);
	    }
	}
    }

    public ArchetypeInstantiationRuleLine addArchetypeReference(boolean showOnlyCDS){
	ArchetypeInstantiationRuleLine airl = new ArchetypeInstantiationRuleLine();
	//airl.getGTCodeRuleLineElement().setValue(EditorManager.getActiveGDLEditor().createNextGTCode());
	airl.setTermDefinition(getCurrentTermDefinition());
	RuleElementEditor.editArchetype(airl.getArchetypeReferenceRuleLineDefinitionElement(), showOnlyCDS);
	if (airl.getArchetypeReferenceRuleLineDefinitionElement().getValue()!=null){
	    getDefinitionRuleLines().add(airl);
	    return airl;
	}else{
	    return null;
	}
    }

    public ArchetypeElementInstantiationRuleLine addArchetypeElement(ArchetypeInstantiationRuleLine airl){
	ArchetypeElementInstantiationRuleLine aeirl = new ArchetypeElementInstantiationRuleLine(airl);
	aeirl.getGTCodeRuleLineElement().setValue(EditorManager.getActiveGDLEditor().createNextGTCode());
	aeirl.setTermDefinition(getCurrentTermDefinition());
	editRuleElement(aeirl.getArchetypeElementRuleLineDefinitionElement());
	if (aeirl.getArchetypeElementRuleLineDefinitionElement().getValue()!=null){
	    return aeirl;
	}else{
	    aeirl.detachFromParent();
	    return null;
	}
    }

    public void setBusy(String description){
	//getEditorPanel().setBusy(true);
	getInfoDialog().changeLoadingText(description);
	getInfoDialog().start();
    }

    public void changeBusyText(String description){
	getInfoDialog().changeLoadingText(description);
    }

    private InfoDialog getInfoDialog(){
	if (_infoDialog==null){
	    _infoDialog = new InfoDialog(EditorManager.getActiveEditorWindow());
	}
	return _infoDialog;
    }

    public void setFree(){
	getInfoDialog().stop();
    }

    private void checkGuideArchetypesAndTemplates(Guide guide) throws InternalErrorException{
	if (guide==null || guide.getDefinition()==null || guide.getDefinition().getArchetypeBindings()==null){
	    return;
	}
	for (ArchetypeBinding archetypeBinding : guide.getDefinition().getArchetypeBindings()) {
	    String archetypeId = archetypeBinding.getArchetypeId();
	    String templateId = archetypeBinding.getTemplateId();
	    if (templateId==null){
		if (Archetypes.getArchetypeVO(archetypeId)==null){
		    int result = ImportUtils.showImportArchetypeDialog(
			    EditorManager.getActiveEditorWindow(), 
			    new File(archetypeId+".adl"));
		    if (result==JFileChooser.CANCEL_OPTION){
			throw new InternalErrorException(new Exception("Archetype '"+archetypeId+"' not found."));
		    }
		}
	    }else{
		if (Templates.getTemplateVO(templateId)==null){
		    int result = ImportUtils.showImportTemplateDialog(
			    EditorManager.getActiveEditorWindow(), 
			    new File(templateId+".oet"));
		    if (result==JFileChooser.CANCEL_OPTION){
			throw new InternalErrorException(new Exception("Template '"+templateId+"' not found."));
		    }
		}
	    }
	}
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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
package se.cambio.cds.formgen.controller;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import se.cambio.cds.controller.guide.ElementInstanceCollection;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.formgen.view.panels.CDSFormPanel;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.GuideImporter;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.cds.delegate.CDSFacadeDelegate;
import se.cambio.cds.model.facade.cds.delegate.CDSFacadeDelegateFactory;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.UserConfigurationManager;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class FormGeneratorController {

    private GuideDTO _guideDTO = null;
    private CDSFacadeDelegate cdsfd;
    private CDSFormPanel _cdsFormPanel = null;
    private FormGeneratorViewer _viewer = null;
    private Map<String, Map<String, ReadableGuide>> _readableGuideMap;
    private List<RuleReference> _lastFiredRulesReference = null;
    private Calendar _currentDate = null;
    private String _lang = null;

    public FormGeneratorController(GuideDTO guideDTO){
	_guideDTO = guideDTO;
	Collection<GuideDTO> guides = new ArrayList<GuideDTO>();
	guides.add(guideDTO);
	init();
    }

    public FormGeneratorController(
	    GuideDTO guideDTO, 
	    GuideManager guideManager,
	    String lang){
	_guideDTO = guideDTO;
	try{
	    getCDSFacadeDelegate().setGuideManager(guideManager);
	} catch (InternalErrorException e) {
	    ExceptionHandler.handle(e);
	} 
	_lang = lang;
	init();
    }

    private void init(){
	getCDSFormPanel().setInputElements(getInputElementInstances());
    }

    public Collection<ElementInstance> getAllElementInstances(){
	return getCDSFormPanel().getElementInstances();
    }

    public GuideDTO getGuideDTO(){
	return _guideDTO;
    }

    public String getName(){
	Term ct = getConcepTerm();
	if (ct!=null){
	    return ct.getText();
	}else{
	    return OpenEHRLanguageManager.getMessage("Unknown");
	}
    }

    public String getDescription(){
	Term ct = getConcepTerm();
	if (ct!=null){
	    return ct.getDescription();
	}else{
	    return OpenEHRLanguageManager.getMessage("Unknown");
	}
    }

    public Term getConcepTerm(){
	String concept = getGuide().getConcept();
	TermDefinition td = getTermDefinition();
	if (td!=null){
	    return td.getTerms().get(concept);
	}else{
	    return null;
	}
    }

    public TermDefinition getTermDefinition(){
	TermDefinition termDefinition = getGuide().getOntology().getTermDefinitions().get(getLanguage());
	if (termDefinition==null){
	    termDefinition = getGuide().getOntology().getTermDefinitions().get(UserConfigurationManager.DEFAULT_LANGUAGE);
	}
	return termDefinition;
    }

    public Guide getGuide(){
	return getGuideManager().getGuide(getGuideDTO().getIdGuide());
    }

    public CDSFormPanel getCDSFormPanel(){
	if (_cdsFormPanel==null){
	    _cdsFormPanel = new CDSFormPanel(this);
	}
	return _cdsFormPanel;
    }


    public GuideManager getGuideManager(){
	try{
	    return getCDSFacadeDelegate().getGuideManager();
	} catch (InternalErrorException e) {
	    ExceptionHandler.handle(e);
	    return null;
	}
    }

    public Calendar getCurrentDate(){
	return _currentDate;
    }

    public void setCurrentDate(Calendar currentDate){
	_currentDate= currentDate;
    }

    /*
    public boolean isLinkedInput(ElementInstance elementInstance){
	Set<String> idGuides = getGuideManager().getAllGuideIdsWithCDSDomain(elementInstance);
	return Domains.CDS_ID.equals(elementInstance.getArchetypeReference().getIdDomain()) &&
		!idGuides.isEmpty() && 
		!idGuides.contains(getGuideDTO().getIdGuide());
    }

    public ActionListener createLinkedFormGeneratorActionListener(ElementInstance elementInstance){
	return new LinkedFormGeneratorActionListener(elementInstance, this);
    }

    private class LinkedFormGeneratorActionListener implements ActionListener{
	private ElementInstance _elementInstance;
	private FormGeneratorController _parentController;
	public LinkedFormGeneratorActionListener(ElementInstance elementInstance, FormGeneratorController parentController){
	    _elementInstance = elementInstance;
	    _parentController = parentController;
	}

	public void actionPerformed(ActionEvent e) {
	    Set<String> idGuides = getGuideManager().getAllGuideIdsWithCDSDomain(_elementInstance);
	    Iterator<GuideDTO> i = getGuideManager().getAllGuidesDTO().iterator();
	    while(i.hasNext()){
		GuideDTO guideDTO = i.next();
		if (idGuides.contains(guideDTO.getIdGuide())){
		    CDSFormGenDialog dialog = new CDSFormGenDialog((Window)getViewer());
		    dialog.addWindowListener(new FGDialogWindowListener(_parentController));
		    FormGeneratorController controller = 
			    new FormGeneratorController(guideDTO, getGuideManager(), getLanguage());
		    replaceElementInstance(controller.getResultElementInstances(), _elementInstance);
		    dialog.setFormGeneratorController(controller);
		    dialog.setVisible(true);
		}
	    }
	}
    }

    private class FGDialogWindowListener extends WindowAdapter{
	private FormGeneratorController _parentController;
	public FGDialogWindowListener(FormGeneratorController parentController){
	    _parentController = parentController;
	}
	public void windowClosed(WindowEvent e) {
	    _parentController.getCDSFormPanel().setInputElements(_parentController.getInputElementInstances());
	}
    }

    public void removeElementInstance(ElementInstance elementInstance){
	ArchetypeReference ar = elementInstance.getArchetypeReference();
	ar.getElementInstancesMap().remove(elementInstance.getId());
    }
     */
    public Collection<ElementInstance> getInputElementInstances(){
	Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
	ElementInstanceCollection eic = getGuideManager().getCompleteElementInstanceCollection();
	elementInstances.addAll(eic.getAllElementInstancesByDomain(Domains.EHR_ID));
	elementInstances.addAll(eic.getAllElementInstancesByDomain(ElementInstanceCollection.EMPTY_CODE));
	return elementInstances;
    }

    public void updateResults(RuleExecutionResult result){
	getCDSFormPanel().updateResults(result);
	if (result!=null){
	    _lastFiredRulesReference = result.getFiredRules();
	}
    }

    public Collection<String> getSupportedLanguages(){
	return getReadableGuideMap().get(_guideDTO.getIdGuide()).keySet();
    }

    public List<RuleReference> getLastRulesFired(){
	return _lastFiredRulesReference;
    }

    public Map<String, Map<String, ReadableGuide>> getReadableGuideMap(){
	if (_readableGuideMap == null){
	    _readableGuideMap = new HashMap<String, Map<String, ReadableGuide>>();
	    try {
		GDLParser parser = new GDLParser();
		for (GuideDTO guideDTO : getGuideManager().getAllGuidesDTO()) {
		    Map<String, ReadableGuide> auxMap = new HashMap<String, ReadableGuide>();
		    _readableGuideMap.put(guideDTO.getIdGuide(), auxMap);
		    Guide guide = parser.parse(new ByteArrayInputStream(guideDTO.getGuideSrc().getBytes()));
		    Map<String, TermDefinition> termDefinitions = guide.getOntology().getTermDefinitions();
		    for (TermDefinition termDefinition : termDefinitions.values()) {
			String lang = termDefinition.getId();
			auxMap.put(lang, GuideImporter.importGuide(guide, lang));
		    }
		}
	    } catch (Exception e) {
		ExceptionHandler.handle(e);
	    }
	}
	return _readableGuideMap;
    }

    public CDSFacadeDelegate getCDSFacadeDelegate(){
	if (cdsfd==null){
	    try {
		cdsfd = CDSFacadeDelegateFactory.getDelegate();
	    } catch (InternalErrorException e) {
		ExceptionHandler.handle(e);
	    } 
	}
	return cdsfd;
    }

    public String getLanguage(){
	if (_lang==null){
	    _lang = OpenEHRLanguageManager.getLanguage();
	}
	return _lang;
    }

    public void setViewer(FormGeneratorViewer viewer){
	_viewer = viewer;
    }

    public FormGeneratorViewer getViewer(){
	return _viewer;
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
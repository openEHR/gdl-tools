package se.cambio.cds.formgen.controller;

import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.formgen.view.panels.CDSFormPanel;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.cds.delegate.CDSExecutionFacadeDelegate;
import se.cambio.cds.model.facade.cds.delegate.CDSExecutionFacadeDelegateFactory;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.util.*;

public class FormGeneratorController {

    private GuideDTO _guideDTO = null;
    private Guide _guide = null;
    private GuideManager _guideManager = null;
    private CDSExecutionFacadeDelegate cdsfd;
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
            String lang){
        _guideDTO = guideDTO;
        _lang = lang;
        init();
    }

    private void init(){
        getCDSFormPanel().setInputElements(getInputArcehtypeReferences());
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
            termDefinition = getGuide().getOntology().getTermDefinitions().get(getGuide().getLanguage().getOriginalLanguage().getCodeString());
        }
        return termDefinition;
    }

    public Guide getGuide(){
        if(_guide==null){
            try {
                _guide = getGuideManager().getGuide(getGuideDTO().getIdGuide());
            } catch (Exception e) {
                ExceptionHandler.handle(e);
            }
        }
        return _guide;
    }

    public CDSFormPanel getCDSFormPanel(){
        if (_cdsFormPanel==null){
            _cdsFormPanel = new CDSFormPanel(this);
        }
        return _cdsFormPanel;
    }


    public GuideManager getGuideManager(){
        if (_guideManager==null){
            _guideManager = new GuideManager(Collections.singletonList(getGuideDTO()));
        }
        return _guideManager;
    }

    public Calendar getCurrentDate(){
        return _currentDate;
    }

    public void setCurrentDate(Calendar currentDate){
        _currentDate= currentDate;
    }

    public Collection<ArchetypeReference> getInputArcehtypeReferences(){
        GeneratedElementInstanceCollection eic = getGuideManager().getCompleteElementInstanceCollection();
        return CDSManager.getEHRArchetypeReferences(eic);
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

    public CDSExecutionFacadeDelegate getCDSFacadeDelegate() throws InternalErrorException{
        if (cdsfd==null){
            cdsfd = CDSExecutionFacadeDelegateFactory.getDelegate();
        }
        return cdsfd;
    }

    public String getLanguage(){
        if (_lang==null){
            _lang = UserConfigurationManager.getLanguage();
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
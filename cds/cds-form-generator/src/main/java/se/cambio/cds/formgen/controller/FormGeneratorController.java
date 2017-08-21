package se.cambio.cds.formgen.controller;

import lombok.extern.slf4j.Slf4j;
import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.formgen.view.panels.CDSFormPanel;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.cds.util.GuideImporter;
import se.cambio.cds.view.swing.DvSwingManager;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.*;

@Slf4j
public class FormGeneratorController {

    private GuideDTO guideDTO = null;
    private Guide guide = null;
    private GuideManager guideManager = null;
    private CDSFormPanel cdsFormPanel = null;
    private FormGeneratorViewer viewer = null;
    private Map<String, Map<String, ReadableGuide>> readableGuideMap;
    private List<RuleReference> lastFiredRulesReference = null;
    private Calendar currentDate = null;
    private String lang = null;
    private GuideImporter guideImporter;
    private ElementInstanceCollectionManager elementInstanceCollectionManager;
    private final ArchetypeManager archetypeManager;
    private final DvSwingManager dvSwingManager;
    private CdsDataManager cdsDataManager;
    private final RuleEngineService ruleEngineService;

    public FormGeneratorController(
            GuideDTO guideDTO,
            String lang,
            GuideImporter guideImporter,
            ElementInstanceCollectionManager elementInstanceCollectionManager,
            ArchetypeManager archetypeManager,
            DvSwingManager dvSwingManager,
            CdsDataManager cdsDataManager,
            RuleEngineService ruleEngineService) {
        this.guideDTO = guideDTO;
        this.lang = lang;
        this.guideImporter = guideImporter;
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
        this.archetypeManager = archetypeManager;
        this.dvSwingManager = dvSwingManager;
        this.cdsDataManager = cdsDataManager;
        this.ruleEngineService = ruleEngineService;
        init();
    }

    public ElementInstanceCollectionManager getElementInstanceCollectionManager() {
        return elementInstanceCollectionManager;
    }

    private void init() {
        getCDSFormPanel().setInputElements(getInputArchetypeReferences());
    }

    public Collection<ElementInstance> getAllElementInstances() {
        return getCDSFormPanel().getElementInstances();
    }

    public GuideDTO getGuideDTO() {
        return guideDTO;
    }

    public String getName() {
        Term ct = getConceptTerm();
        if (ct != null) {
            return ct.getText();
        } else {
            return OpenEHRLanguageManager.getMessage("Unknown");
        }
    }

    public String getDescription() {
        Term ct = getConceptTerm();
        if (ct != null) {
            return ct.getDescription();
        } else {
            return OpenEHRLanguageManager.getMessage("Unknown");
        }
    }

    private Term getConceptTerm() {
        String concept = getGuide().getConcept();
        TermDefinition td = getTermDefinition();
        if (td != null) {
            return td.getTerms().get(concept);
        } else {
            return null;
        }
    }

    public TermDefinition getTermDefinition() {
        TermDefinition termDefinition = getGuide().getOntology().getTermDefinitions().get(getLanguage());
        if (termDefinition == null) {
            termDefinition = getGuide().getOntology().getTermDefinitions().get(getGuide().getLanguage().getOriginalLanguage().getCodeString());
        }
        return termDefinition;
    }

    public Guide getGuide() {
        if (guide == null) {
            guide = getGuideManager().getGuide(getGuideDTO().getId());
        }
        return guide;
    }

    public CDSFormPanel getCDSFormPanel() {
        if (cdsFormPanel == null) {
            cdsFormPanel = new CDSFormPanel(this, archetypeManager, dvSwingManager, cdsDataManager, ruleEngineService);
        }
        return cdsFormPanel;
    }


    public GuideManager getGuideManager() {
        if (guideManager == null) {
            guideManager = new GuideManager(Collections.singletonList(getGuideDTO()), elementInstanceCollectionManager);
        }
        return guideManager;
    }

    public Calendar getCurrentDate() {
        return currentDate;
    }

    public void setCurrentDate(Calendar currentDate) {
        this.currentDate = currentDate;
    }

    private Collection<ArchetypeReference> getInputArchetypeReferences() {
        GeneratedElementInstanceCollection eic = getGuideManager().getCompleteElementInstanceCollection();
        return cdsDataManager.getEHRArchetypeReferences(eic);
    }

    public void updateResults(RuleExecutionResult result) {
        getCDSFormPanel().updateResults(result);
        if (result != null) {
            lastFiredRulesReference = result.getFiredRules();
        }
    }

    public Collection<String> getSupportedLanguages() {
        return getReadableGuideMap().get(guideDTO.getId()).keySet();
    }

    public List<RuleReference> getLastRulesFired() {
        return lastFiredRulesReference;
    }

    public Map<String, Map<String, ReadableGuide>> getReadableGuideMap() {
        if (readableGuideMap == null) {
            readableGuideMap = new HashMap<>();
            GDLParser parser = new GDLParser();
            for (GuideDTO guideDTO : getGuideManager().getAllGuidesDTO()) {
                Map<String, ReadableGuide> auxMap = new HashMap<>();
                readableGuideMap.put(guideDTO.getId(), auxMap);
                try {
                    Guide guide = parser.parse(new ByteArrayInputStream(guideDTO.getSource().getBytes("UTF-8")));
                    Map<String, TermDefinition> termDefinitions = guide.getOntology().getTermDefinitions();
                    for (TermDefinition termDefinition : termDefinitions.values()) {
                        String lang = termDefinition.getId();
                        ReadableGuide readableGuide = guideImporter.importGuide(guide, lang);
                        auxMap.put(lang, readableGuide);
                    }
                } catch (UnsupportedEncodingException ex) {
                    log.error("Error parsing guideline: " + guideDTO.getId(), ex);
                }
            }
        }
        return readableGuideMap;
    }

    public String getLanguage() {
        if (lang == null) {
            lang = archetypeManager.getUserConfigurationManager().getLanguage();
        }
        return lang;
    }

    public void setViewer(FormGeneratorViewer viewer) {
        this.viewer = viewer;
    }

    public FormGeneratorViewer getViewer() {
        return viewer;
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
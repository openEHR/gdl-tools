package se.cambio.openehr.controller.session.data;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeOntology;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.archetype.ontology.OntologyDefinitions;
import se.cambio.openehr.controller.OpenEHRObjectBundleManager;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.model.archetype.vo.TemplateObjectBundleCustomVO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.util.comparators.TemplateComparator;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.*;


public class Templates {
    private static Templates _instance = null;
    private Map<String, TemplateDTO> _templatessById = null;
    public static ImageIcon ICON = OpenEHRImageUtil.TEMPLATE;
    private boolean _loaded = false;
    private HashMap<String, Map<String, Map<String, ArchetypeTerm>>> _archetypeTermsMap;

    private Templates(){
    }

    public static void loadTemplates() throws InternalErrorException{
        loadTemplates(false);
    }

    public static void loadTemplates(boolean force) throws InternalErrorException{
        Collection<TemplateDTO> templateDTOs =
                OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllTemplates();
        loadTemplates(templateDTOs, force);
    }

    public static void loadTemplates(Collection<TemplateDTO> templateDTOs) throws InternalErrorException{
        loadTemplates(templateDTOs, false);
    }

    public static void loadTemplates(Collection<TemplateDTO> templateDTOs, boolean force) throws InternalErrorException{
        if (!getDelegate()._loaded || force){
            init();
            OpenEHRObjectBundleManager.generateTemplateObjectBundles(templateDTOs);
            loadTemplateObjectBundles(templateDTOs);
            getDelegate()._loaded = true;
        }
    }

    public static void loadTemplate(TemplateDTO templateDTO) throws InternalErrorException{
        OpenEHRObjectBundleManager.generateTemplateObjectBundles(Collections.singleton(templateDTO));
        registerTemplate(templateDTO);
    }

    private static void init(){
        getTemplatesMap().clear();
    }

    private static void registerTemplate(TemplateDTO templateDTO){
        getTemplatesMap().put(templateDTO.getIdTemplate(), templateDTO);
        generateArchetypeDefinitionsMap(templateDTO.getIdTemplate());
    }

    private static void generateArchetypeDefinitionsMap(String templateId){
        Archetype archetype = getTemplateAOM(templateId);
        ArchetypeOntology ao = archetype.getOntology();
        List<OntologyDefinitions> ods = ao.getTermDefinitionsList();
        for (OntologyDefinitions od : ods){
            String lang = od.getLanguage();
            List<ArchetypeTerm> archetypeTerms = od.getDefinitions();
            for(ArchetypeTerm archetypeTerm: archetypeTerms){
                getArchetypeTermsMap(templateId, lang).put(archetypeTerm.getCode(), archetypeTerm);
            }
        }
    }

    public static ArchetypeTerm getArchetypeTerm(String templateId, String lang, String atCode){
        return getArchetypeTermsMap(templateId, lang).get(atCode);
    }

    private static Map<String, ArchetypeTerm> getArchetypeTermsMap(String templateId, String lang){
        Map<String, ArchetypeTerm> archetypeTermMap = getArchetypeTermsMap(templateId).get(lang);
        if(archetypeTermMap==null){
            archetypeTermMap = new HashMap<String, ArchetypeTerm>();
            getArchetypeTermsMap(templateId).put(lang, archetypeTermMap);
        }
        return archetypeTermMap;
    }

    private static Map<String, Map<String, ArchetypeTerm>> getArchetypeTermsMap(String archetypeId){
        Map<String, Map<String, ArchetypeTerm>> archetypeTermMap = getArchetypeTermsMap().get(archetypeId);
        if(archetypeTermMap==null){
            archetypeTermMap = new HashMap<String, Map<String, ArchetypeTerm>>();
            getArchetypeTermsMap().put(archetypeId, archetypeTermMap);
        }
        return archetypeTermMap;
    }

    private static Map<String, Map<String, Map<String, ArchetypeTerm>>> getArchetypeTermsMap(){
        if(getDelegate()._archetypeTermsMap==null){
            getDelegate()._archetypeTermsMap = new HashMap<String, Map<String, Map<String, ArchetypeTerm>>>();
        }
        return getDelegate()._archetypeTermsMap;
    }


    public static TemplateDTO getTemplateDTO(String idTemplate){
        return getTemplatesMap().get(idTemplate);
    }

    public static void loadTemplateObjectBundles(Collection<TemplateDTO> templateDTOs){
        for (TemplateDTO templateDTO : templateDTOs) {
            loadTemplateObjectBundle(templateDTO);
        }
    }

    public static void loadTemplateObjectBundle(TemplateDTO templateDTO){
        registerTemplate(templateDTO);
        TemplateObjectBundleCustomVO templateObjectBundleCustomVO =
                (TemplateObjectBundleCustomVO)IOUtils.getObject(templateDTO.getTobcVO());
        ArchetypeElements.loadArchetypeElements(templateObjectBundleCustomVO.getElementVOs());
        Clusters.loadClusters(templateObjectBundleCustomVO.getClusterVOs());
        CodedTexts.loadCodedTexts(templateObjectBundleCustomVO.getCodedTextVOs());
        Ordinals.loadOrdinals(templateObjectBundleCustomVO.getOrdinalVOs());
        //ArchetypeSlots.loadArchetypeNodes(templateObjectBundleCustomVO.getSlotVOs());
        Units.loadUnits(templateObjectBundleCustomVO.getUnitVOs());
    }

    public static ArrayList<TemplateDTO> getTemplates(String entryType){
        ArrayList<TemplateDTO> list = new ArrayList<TemplateDTO>();
        for (TemplateDTO templateDTO : getTemplatesMap().values()) {
            if (entryType.equals(templateDTO.getRMName())){
                list.add(templateDTO);
            }
        }
        return list;
    }

    public static void removeTemplate(String templateId) throws InternalErrorException{
        getTemplatesMap().remove(templateId);
    }

    public static ImageIcon getIcon(String idTemplate){
        String entryType = getTemplateDTO(idTemplate).getRMName();
        ImageIcon icon = OpenEHRConstUI.getIcon(entryType);
        if (icon!=null){
            return icon;
        }else{
            return ICON;
        }
    }

    public static List<TemplateDTO> getAllTemplates(){
        return new ArrayList<TemplateDTO>(getTemplatesMap().values());
    }

    public static List<String> getAllTemplateIds(){
        return new ArrayList<String>(getTemplatesMap().keySet());
    }

    private static Map<String, TemplateDTO> getTemplatesMap(){
        if (getDelegate()._templatessById == null){
            getDelegate()._templatessById = new HashMap<String, TemplateDTO>();
        }
        return getDelegate()._templatessById;
    }

    public static Archetype getTemplateAOM(String templateId){
        Archetype aom = null;
        TemplateDTO templateDTO = getTemplateDTO(templateId);
        if (templateDTO!=null && templateDTO.getAom()!=null){
            aom = (Archetype)IOUtils.getObject(templateDTO.getAom());
        }else {
            Logger.getLogger(Archetype.class).debug("Template '"+templateId+"' not found.");
        }
        return aom;
    }

    public int hashCode(){
        List<TemplateDTO> templateDTOs = getAllTemplates();
        Collections.sort(templateDTOs, new TemplateComparator());
        List<String> defs = new ArrayList<String>();
        for(TemplateDTO templateDTO: templateDTOs){
            defs.add(templateDTO.getArchetype());
        }
        return defs.hashCode();
    }

    public static boolean hasBeenLoaded(){
        return getDelegate()._loaded;
    }

    public static Templates getDelegate(){
        if (_instance == null){
            _instance = new Templates();
        }
        return _instance;
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
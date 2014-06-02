package se.cambio.openehr.controller.session.data;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeOntology;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.archetype.ontology.OntologyDefinitions;
import se.cambio.openehr.controller.OpenEHRObjectBundleManager;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.model.util.comparators.ArchetypeComparator;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.*;


public class Archetypes {
    private static Archetypes _instance = null;
    private Map<String, ArchetypeDTO> _archetypesById = null;
    private Map<String, Archetype> _archetypeAOM = null;
    private Map<String, Map<String, Map<String, ArchetypeTerm>>> _archetypeTermsMap = null;
    public static ImageIcon ICON = OpenEHRImageUtil.ARCHETYPE;
    public boolean _loaded = false;

    private Archetypes(){
    }


    public static void loadArchetypes() throws InternalErrorException{
        loadArchetypes(false);
    }

    public static void loadArchetypes(boolean force) throws InternalErrorException{
        Collection<ArchetypeDTO> archetypeDTOs =
                OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllArchetypes();
        loadArchetypes(archetypeDTOs, force);
    }
    public static void loadArchetypes(Collection<ArchetypeDTO> archetypeDTOs) throws InternalErrorException{
        loadArchetypes(archetypeDTOs, false);
    }

    public static void loadArchetypes(Collection<ArchetypeDTO> archetypeDTOs, boolean force) throws InternalErrorException{
        if (!getDelegate()._loaded || force){
            init();
            Collection<ArchetypeDTO> correctlyParsedArchetypeDTOs = OpenEHRObjectBundleManager.generateArchetypesObjectBundles(archetypeDTOs);
            loadArchetypesObjectBundle(correctlyParsedArchetypeDTOs);
            getDelegate()._loaded = true;
        }
    }

    public static void loadArchetype(ArchetypeDTO archetypeDTO) throws InternalErrorException{
        Collection<ArchetypeDTO> correctlyParsedArchetypeDTOs = OpenEHRObjectBundleManager.generateArchetypesObjectBundles(Collections.singleton(archetypeDTO));
        if (!correctlyParsedArchetypeDTOs.isEmpty()){
            loadArchetypeDTO(correctlyParsedArchetypeDTOs.iterator().next());
        }
    }

    public static void removeArchetype(String archetypeId) throws InternalErrorException{
        getArchetypeDTOMap().remove(archetypeId);
    }

    private static void init(){
        getArchetypeDTOMap().clear();
        getAOMMap().clear();
        getArchetypeTermsMap().clear();
        ArchetypeElements.getDelegate().init();
        Clusters.getDelegate().init();
        CodedTexts.getDelegate().init();
        Ordinals.getDelegate().init();
        ArchetypeSlots.getDelegate().init();
        Units.getDelegate().init();
        ProportionTypesUI.getDelegate().init();
    }

    private static void registerArchertype(ArchetypeDTO archetypeDTO){
        getArchetypeDTOMap().put(archetypeDTO.getIdArchetype(), archetypeDTO);
        generateArchetypeDefinitionsMap(archetypeDTO.getIdArchetype());
    }

    private static void generateArchetypeDefinitionsMap(String archetypeId){
        Archetype archetype = getArchetypeAOM(archetypeId);
        ArchetypeOntology ao = archetype.getOntology();
        List<OntologyDefinitions> ods = ao.getTermDefinitionsList();
        for (OntologyDefinitions od : ods){
            String lang = od.getLanguage();
            List<ArchetypeTerm> archetypeTerms = od.getDefinitions();
            for(ArchetypeTerm archetypeTerm: archetypeTerms){
                getArchetypeTermsMap(archetypeId, lang).put(archetypeTerm.getCode(), archetypeTerm);
            }
        }
    }

    public static ArchetypeTerm getArchetypeTerm(String archetypeId, String lang, String atCode){
        return getArchetypeTermsMap(archetypeId, lang).get(atCode);
    }

    private static Map<String, ArchetypeTerm> getArchetypeTermsMap(String archetypeId, String lang){
        Map<String, ArchetypeTerm> archetypeTermMap = getArchetypeTermsMap(archetypeId).get(lang);
        if(archetypeTermMap==null){
            archetypeTermMap = new HashMap<String, ArchetypeTerm>();
            getArchetypeTermsMap(archetypeId).put(lang, archetypeTermMap);
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

    public static ArchetypeDTO getArchetypeDTO(String idArchetype){
        return getArchetypeDTOMap().get(idArchetype);
    }

    public static Archetype getArchetypeAOM(String idArchetype){
        Archetype aom = getAOMMap().get(idArchetype);
        if(aom==null){
            ArchetypeDTO archetypeDTO = getArchetypeDTO(idArchetype);
            if (archetypeDTO!=null && archetypeDTO.getAom()!=null){
                aom = (Archetype)IOUtils.getObject(archetypeDTO.getAom());
                getAOMMap().put(idArchetype, aom);
            }else {
                Logger.getLogger(Archetype.class).debug("Archetype '"+idArchetype+"' not found.");
            }
        }
        return aom;
    }

    public static String getArchetypeSource(String idArchetype){
        return getArchetypeDTO(idArchetype).getArchetype();
    }

    private static void loadArchetypesObjectBundle(Collection<ArchetypeDTO> archetypeDTOs){
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            loadArchetypeDTO(archetypeDTO);
        }
    }

    public static void loadArchetypeDTO(ArchetypeDTO archetypeDTO){
        try{
            registerArchertype(archetypeDTO);
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
            ArchetypeElements.loadArchetypeElements(archetypeObjectBundleCustomVO.getElementVOs());
            Clusters.loadClusters(archetypeObjectBundleCustomVO.getClusterVOs());
            CodedTexts.loadCodedTexts(archetypeObjectBundleCustomVO.getCodedTextVOs());
            Ordinals.loadOrdinals(archetypeObjectBundleCustomVO.getOrdinalVOs());
            ArchetypeSlots.loadArchetypeNodes(archetypeObjectBundleCustomVO.getSlotVOs());
            Units.loadUnits(archetypeObjectBundleCustomVO.getUnitVOs());
            ProportionTypesUI.loadProportionTypes(archetypeObjectBundleCustomVO.getProportionTypes());
        }catch(Throwable e){
            ExceptionHandler.handle(e);   //TODO
        }
    }

    public static ArrayList<ArchetypeDTO> getArchetypes(String entryType){
        ArrayList<ArchetypeDTO> list = new ArrayList<ArchetypeDTO>();
        for (ArchetypeDTO archetypeVO : getArchetypeDTOMap().values()) {
            if (entryType.equals(archetypeVO.getRMName())){
                list.add(archetypeVO);
            }
        }
        return list;
    }

    public static ImageIcon getIcon(String idArchetype){
        ArchetypeDTO archetypeDTO = getArchetypeDTO(idArchetype);
        ImageIcon icon = null;
        if (archetypeDTO!=null){
            String entryType = archetypeDTO.getRMName();
            icon = OpenEHRConstUI.getIcon(entryType);
        }else{
            Logger.getLogger(Archetypes.class).warn("Archetype '"+idArchetype+"' was not found loading icon.");
        }
        if (icon!=null){
            return icon;
        }else{
            return ICON;
        }
    }

    public static List<ArchetypeDTO> getAllArchetypes(){
        return new ArrayList<ArchetypeDTO>(getArchetypeDTOMap().values());
    }

    public static Map<String, Archetype> getArchetypeMap(){
        Map<String, Archetype> archetypeMap = new HashMap<String, Archetype>();
        for (ArchetypeDTO archetypeVO : getAllArchetypes()) {
            archetypeMap.put(archetypeVO.getIdArchetype(), getArchetypeAOM(archetypeVO.getIdArchetype()));
        }
        return archetypeMap;
    }

    public static List<String> getAllArchetypeIds(){
        return new ArrayList<String>(getArchetypeDTOMap().keySet());
    }

    public static Map<String, Archetype> getAOMMap(){
        if (getDelegate()._archetypeAOM == null){
            getDelegate()._archetypeAOM = new HashMap<String, Archetype>();
        }
        return getDelegate()._archetypeAOM;
    }

    public static Map<String, ArchetypeDTO> getArchetypeDTOMap(){
        if (getDelegate()._archetypesById == null){
            getDelegate()._archetypesById = new HashMap<String, ArchetypeDTO>();
        }
        return getDelegate()._archetypesById;
    }

    public int hashCode(){
        List<ArchetypeDTO> archetypeDTOs = getAllArchetypes();
        Collections.sort(archetypeDTOs, new ArchetypeComparator());
        List<String> defs = new ArrayList<String>();
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            defs.add(archetypeDTO.getArchetype());
        }
        return defs.hashCode();
    }

    public static boolean hasBeenLoaded(){
        return getDelegate()._loaded;
    }

    public static Archetypes getDelegate(){
        if (_instance == null){
            _instance = new Archetypes();
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
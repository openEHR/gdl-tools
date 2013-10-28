package se.cambio.openehr.controller.session.data;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.OpenEHRObjectBundleManager;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


public class Archetypes {
    private static Archetypes _instance = null;
    private Map<String, ArchetypeDTO> _archetypesById = null;
    private Map<String, Archetype> _archetypeAOM = null;
    public static ImageIcon ICON = OpenEHRImageUtil.ARCHETYPE;

    private Archetypes(){
    }

    public static void loadArchetypes() throws InternalErrorException{
        loadArchetypes(true);
    }

    public static void loadArchetypes(boolean generate) throws InternalErrorException{
        init();
        Collection<ArchetypeDTO> archetypeDTOs =
                OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllArchetypes();
        if (generate){
            OpenEHRObjectBundleManager.generateArchetypesObjectBundles(archetypeDTOs);
        }
        loadArchetypesObjectBundle(archetypeDTOs);
    }

    private static void init(){
        getDelegate()._archetypesById = new HashMap<String, ArchetypeDTO>();
        getDelegate()._archetypeAOM = new HashMap<String, Archetype>();
    }

    public static void registerArchertype(ArchetypeDTO archetypeVO){
        getDelegate()._archetypesById.put(archetypeVO.getIdArchetype(), archetypeVO);
    }

    public static ArchetypeDTO getArchetypeDTO(String idArchetype){
        return getDelegate()._archetypesById.get(idArchetype);
    }

    public static Archetype getArchetypeAOM(String idArchetype){
        Archetype aom = getDelegate()._archetypeAOM.get(idArchetype);
        if(aom==null){
            ArchetypeDTO archetypeDTO = getArchetypeDTO(idArchetype);
            if (archetypeDTO!=null && archetypeDTO.getAom()!=null){
                aom = (Archetype)IOUtils.getObject(archetypeDTO.getAom());
                getDelegate()._archetypeAOM.put(idArchetype, aom);
            }else {
                Logger.getLogger(Archetype.class).debug("Archetype '"+idArchetype+"' not found.");
            }
        }
        return aom;
    }

    public static String getArchetypeSource(String idArchetype){
        return getArchetypeDTO(idArchetype).getArchetype();
    }

    public static void loadArchetypesObjectBundle(Collection<ArchetypeDTO> archetypeDTOs){
        getDelegate()._archetypesById.clear();
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            loadArchetypeDTO(archetypeDTO);
        }
    }

    public static void loadArchetypeDTO(ArchetypeDTO archetypeDTO){
        registerArchertype(archetypeDTO);
        ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
        ArchetypeElements.loadArchetypeElements(archetypeObjectBundleCustomVO.getElementVOs());
        Clusters.loadClusters(archetypeObjectBundleCustomVO.getClusterVOs());
        CodedTexts.loadCodedTexts(archetypeObjectBundleCustomVO.getCodedTextVOs());
        Ordinals.loadOrdinals(archetypeObjectBundleCustomVO.getOrdinalVOs());
        ArchetypeSlots.loadArchetypeNodes(archetypeObjectBundleCustomVO.getSlotVOs());
        Units.loadUnits(archetypeObjectBundleCustomVO.getUnitVOs());
        ProportionTypesUI.loadProportionTypes(archetypeObjectBundleCustomVO.getProportionTypes());
    }

    public static ArrayList<ArchetypeDTO> getArchetypes(String entryType){
        ArrayList<ArchetypeDTO> list = new ArrayList<ArchetypeDTO>();
        for (ArchetypeDTO archetypeVO : getDelegate()._archetypesById.values()) {
            if (entryType.equals(archetypeVO.getRMName())){
                list.add(archetypeVO);
            }
        }
        return list;
    }

    public static ImageIcon getIcon(String idArchetype){
        String entryType = getArchetypeDTO(idArchetype).getRMName();
        ImageIcon icon = OpenEHRConstUI.getIcon(entryType);
        if (icon!=null){
            return icon;
        }else{
            return ICON;
        }
    }

    public static Collection<ArchetypeDTO> getAllArchetypes(){
        return new ArrayList<ArchetypeDTO>(getDelegate()._archetypesById.values());
    }

    public static Map<String, Archetype> getArchetypeMap(){
        Map<String, Archetype> archetypeMap = new HashMap<String, Archetype>();
        for (ArchetypeDTO archetypeVO : getAllArchetypes()) {
            archetypeMap.put(archetypeVO.getIdArchetype(), getArchetypeAOM(archetypeVO.getIdArchetype()));
        }
        return archetypeMap;
    }

    public static Collection<String> getAllArchetypeIds(){
        return new ArrayList<String>(getDelegate()._archetypesById.keySet());
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
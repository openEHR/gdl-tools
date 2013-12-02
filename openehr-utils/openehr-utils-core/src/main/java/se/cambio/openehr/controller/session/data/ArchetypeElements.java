package se.cambio.openehr.controller.session.data;

import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.*;

public class ArchetypeElements {
    private static ArchetypeElements _instance = null;
    private Map<String, ArchetypeElementVO> _archetypeElementsById = null;
    private Map<String, Map<String, ArchetypeElementVO>> _templateElementsByTemplateIdAndId = null;

    public static String ID_CURRENT_DATE_TIME = "currentDateTime";

    //Add current time
    public static ArchetypeElementVO CURRENT_DATE_TIME =
            new ArchetypeElementVO(
                    OpenEHRLanguageManager.getMessage("CurrentDateTime"),
                    OpenEHRLanguageManager.getMessage("CurrentDateTime"),
                    OpenEHRDataValues.DV_DATE_TIME, null, null, null, null);

    private ArchetypeElements(){
        _archetypeElementsById = new LinkedHashMap<String, ArchetypeElementVO>();
        _templateElementsByTemplateIdAndId = new LinkedHashMap<String, Map<String, ArchetypeElementVO>>();

    }

    public static void loadArchetypeElements(Collection<ArchetypeElementVO> archetypeElementVOs){
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            registerArchetypeElement(archetypeElementVO);
        }
    }

    public static void registerArchetypeElement(ArchetypeElementVO archetypeElementVO){
        if (archetypeElementVO.getIdTemplate()==null){
            getDelegate()._archetypeElementsById.put(archetypeElementVO.getId(), archetypeElementVO);
        }else{
            getArchetypeElementsInTemplate(archetypeElementVO.getIdTemplate()).put(archetypeElementVO.getId(), archetypeElementVO);
        }
    }

    //TODO Should throw an exception when the element is not found
    public static ArchetypeElementVO getArchetypeElement(String idTemplate, String idElement){
        if (idTemplate==null){
            return getDelegate()._archetypeElementsById.get(idElement);
        }else{
            return getArchetypeElementsInTemplate(idTemplate).get(idElement);
        }
    }

    private static Map<String, ArchetypeElementVO> getArchetypeElementsInTemplate(String idTemplate){
        Map<String, ArchetypeElementVO> elementsInTemplate =
                getDelegate()._templateElementsByTemplateIdAndId.get(idTemplate);
        if (elementsInTemplate==null){
            elementsInTemplate = new LinkedHashMap<String, ArchetypeElementVO>();
            getDelegate()._templateElementsByTemplateIdAndId.put(idTemplate, elementsInTemplate);
        }
        return elementsInTemplate;
    }

    public static Collection<ArchetypeElementVO> getArchetypeElementsVO(String idArchetype, String idTemplate){
        Collection<ArchetypeElementVO> list = new ArrayList<ArchetypeElementVO>();
        if (idTemplate!=null){
            list.addAll(getArchetypeElementsInTemplate(idTemplate).values());
        }else{
            for (ArchetypeElementVO archetypeElementVO : getDelegate()._archetypeElementsById.values()) {
                if(idArchetype.equals(archetypeElementVO.getIdArchetype())){
                    list.add(archetypeElementVO);
                }
            }
        }
        return list;
    }

    public static ArrayList<ClusterVO> getClusters(ArchetypeElementVO archetypeElementVO){
        ArrayList<ClusterVO> clusters = new ArrayList<ClusterVO>();
        String[] pathArray = archetypeElementVO.getPath().split("\\/");
        StringBuffer clusterPathSB = new StringBuffer();
        clusterPathSB.append(archetypeElementVO.getIdArchetype());
        for (String pathNode : pathArray) {
            if (!pathNode.isEmpty()){
                clusterPathSB.append("/"+pathNode);
                ClusterVO clusterVO = Clusters.getClusterVO(archetypeElementVO.getIdTemplate(), clusterPathSB.toString());
                if (clusterVO!=null){
                    clusters.add(clusterVO);
                }
            }
        }
        return clusters;
    }

    public static ArchetypeElements getDelegate(){
        if (_instance == null){
            _instance = new ArchetypeElements();
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
package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVOBuilder;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public class ArchetypeElements {

    private final ArchetypeManager archetypeManager;
    private Map<String, ArchetypeElementVO> archetypeElementsById = null;
    private Map<String, Map<String, ArchetypeElementVO>> templateElementsByTemplateIdAndId = null;

    public static ArchetypeElementVO CURRENT_DATE_TIME =
            new ArchetypeElementVOBuilder()
                    .setName(OpenEHRLanguageManager.getMessage("CurrentDateTime"))
                    .setDescription(OpenEHRLanguageManager.getMessage("CurrentDateTime"))
                    .setType(OpenEHRDataValues.DV_DATE_TIME)
                    .createArchetypeElementVO();


    public ArchetypeElements(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        init();
    }


    public void init() {
        archetypeElementsById = new LinkedHashMap<>();
        templateElementsByTemplateIdAndId = new LinkedHashMap<>();
    }

    public void loadArchetypeElements(Collection<ArchetypeElementVO> archetypeElementVOs) {
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            registerArchetypeElement(archetypeElementVO);
        }
    }

    public void registerArchetypeElement(ArchetypeElementVO archetypeElementVO) {
        if (archetypeElementVO.getIdTemplate() == null) {
            archetypeElementsById.put(archetypeElementVO.getId(), archetypeElementVO);
        } else {
            getArchetypeElementsInTemplate(archetypeElementVO.getIdTemplate()).put(archetypeElementVO.getId(), archetypeElementVO);
        }
    }

    //TODO Should throw an exception when the element is not found
    public ArchetypeElementVO getArchetypeElement(String idTemplate, String idElement) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        if (idTemplate == null) {
            return archetypeElementsById.get(idElement);
        } else {
            return getArchetypeElementsInTemplate(idTemplate).get(idElement);
        }
    }

    public String getText(ArchetypeElementVO archetypeElementVO, String lang) {
        return getText(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), lang);
    }

    public String getText(String idTemplate, String idElement, String lang) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        ArchetypeElementVO archetypeElementVO = getArchetypeElement(idTemplate, idElement);
        if (archetypeElementVO != null) {
            ArchetypeTerm archetypeTem = archetypeManager.getArchetypeTerm(idTemplate, idElement, lang);
            if (archetypeTem != null) {
                return archetypeTem.getText();
            } else {
                return archetypeElementVO.getName();
            }
        } else {
            return "*UNKNOWN*";
        }
    }

    public String getDescription(ArchetypeElementVO archetypeElementVO, String lang) {
        return getDescription(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), lang);
    }

    public String getDescription(String idTemplate, String idElement, String lang) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        ArchetypeElementVO archetypeElementVO = getArchetypeElement(idTemplate, idElement);
        if (archetypeElementVO != null) {
            ArchetypeTerm archetypeTem = archetypeManager.getArchetypeTerm(idTemplate, idElement, lang);
            if (archetypeTem != null) {
                return archetypeTem.getDescription();
            } else {
                return archetypeElementVO.getDescription();
            }
        } else {
            return "*UNKNOWN*";
        }
    }

    private ArchetypeTerms getArchetypeTerms() {
        return this.archetypeManager.getArchetypeTerms();
    }

    private Map<String, ArchetypeElementVO> getArchetypeElementsInTemplate(String idTemplate) {
        Map<String, ArchetypeElementVO> elementsInTemplate =
                templateElementsByTemplateIdAndId.get(idTemplate);
        try {
            archetypeManager.getTemplates().getCMElement(idTemplate);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        if (elementsInTemplate == null) {
            elementsInTemplate = new LinkedHashMap<>();
            templateElementsByTemplateIdAndId.put(idTemplate, elementsInTemplate);
        }
        return elementsInTemplate;
    }

    public Collection<ArchetypeElementVO> getArchetypeElementsVO(String idArchetype, String idTemplate) {
        archetypeManager.loadArchetypesIfNeeded(idArchetype);
        archetypeManager.loadTemplateIfNeeded(idTemplate);
        Collection<ArchetypeElementVO> list = new ArrayList<ArchetypeElementVO>();
        if (idTemplate != null) {
            list.addAll(getArchetypeElementsInTemplate(idTemplate).values());
        } else {
            for (ArchetypeElementVO archetypeElementVO : archetypeElementsById.values()) {
                if (idArchetype.equals(archetypeElementVO.getIdArchetype())) {
                    list.add(archetypeElementVO);
                }
            }
        }
        return list;
    }

    public ArrayList<ClusterVO> getClusters(ArchetypeElementVO archetypeElementVO) {
        ArrayList<ClusterVO> clusters = new ArrayList<>();
        String[] pathArray = archetypeElementVO.getPath().split("\\/");
        StringBuilder clusterPathSB = new StringBuilder();
        clusterPathSB.append(archetypeElementVO.getIdArchetype());
        for (String pathNode : pathArray) {
            if (!pathNode.isEmpty()) {
                clusterPathSB.append("/").append(pathNode);
                ClusterVO clusterVO = this.archetypeManager.getClusters().getClusterVO(archetypeElementVO.getIdTemplate(), clusterPathSB.toString());
                if (clusterVO != null) {
                    clusters.add(clusterVO);
                }
            }
        }
        return clusters;
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
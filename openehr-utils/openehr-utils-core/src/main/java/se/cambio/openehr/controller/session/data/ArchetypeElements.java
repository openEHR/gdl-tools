package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVOBuilder;
import se.cambio.cm.model.archetype.vo.ArchetypeTermVO;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

import static java.lang.String.format;

public class ArchetypeElements {

    private final ArchetypeManager archetypeManager;
    private Map<String, ArchetypeElementVO> archetypeElementsById = null;
    private Map<String, Map<String, ArchetypeElementVO>> templateElementsByTemplateIdAndId = null;
    private ArchetypeElementVO currentDateTime;

    public ArchetypeElementVO getCurrentDateTimeArchetypeElementVO() {
        if (currentDateTime == null) {
            currentDateTime = new ArchetypeElementVOBuilder()
                    .setName(OpenEHRLanguageManager.getMessage("CurrentDateTime"))
                    .setDescription(OpenEHRLanguageManager.getMessage("CurrentDateTime"))
                    .setType(OpenEHRDataValues.DV_DATE_TIME)
                    .createArchetypeElementVO();
        }
        return currentDateTime;
    }


    public ArchetypeElements(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        init();
    }


    public void init() {
        archetypeElementsById = new LinkedHashMap<>();
        templateElementsByTemplateIdAndId = new LinkedHashMap<>();
    }

    public void loadArchetypeElements(
            String archetypeId,
            String templateId,
            Collection<ArchetypeElementVO> archetypeElementVOs) {
        cleanPreviousElements(archetypeId, templateId);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            registerArchetypeElement(archetypeElementVO);
        }
    }

    private void cleanPreviousElements(String archetypeId, String templateId) {
        if (templateId != null) {
            templateElementsByTemplateIdAndId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(archetypeElementsById.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    archetypeElementsById.remove(id);
                }
            }
        }
    }

    private void registerArchetypeElement(ArchetypeElementVO archetypeElementVO) {
        if (archetypeElementVO.getIdTemplate() == null) {
            archetypeElementsById.put(archetypeElementVO.getId(), archetypeElementVO);
        } else {
            getArchetypeElementsInTemplate(archetypeElementVO.getIdTemplate()).put(archetypeElementVO.getId(), archetypeElementVO);
        }
    }

    public ArchetypeElementVO getArchetypeElement(String templateId, String elementId) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, elementId);
        if (templateId == null) {
            if (!archetypeElementsById.containsKey(elementId)) {
                throw new RuntimeException(format("Could not find element '%s'", elementId));
            }
            return archetypeElementsById.get(elementId);
        } else {
            if (!getArchetypeElementsInTemplate(templateId).containsKey(elementId)) {
                throw new RuntimeException(format("Could not find element '%s' for template '%s'", elementId, templateId));
            }
            return getArchetypeElementsInTemplate(templateId).get(elementId);
        }
    }

    public boolean isArchetypeElement(String templateId, String elementId) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, elementId);
        if (templateId == null) {
            return archetypeElementsById.containsKey(elementId);
        } else {
            return getArchetypeElementsInTemplate(templateId).containsKey(elementId);
        }
    }

    public String getText(ArchetypeElementVO archetypeElementVO, String lang) {
        return getText(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), lang);
    }

    public String getText(String templateId, String elementId, String lang) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, elementId);
        ArchetypeElementVO archetypeElementVO = getArchetypeElement(templateId, elementId);
        if (archetypeElementVO != null) {
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(templateId, elementId, lang);
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

    public String getDescription(String templateId, String elementId, String lang) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, elementId);
        ArchetypeElementVO archetypeElementVO = getArchetypeElement(templateId, elementId);
        if (archetypeElementVO != null) {
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(templateId, elementId, lang);
            if (archetypeTem != null) {
                return archetypeTem.getDescription();
            } else {
                return archetypeElementVO.getDescription();
            }
        } else {
            return "*UNKNOWN*";
        }
    }

    private Map<String, ArchetypeElementVO> getArchetypeElementsInTemplate(String templateId) {
        Map<String, ArchetypeElementVO> elementsInTemplate =
                templateElementsByTemplateIdAndId.get(templateId);
        archetypeManager.getTemplates().getCMElement(templateId);
        if (elementsInTemplate == null) {
            elementsInTemplate = new LinkedHashMap<>();
            templateElementsByTemplateIdAndId.put(templateId, elementsInTemplate);
        }
        return elementsInTemplate;
    }

    public Collection<ArchetypeElementVO> getArchetypeElementsVO(String idArchetype, String templateId) {
        archetypeManager.loadArchetypesIfNeeded(idArchetype);
        archetypeManager.loadTemplateIfNeeded(templateId);
        Collection<ArchetypeElementVO> list = new ArrayList<>();
        if (templateId != null) {
            list.addAll(getArchetypeElementsInTemplate(templateId).values());
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
package se.cambio.openehr.controller.session.data;

import se.cambio.cm.model.archetype.vo.ArchetypeTermVO;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

import static java.lang.String.format;

public class Clusters {
    private final ArchetypeManager archetypeManager;
    private Map<String, ClusterVO> clustersById = null;
    private Map<String, Map<String, ClusterVO>> templateClustersByTemplateIdAndId = null;


    public Clusters(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        init();
    }

    public void init() {
        clustersById = new HashMap<>();
        templateClustersByTemplateIdAndId = new HashMap<>();
    }

    public void loadClusters(
            String archetypeId,
            String templateId,
            Collection<ClusterVO> clusterVOs) {
        cleanPreviousElements(archetypeId, templateId);
        for (ClusterVO clusterVO : clusterVOs) {
            registerCluster(clusterVO);
        }
    }

    private void cleanPreviousElements(String archetypeId, String templateId) {
        if (templateId != null) {
            templateClustersByTemplateIdAndId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(clustersById.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    clustersById.remove(id);
                }
            }
        }
    }

    private void registerCluster(ClusterVO clusterVO) {
        String clusterId = clusterVO.getId();
        Map<String, ClusterVO> map;
        if (clusterVO.getIdTemplate() == null) {
            map = clustersById;
        } else {
            map = getClusterVOMap(clusterVO.getIdTemplate());
        }
        map.put(clusterId, clusterVO);
    }

    private Map<String, ClusterVO> getClusterVOMap(String templateId) {
        return templateClustersByTemplateIdAndId.computeIfAbsent(templateId, k -> new LinkedHashMap<>());
    }

    public ClusterVO getClusterVO(String templateId, String clusterId) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, clusterId);
        Map<String, ClusterVO> clusterMap = getClusterMap(templateId);
        if (!clusterMap.containsKey(clusterId)) {
            String complexClusterId = findComplexClusterId(clusterId, clusterMap);
            if (complexClusterId == null) {
                throw new RuntimeException(format("Could not find cluster '%s' in template '%s'", clusterId, templateId));
            } else {
                clusterId = complexClusterId;
            }
        }
        return clusterMap.get(clusterId);
    }

    private Map<String, ClusterVO> getClusterMap(String templateId) {
        if (templateId != null) {
            return getClusterVOMap(templateId);
        } else {
            return clustersById;
        }
    }

    private String findComplexClusterId(String clusterId, Map<String, ClusterVO> map) {
        for (String clusterIdAux: map.keySet()) {
            String simplifiedClusterId = getSimplifiedClusterId(clusterIdAux);
            if (clusterId.equals(simplifiedClusterId)) {
                return clusterIdAux;
            }
        }
        return null;
    }

    private String getSimplifiedClusterId(String clusterIdAux) {
        return clusterIdAux.replaceAll("\\[[^]]+]","");
    }

    public boolean isCluster(String templateId, String clusterId) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, clusterId);
        Map<String, ClusterVO> clusterMap = getClusterMap(templateId);
        return clusterMap.containsKey(clusterId)
                || clusterMap.containsKey(findComplexClusterId(clusterId, clusterMap));
    }

    public Collection<ClusterVO> getSections(String templateId) {
        Collection<ClusterVO> sections = new ArrayList<>();
        for (ClusterVO clusterVO : getClusterVOMap(templateId).values()) {
            if (clusterVO.getType().equals(OpenEHRConst.SECTION)) {
                sections.add(clusterVO);
            }
        }
        return sections;
    }

    public String getText(ClusterVO clusterVO, String lang) throws InternalErrorException {
        return getText(clusterVO.getIdTemplate(), clusterVO.getId(), lang);
    }

    public String getText(String templateId, String elementId, String lang) {
        ClusterVO clusterVO = getClusterVO(templateId, elementId);
        if (clusterVO != null) {
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(templateId, elementId, lang);
            if (archetypeTem != null) {
                return archetypeTem.getText();
            } else {
                return clusterVO.getName();
            }
        } else {
            return "*UNKNOWN*";
        }
    }

    public String getDescription(ClusterVO clusterVO, String lang) throws InternalErrorException {
        return getDescription(clusterVO.getIdTemplate(), clusterVO.getId(), lang);
    }

    public String getDescription(String templateId, String elementId, String lang) throws InternalErrorException {
        ClusterVO clusterVO = getClusterVO(templateId, elementId);
        if (clusterVO != null) {
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(templateId, elementId, lang);
            if (archetypeTem != null) {
                return archetypeTem.getDescription();
            } else {
                return clusterVO.getDescription();
            }
        } else {
            return "*UNKNOWN*";
        }
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
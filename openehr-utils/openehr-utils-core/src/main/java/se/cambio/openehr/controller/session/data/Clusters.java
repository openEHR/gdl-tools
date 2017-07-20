package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.ontology.ArchetypeTerm;
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
        if (clusterVO.getIdTemplate() == null) {
            clustersById.put(clusterVO.getId(), clusterVO);
        } else {
            getClusterVOMap(clusterVO.getIdTemplate()).put(clusterVO.getId(), clusterVO);
        }
    }

    private Map<String, ClusterVO> getClusterVOMap(String templateId) {
        return templateClustersByTemplateIdAndId.computeIfAbsent(templateId, k -> new LinkedHashMap<>());
    }

    public ClusterVO getClusterVO(String templateId, String idCluster) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, idCluster);
        if (templateId != null) {
            if (!getClusterVOMap(templateId).containsKey(idCluster)) {
                throw new RuntimeException(format("Could not find cluster '%s' in template '%s'", idCluster, templateId));
            }
            return getClusterVOMap(templateId).get(idCluster);
        } else {
            if (!clustersById.containsKey(idCluster)) {
                throw new RuntimeException(format("Could not find cluster '%s'", idCluster));
            }
            return clustersById.get(idCluster);
        }
    }

    public boolean isCluster(String templateId, String idCluster) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, idCluster);
        if (templateId != null) {
            return getClusterVOMap(templateId).containsKey(idCluster);
        } else {
            return clustersById.containsKey(idCluster);
        }
    }

    public Collection<ClusterVO> getSections(String templateId) {
        Collection<ClusterVO> sections = new ArrayList<>();
        for (ClusterVO clusterVO : getClusterVOMap(templateId).values()) {
            if (clusterVO.getRMType().equals(OpenEHRConst.SECTION)) {
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
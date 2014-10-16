package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class Clusters {
    private final ArchetypeObjectBundles archetypeObjectBundles;
    private  Map<String, ClusterVO> _clustersById = null;
    private Map<String, Map<String, ClusterVO>> _templateClustersByTemplateIdAndId = null;


    public Clusters(ArchetypeObjectBundles archetypeObjectBundles){
        this.archetypeObjectBundles = archetypeObjectBundles;
        init();
    }

    public void init(){
        _clustersById = new HashMap<String, ClusterVO>();
        _templateClustersByTemplateIdAndId = new HashMap<String, Map<String,ClusterVO>>();
    }

    public void loadClusters(Collection<ClusterVO> clusterVOs){
        for (ClusterVO clusterVO : clusterVOs) {
            registerCluster(clusterVO);
        }
    }

    public void registerCluster(ClusterVO clusterVO){
        if (clusterVO.getIdTemplate()==null){
            _clustersById.put(clusterVO.getId(), clusterVO);
        }else{
            getClusterVOMap(clusterVO.getIdTemplate()).put(clusterVO.getId(), clusterVO);
        }
    }

    private Map<String, ClusterVO> getClusterVOMap(String idTemplate){
        Map<String, ClusterVO> map = _templateClustersByTemplateIdAndId.get(idTemplate);
        if (map==null){
            map = new LinkedHashMap<String, ClusterVO>();
            _templateClustersByTemplateIdAndId.put(idTemplate, map);
        }
        return map;
    }

    public ClusterVO getClusterVO(String idTemplate, String idCluster){
        if (idTemplate!=null){
            return getClusterVOMap(idTemplate).get(idCluster);
        }else{
            return _clustersById.get(idCluster);
        }
    }

    public ClusterVO getClusterVOWithCardinalityGT1(String idTemplate, String idCluster){
        ClusterVO clusterVO = getClusterVO(idTemplate, idCluster);
        while (clusterVO!=null){
            if (clusterVO.getUpperCardinality()==null || clusterVO.getUpperCardinality()>1){
                return clusterVO;
            }else{
                clusterVO = getClusterVO(idTemplate, clusterVO.getIdParent());
            }
        }
        return null;
    }

    public Collection<ClusterVO> getSections(String idTemplate){
        Collection<ClusterVO> sections = new ArrayList<ClusterVO>();
        for (ClusterVO clusterVO : getClusterVOMap(idTemplate).values()) {
            if(clusterVO.getRMType().equals(OpenEHRConst.SECTION)){
                sections.add(clusterVO);
            }
        }
        return sections;
    }

    public String getText(ClusterVO clusterVO, String lang) throws InternalErrorException {
        return getText(clusterVO.getIdTemplate(), clusterVO.getId(), lang);
    }

    public String getText(String idTemplate, String idElement, String lang) {
        ClusterVO clusterVO = getClusterVO(idTemplate, idElement);
        if (clusterVO!=null){
            ArchetypeTerm archetypeTem = archetypeObjectBundles.getArchetypeTerm(idTemplate, idElement, lang);
            if (archetypeTem!=null){
                return archetypeTem.getText();
            }else{
                return clusterVO.getName();
            }
        }else{
            return "*UNKNOWN*";
        }
    }

    public String getDescription(ClusterVO clusterVO, String lang) throws InternalErrorException {
        return getDescription(clusterVO.getIdTemplate(), clusterVO.getId(), lang);
    }

    public String getDescription(String idTemplate, String idElement, String lang) throws InternalErrorException {
        ClusterVO clusterVO = getClusterVO(idTemplate, idElement);
        if (clusterVO!=null){
            ArchetypeTerm archetypeTem = archetypeObjectBundles.getArchetypeTerm(idTemplate, idElement, lang);
            if (archetypeTem!=null){
                return archetypeTem.getDescription();
            }else{
                return clusterVO.getDescription();
            }
        }else{
            return "*UNKNOWN*";
        }
    }

    private ArchetypeTerms getArchetypeTerms() {
        return this.archetypeObjectBundles.getArchetypeTerms();
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
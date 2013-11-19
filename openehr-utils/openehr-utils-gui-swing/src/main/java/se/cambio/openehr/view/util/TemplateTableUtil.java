package se.cambio.openehr.view.util;

import org.apache.log4j.Logger;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.Clusters;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.model.archetype.vo.PathableVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.view.treetables.models.TemplateTreeTableModel;

import java.util.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 12:21
 */
public class TemplateTableUtil {

    public static Map<ClusterVO, List<PathableVO>> generatePathableMap(String archetypeId, String templateId, boolean showStructure){
        Map<ClusterVO, List<PathableVO>> pathablesMap = new LinkedHashMap<ClusterVO, List<PathableVO>>();
        for (ArchetypeElementVO aeVO : ArchetypeElements.getArchetypeElementsVO(archetypeId, templateId)){
            addToPathablesMap(templateId, pathablesMap, aeVO, showStructure);
        }
        return pathablesMap;
    }

    public static void addToPathablesMap(String templateId, Map<ClusterVO, List<PathableVO>> pathablesMap, PathableVO pathableVO, boolean showStructure){
        if (pathableVO.getUpperCardinality()!=null && pathableVO.getUpperCardinality()==0){
            return;
        }
        String parentId = pathableVO.getIdParent();
        if (parentId==null){
            Logger.getLogger(TemplateTreeTableModel.class).warn("Pathable '"+pathableVO.getId()+"' has null parent!");
            return;
        }
        ClusterVO clusterVO = Clusters.getClusterVO(templateId, parentId);
        while ( !showStructure &&
                clusterVO.getIdParent()!=null &&
                !(OpenEHRConst.CLUSTER.equals(clusterVO.getRMType()) ||
                OpenEHRConst.SECTION.equals(clusterVO.getRMType()))){
            clusterVO = Clusters.getClusterVO(templateId, clusterVO.getIdParent());
        }

        if (clusterVO.getUpperCardinality()!=null && clusterVO.getUpperCardinality()==0){
            return;
        }

        List<PathableVO> children = pathablesMap.get(clusterVO);
        if (children==null){
            children = new ArrayList<PathableVO>();
            pathablesMap.put(clusterVO, children);
        }
        if (!children.contains(pathableVO)){
            children.add(pathableVO);
        }
        if (clusterVO.getIdParent()!=null){
            addToPathablesMap(templateId, pathablesMap, clusterVO, showStructure);
        }
    }

    public static List<ArchetypeElementVO> getArchetypeElementsInCluster(String archetypeId, String templateId, ClusterVO clusterVO){
        List<ArchetypeElementVO> archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
        String clusterId = clusterVO.getId();
        for (ArchetypeElementVO aeVO : ArchetypeElements.getArchetypeElementsVO(archetypeId, templateId)){
            if (aeVO.getId().startsWith(clusterId) && hasOcurrence(aeVO)){
                archetypeElementVOs.add(aeVO);
            }
        }
        return archetypeElementVOs;
    }

    public static boolean hasOcurrence(PathableVO pathableVO){
        Integer upperOcurrence = pathableVO.getUpperCardinality();
        if(upperOcurrence==null || upperOcurrence>=1){
           if (pathableVO.getIdParent()!=null){
                return hasOcurrence(Clusters.getClusterVO(pathableVO.getIdTemplate(), pathableVO.getIdParent()));
           }else{
               return true;
           }
        }else{
          return false;
        }
    }

    public static ClusterVO getRootCluster( Map<ClusterVO, List<PathableVO>> pathablesMap){
        for (ClusterVO clusterVO: pathablesMap.keySet()){
            if (clusterVO.getIdParent()==null){
                return clusterVO;
            }
        }
        return null;
    }
    public static List<ClusterVO> getSectionClusters( Map<ClusterVO, List<PathableVO>> pathablesMap){
        List<ClusterVO> sectionClusterVO = new ArrayList<ClusterVO>();
        for (ClusterVO clusterVO: pathablesMap.keySet()){
            if (clusterVO.getRMType().equals(OpenEHRConst.SECTION)){
                sectionClusterVO.add(clusterVO);
            }
        }
        return sectionClusterVO;
    }

}

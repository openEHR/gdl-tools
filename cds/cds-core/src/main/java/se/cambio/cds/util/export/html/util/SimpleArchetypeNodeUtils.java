package se.cambio.cds.util.export.html.util;

import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Clusters;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-15
 * Time: 13:31
 */
public class SimpleArchetypeNodeUtils {

    public static SimpleArchetypeNode getSimpleArchetypeNode(String archetypeId, String idTemplate, String lang){
        ArchetypeDTO archetypeVO = Archetypes.getArchetypeDTO(archetypeId);
        SimpleArchetypeNode rootNode = new SimpleArchetypeNode("/", archetypeVO.getName(), archetypeVO.getDescription(), OpenEHRConstUI.getIconName(archetypeVO.getRMName()));
        Map<String, SimpleArchetypeNode> rmNodes = new HashMap<String, SimpleArchetypeNode>();
        Map<String, SimpleArchetypeNode> clusters = new HashMap<String, SimpleArchetypeNode>();
        Collection<ArchetypeElementVO> archetypeElementVOs = ArchetypeElements.getArchetypeElementsVO(archetypeId, idTemplate);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SimpleArchetypeNode rmNode = getRMNode(rootNode, rmNodes, archetypeElementVO.getPath());
            SimpleArchetypeNode clusterNode =
                    getClusterNode(
                            idTemplate,
                            archetypeElementVO.getIdParent(),
                            rmNode, clusters, lang);
            SimpleArchetypeNode nodoOrigen = createElementNode(archetypeElementVO, lang);
            clusterNode.getChildren().add(nodoOrigen);
        }
        return rootNode;
    }

    private static SimpleArchetypeNode createElementNode(ArchetypeElementVO archetypeElementVO, String lang){
        String name = ArchetypeElements.getText(archetypeElementVO, lang);
        String desc = ArchetypeElements.getDescription(archetypeElementVO, lang);
        return new SimpleArchetypeNode(archetypeElementVO.getId(), name, desc, OpenEHRDataValuesUI.getDVIconName(archetypeElementVO.getRMType()));
    }

    public static SimpleArchetypeNode getRMNode(
            SimpleArchetypeNode rootNode,
            Map<String, SimpleArchetypeNode> rmNodes,
            String path){
        String idRMClassifier = getIdRMClassifier(path);
        if (idRMClassifier.contains("/")){
            rootNode = getRMNode(rootNode, rmNodes, "/"+idRMClassifier.substring(0, idRMClassifier.lastIndexOf("/")));
            idRMClassifier = idRMClassifier.substring(idRMClassifier.lastIndexOf("/")+1, idRMClassifier.length());
        }
        SimpleArchetypeNode rmNode = rmNodes.get(idRMClassifier);
        if (rmNode==null){
            rmNode = new SimpleArchetypeNode(idRMClassifier, idRMClassifier, idRMClassifier, "element.png");
            rmNodes.put(idRMClassifier, rmNode);
            rootNode.getChildren().add(rmNode);
        }
        return rmNode;
    }

    public static String getIdRMClassifier(String path){
        int index = path.indexOf("[", 1);
        if (index>0){
            return path.substring(1, index);
        }else{
            index = path.indexOf("/", 1);
            if (index>0){
                return path.substring(1, index);
            }else{
                return path.substring(1, path.length());
            }
        }
    }

    public static SimpleArchetypeNode getClusterNode(
            String idTemplate, String idCluster,
            SimpleArchetypeNode rootNode,
            Map<String, SimpleArchetypeNode> clusters,
            String lang){
        if (idCluster!=null && !idCluster.endsWith("/")){
            ClusterVO clusterVO = Clusters.getClusterVO(idTemplate, idCluster);
            if (clusterVO!=null){
                if ((!OpenEHRConst.SECTION.equals(clusterVO.getRMType()) &&
                        !OpenEHRConst.CLUSTER.equals(clusterVO.getRMType()))){
                    //Skip node  (simplified tree)
                    return getClusterNode(idTemplate, clusterVO.getIdParent(),
                            rootNode, clusters, lang);
                }
            }else{
                ExceptionHandler.handle(new Exception("Cluster id '" + idCluster + "' not found"));
                return rootNode;
            }
            SimpleArchetypeNode clusterNode = clusters.get(idCluster);
            if(clusterNode==null){
                SimpleArchetypeNode parentNode =
                        getClusterNode(idTemplate, clusterVO.getIdParent(),
                                rootNode, clusters, lang);
                clusterNode = createClusterNode(clusterVO, lang);
                clusters.put(idCluster, clusterNode);
                parentNode.getChildren().add(clusterNode);

            }
            return clusterNode;
        } else{
            return rootNode;
        }
    }

    public static SimpleArchetypeNode createClusterNode(ClusterVO clusterVO, String lang){
        String upperNumOcurrences =
                (clusterVO.getUpperCardinality()==null?" [*]":clusterVO.getUpperCardinality()>1?" ["+clusterVO.getUpperCardinality()+"]":"");
        String name = Clusters.getText(clusterVO, lang);
        String desc = Clusters.getDescription(clusterVO, lang);
        return new SimpleArchetypeNode(
                clusterVO.getId(),
                name+upperNumOcurrences,
                desc, OpenEHRConstUI.getIconName(clusterVO.getRMType()));
    }

}

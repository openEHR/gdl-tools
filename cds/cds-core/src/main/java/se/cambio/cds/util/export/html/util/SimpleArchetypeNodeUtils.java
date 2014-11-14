package se.cambio.cds.util.export.html.util;

import org.openehr.rm.common.archetyped.Locatable;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.util.export.json.DVDefSerializer;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Clusters;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-15
 * Time: 13:31
 */
public class SimpleArchetypeNodeUtils {

    public static SimpleArchetypeNode getSimpleArchetypeNode(String archetypeId, String idTemplate, String lang, ArchetypeManager archetypeManager) throws InternalErrorException, InstanceNotFoundException {
        SimpleArchetypeNode rootNode = new SimpleArchetypeNode("/", archetypeId, archetypeId, OpenEHRConstUI.getIconName(Archetypes.getEntryType(archetypeId)));
        Map<String, SimpleArchetypeNode> rmNodes = new HashMap<String, SimpleArchetypeNode>();
        Map<String, SimpleArchetypeNode> clusters = new HashMap<String, SimpleArchetypeNode>();
        Collection<ArchetypeElementVO> archetypeElementVOs = archetypeManager.getArchetypeElements().getArchetypeElementsVO(archetypeId, idTemplate);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SimpleArchetypeNode rmNode = getRMNode(rootNode, rmNodes, archetypeElementVO.getPath());
            SimpleArchetypeNode clusterNode =
                    getClusterNode(
                            idTemplate,
                            archetypeElementVO.getIdParent(),
                            rmNode, clusters, lang, archetypeManager.getClusters());
            SimpleArchetypeNode nodoOrigen = createElementNode(archetypeElementVO, lang, archetypeManager.getArchetypeElements());
            clusterNode.getChildren().add(nodoOrigen);
        }
        return rootNode;
    }

    private static SimpleArchetypeNode createElementNode(ArchetypeElementVO archetypeElementVO, String lang, ArchetypeElements archetypeElements){
        String name = archetypeElements.getText(archetypeElementVO, lang);
        String desc = archetypeElements.getDescription(archetypeElementVO, lang);
        return new SimpleArchetypeNode(archetypeElementVO.getId(), name, desc, OpenEHRDataValuesUI.getDVIconName(archetypeElementVO.getRMType()));
    }

    public static SimpleArchetypeNode getSimpleArchetypeNode(String archetypeId, String idTemplate, Locatable locatable, String lang, ArchetypeManager archetypeManager) throws InternalErrorException, InstanceNotFoundException {
        SimpleArchetypeNode rootNode = new SimpleArchetypeNode("/", archetypeId, archetypeId, OpenEHRConstUI.getIconName(Archetypes.getEntryType(archetypeId)));
        Map<String, SimpleArchetypeNode> rmNodes = new HashMap<String, SimpleArchetypeNode>();
        Map<String, SimpleArchetypeNode> clusters = new HashMap<String, SimpleArchetypeNode>();
        Collection<ArchetypeElementVO> archetypeElementVOs = archetypeManager.getArchetypeElements().getArchetypeElementsVO(archetypeId, idTemplate);
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
            SimpleArchetypeNode rmNode = getRMNode(rootNode, rmNodes, archetypeElementVO.getPath());
            SimpleArchetypeNode clusterNode =
                    getClusterNode(
                            idTemplate,
                            archetypeElementVO.getIdParent(),
                            rmNode, clusters, lang, archetypeManager.getClusters());
            SimpleArchetypeNode node = createElementNode(archetypeElementVO, locatable, lang, archetypeManager.getArchetypeElements());
            clusterNode.getChildren().add(node);
        }
        return rootNode;
    }

    private static SimpleArchetypeNode createElementNode(ArchetypeElementVO archetypeElementVO, Locatable locatable, String lang, ArchetypeElements archetypeElements){
        String name = archetypeElements.getText(archetypeElementVO, lang);
        String desc = archetypeElements.getDescription(archetypeElementVO, lang);
        Object obj = locatable.itemAtPath(archetypeElementVO.getPath()+"/value");
        String valueStr = null;
        if (obj instanceof DataValue){
            valueStr = DVDefSerializer.getReadableValue((DataValue)obj, null);
        }
        StringBuilder sb = new StringBuilder();
        sb.append(name);
        if (valueStr != null){
            sb.append(" = ");
            sb.append(valueStr);
        }
        return new SimpleArchetypeNode(archetypeElementVO.getId(), sb.toString(), desc, OpenEHRDataValuesUI.getDVIconName(archetypeElementVO.getRMType()));
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
        if (index > 0){
            return path.substring(1, index);
        }else{
            index = path.indexOf("/", 1);
            if (index > 0){
                return path.substring(1, index);
            }else{
                return path.substring(1, path.length());
            }
        }
    }

    public static SimpleArchetypeNode getClusterNode(
            String idTemplate, String idCluster,
            SimpleArchetypeNode rootNode,
            Map<String, SimpleArchetypeNode> clusterList,
            String lang, Clusters clusters) throws InternalErrorException {
        if (idCluster!=null && !idCluster.endsWith("/")){
            ClusterVO clusterVO = clusters.getClusterVO(idTemplate, idCluster);
            if (clusterVO == null){
                ExceptionHandler.handle(new Exception("Cluster id '" + idCluster + "' not found"));
                return rootNode;
            }
            SimpleArchetypeNode clusterNode = clusterList.get(idCluster);
            if(clusterNode == null){
                SimpleArchetypeNode parentNode =
                        getClusterNode(idTemplate, clusterVO.getIdParent(),
                                rootNode, clusterList, lang, clusters);
                clusterNode = createClusterNode(clusterVO, lang, clusters);
                clusterList.put(idCluster, clusterNode);
                parentNode.getChildren().add(clusterNode);

            }
            return clusterNode;
        } else{
            return rootNode;
        }
    }

    public static SimpleArchetypeNode createClusterNode(ClusterVO clusterVO, String lang, Clusters clusters) throws InternalErrorException {
        String upperNumOcurrences =
                (clusterVO.getUpperCardinality()==null?" [*]":clusterVO.getUpperCardinality()>1?" ["+clusterVO.getUpperCardinality()+"]":"");
        String name = clusters.getText(clusterVO, lang);
        String desc = clusters.getDescription(clusterVO, lang);
        return new SimpleArchetypeNode(
                clusterVO.getId(),
                name+upperNumOcurrences,
                desc, OpenEHRConstUI.getIconName(clusterVO.getRMType()));
    }

}

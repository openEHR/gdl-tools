package se.cambio.cds.gdl.editor.view.util;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.openehr.controller.session.data.Clusters;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeWithIcon;

import java.util.Map;

public class ClusterNodesUtil {

    public static SelectableNode<Object> getClusterNode(
            String idTemplate, String idCluster,
            SelectableNode<Object> rootNode,
            Map<Object, SelectableNode<Object>> clusters,
            boolean singleSelection,
            boolean simplifiedTree){
        if (idCluster!=null && !idCluster.endsWith("/")){
            ClusterVO clusterVO = Clusters.getClusterVO(idTemplate, idCluster);
            if (clusterVO!=null){
                if ((simplifiedTree &&
                        !OpenEHRConst.SECTION.equals(clusterVO.getRMType()) &&
                        !OpenEHRConst.CLUSTER.equals(clusterVO.getRMType()))){
                    //Skip node
                    return getClusterNode(idTemplate, clusterVO.getIdParent(),
                            rootNode, clusters, singleSelection, simplifiedTree);
                }
            }else{
                ExceptionHandler.handle(new Exception("Cluster id '" + idCluster + "' not found"));
                return rootNode;
            }
            SelectableNode<Object> clusterNode = clusters.get(idCluster);
            if(clusterNode==null){

                SelectableNode<Object> parentNode =
                        getClusterNode(idTemplate, clusterVO.getIdParent(),
                                rootNode, clusters, singleSelection, simplifiedTree);
                clusterNode = createClusterNode(clusterVO, null, singleSelection);
                clusters.put(idCluster, clusterNode);
                parentNode.add(clusterNode);

            }
            return clusterNode;
        } else{
            return rootNode;
        }
    }

    public static SelectableNode<Object> getRMNode(
            SelectableNode <Object> rootNode,
            Map<String, SelectableNode<Object>> rmNodes,
            String path){
        String idRMClassifier = getIdRMClassifier(path);
        if (idRMClassifier.contains("/")){
            rootNode = getRMNode(rootNode, rmNodes, "/"+idRMClassifier.substring(0, idRMClassifier.lastIndexOf("/")));
            idRMClassifier = idRMClassifier.substring(idRMClassifier.lastIndexOf("/")+1, idRMClassifier.length());
        }
        SelectableNode<Object> rmNode = rmNodes.get(idRMClassifier);
        if (rmNode==null){
            rmNode = new SelectableNodeWithIcon<Object>(
                    idRMClassifier, null, true, false,
                    GDLEditorImageUtil.OBJECT_ICON);
            rmNodes.put(idRMClassifier, rmNode);
            rootNode.add(rmNode);
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

    public static SelectableNode<Object> createClusterNode(ClusterVO clusterVO, Object object, boolean singleSelection){
        String upperNumOcurrences =
                (clusterVO.getUpperCardinality()==null?" [*]":clusterVO.getUpperCardinality()>1?" ["+clusterVO.getUpperCardinality()+"]":"");
        return new SelectableNodeWithIcon<Object>(
                clusterVO.getName()+upperNumOcurrences, object ,singleSelection, false,
                OpenEHRConstUI.getIcon(clusterVO.getRMType()),
                clusterVO.getDescription());
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
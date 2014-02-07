/*
 * Created on 02-nov-2006
 *


 */
package se.cambio.openehr.view.util;

import se.cambio.openehr.view.trees.SelectableNode;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;


/**
 * @author icorram
 *
 */
public class NodeConversor {

    public static enum SearchType{
        SEARCH_ONLY_LEAVES, SEARCH_ONLY_PARENT, SEARCH_ALL
    }

    public static void  setAllVisible(SelectableNode<?> raizNodo){
        raizNodo.setVisible(true);
        Enumeration<?> e = raizNodo.getAllchildren();
        while (e.hasMoreElements()){
            NodeConversor.setAllVisible((SelectableNode<?>)e.nextElement());
        }
    }

    public static void filterByText(SelectableNode<?> raizNodo, String filtro){
        boolean visible = false;
        if (raizNodo.getDescripcion()!=null){
            if (filtro.trim().length()>0){
                String desc1 = Normalizer.normalize(raizNodo.getDescripcion(), Normalizer.Form.NFD);
                desc1 = FormatConverter.textWithoutPunctuation(desc1);
                String desc2 = Normalizer.normalize(filtro, Normalizer.Form.NFD);
                desc2 = FormatConverter.textWithoutPunctuation(desc2);
                visible = desc1.contains(desc2);
            }else{
                visible = true;
            }
        }
        if (raizNodo.getSeleccionUnica()){
            raizNodo.setSelected(false);
        }
        raizNodo.setVisible(visible);
        if (visible){
            filtro = new String();
        }
        if (!raizNodo.isLeaf()){
            Enumeration<?> e = raizNodo.getAllchildren();
            while (e.hasMoreElements()){
                NodeConversor.filterByText(
                        (SelectableNode<?>)e.nextElement(),
                        filtro);
            }
            if (raizNodo.getChildCount()!=0) {
                raizNodo.setVisible(true);
            }
        }
    }

    public static Object getSelectedObject(SelectableNode<?> nodoRaiz){
        SelectableNode<?> selectedNode = getSelectedNode(nodoRaiz, false);
        return selectedNode!=null?selectedNode.getObject():null;
    }

    public static Object getSelectedObject(SelectableNode<?> nodoRaiz, boolean allowParent){
        SelectableNode<?> selectedNode = getSelectedNode(nodoRaiz, allowParent);
        return selectedNode!=null?selectedNode.getObject():null;
    }

    public static SelectableNode<?> getSelectedNode(SelectableNode<?> nodoRaiz, boolean allowParent){
        Enumeration<?> e = nodoRaiz.getAllchildren();
        while (e.hasMoreElements()){
            SelectableNode<?> nodo = (SelectableNode<?>)e.nextElement();
            if (nodo.getSeleccionado() && (allowParent || nodo.isLeaf() || !nodo.getSeleccionUnica())){
                return nodo;
            }else {
                if (!nodo.isLeaf() && nodo.getContineneSeleccionado()){
                    return getSelectedNode(nodo, allowParent);
                }
            }
        }
        return null;
    }

    public static Collection<Object> getSelectedObjects(SelectableNode<?> nodoRaiz){
        return getSelectedObjects(nodoRaiz, SearchType.SEARCH_ALL);
    }

    public static Collection<Object> getSelectedObjects(SelectableNode<?> nodoRaiz, SearchType searchType){
        Collection<SelectableNode<?>> selectedNodes = new ArrayList<SelectableNode<?>>();
        addSelectedNodes(nodoRaiz, selectedNodes, searchType);
        Collection<Object> objs = new ArrayList<Object>();
        for (SelectableNode<?> nodoSeleccionable : selectedNodes) {
            objs.add(nodoSeleccionable.getObject());
        }
        return objs;
    }

    public static void addSelectedNodes(SelectableNode<?> nodoRaiz, Collection<SelectableNode<?>> selectedNodes, SearchType searchType){
        Enumeration<?> e = nodoRaiz.getAllchildren();
        while (e.hasMoreElements()){
            SelectableNode<?> node = (SelectableNode<?>)e.nextElement();
            if (node.getSeleccionado()){
                if (searchType!=SearchType.SEARCH_ONLY_LEAVES || node.isLeaf()){
                    selectedNodes.add(node);
                }
                if (searchType!=SearchType.SEARCH_ONLY_PARENT){
                    addSelectedNodes(node, selectedNodes, searchType);
                }
            }else if (node.getContineneSeleccionado()){
                if (searchType!=SearchType.SEARCH_ONLY_PARENT || !node.isLeaf()){
                    addSelectedNodes(node, selectedNodes, searchType);
                }
            }
        }
    }


    public static boolean selectObject(SelectableNode<?> nodoRaiz, Object obj){
        Enumeration<?> e = nodoRaiz.getAllchildren();
        while (e.hasMoreElements()){
            SelectableNode<?> node = (SelectableNode<?>)e.nextElement();
            if (obj.equals(node.getObject())){
                node.setSelected(true);
                node.cambioEstado(node);
                return true;
            }else{
                boolean found = selectObject(node, obj);
                if (found){
                    return true;
                }
            }
        }
        return false;
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
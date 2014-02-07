package se.cambio.openehr.view.trees;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import java.awt.*;
import java.util.Enumeration;
import java.util.Vector;

/**
 * @author icorram
 *
 */
public class SelectableNode<E> extends DefaultMutableTreeNode {

    /**
     *
     */
    private static final long serialVersionUID = -7670845142505301936L;
    private Boolean _seleccionado = Boolean.FALSE;
    private Boolean _contineneSeleccionado = Boolean.FALSE;
    private Boolean _visible = Boolean.TRUE;
    private String _descripcion = null;
    private String _tooltip = null;
    private boolean _seleccionUnica = false;
    private boolean _hierarchySelection = true;
    private boolean _bold = false;
    private Color _foreground = null;
    private boolean _cursiva = false;
    private E _objeto = null;
    private SelectableNode<?> _parent = null;
    private Vector<SelectableNode<?>> _visibleChildren = new Vector<SelectableNode<?>>();

    /**
     * An enumeration that is always empty. This is used when an enumeration
     * of a leaf node's children is requested.
     */

    public SelectableNode() {
        super();
    }

    public SelectableNode(String descripcion) {
        super(descripcion);
        _descripcion = descripcion;
    }

    public SelectableNode(String descripcion, E objeto) {
        super(descripcion);
        _descripcion = descripcion;
        _objeto = objeto;
    }

    public SelectableNode(String descripcion,
                          E objeto, Boolean seleccionado) {
        super(descripcion);
        _seleccionado = seleccionado;
        _descripcion = descripcion;
        _objeto = objeto;
    }


    public SelectableNode(String descripcion,
                          E objeto, Boolean seleccionUnica, Boolean seleccionado) {
        super(descripcion);
        _seleccionado = seleccionado;
        _seleccionUnica = seleccionUnica;
        _descripcion = descripcion;
        _objeto = objeto;
    }

    public SelectableNode(String descripcion,
                          E objeto, Boolean seleccionUnica, Boolean seleccionado, String tooltip) {
        super(descripcion);
        _seleccionado = seleccionado;
        _seleccionUnica = seleccionUnica;
        _descripcion = descripcion;
        _objeto = objeto;
        _tooltip = tooltip;
    }

    public Boolean getSeleccionado() {
        return _seleccionado;
    }

    public Boolean getSeleccionUnica() {
        return _seleccionUnica;
    }

    public void setSingleSelection(Boolean seleccionUnica) {
        _seleccionUnica = seleccionUnica;
    }

    public void setSelected(Boolean seleccionado) {
        _seleccionado = seleccionado;
    }

    public Boolean getContineneSeleccionado() {
        return _contineneSeleccionado;
    }

    public void setContainsSelected(Boolean seleccionado) {
        _contineneSeleccionado = seleccionado;
    }

    public void setHierarchySelection(boolean hierarchySelection){
        _hierarchySelection = hierarchySelection;
    }

    public void setAllSelected(Boolean seleccionado) {
        _seleccionado = seleccionado;
        _contineneSeleccionado = seleccionado;
        if (_hierarchySelection){
            if (!_seleccionUnica || !seleccionado){
                Enumeration<?> e = children();
                while (e.hasMoreElements()){
                    ((SelectableNode<?>)e.nextElement()).setAllSelected(seleccionado);
                }
            }
        }
    }

    public Boolean getVisible() {
        return _visible;
    }

    public void setVisible(Boolean visible) {
        this._visible = visible;
        if (_parent!=null){
            if (visible){
                if (!_parent.getVisibleChildren().contains(this)){
                    _parent.getVisibleChildren().add(this);
                }
            }else{
                _parent.getVisibleChildren().remove(this);
            }
        }
    }

    public String getDescripcion() {
        return _descripcion;
    }

    public void setDescription(String descripcion) {
        _descripcion = descripcion;
    }

    public String getToolTip() {
        return _tooltip;
    }

    public void setToolTip(String tooltip) {
        _tooltip = tooltip;
    }

    public E getObject() {
        return _objeto;
    }

    public void setObject(E obj) {
        _objeto=obj;
    }

    public Boolean isBold() {
        return _bold;
    }

    public void setBold(Boolean bold) {
        _bold = bold;
    }

    public Color getForeground() {
        return _foreground;
    }

    public void setForeground(Color color) {
        _foreground = color;
    }
    public void setItalics(Boolean cursiva) {
        _cursiva = cursiva;
    }

    public Boolean isCursiva() {
        return _cursiva;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public void cambioEstado(SelectableNode<?> nodoSeleccionable){
        if (_seleccionUnica && this.equals(nodoSeleccionable)){
            SelectableNode<E> nodoRaiz = this;
            while (nodoRaiz.getParent()!=null){
                nodoRaiz = (SelectableNode)nodoRaiz.getParent();
            }

            //Borramos todas las selecciones existentes
            nodoRaiz.setAllSelected(false);
            if (!this.equals(nodoRaiz)){
                if (_hierarchySelection){
                    this.setSelected(true);
                }
                this.setContainsSelected(true);
            }
        }
        if (this.children !=null && this.children.contains(nodoSeleccionable)){
            boolean seleccionado = !_seleccionUnica && getChildCount()>0;
            boolean contieneSeleccionado = false;
            Enumeration<?> e = getAllchildren();
            while (e.hasMoreElements()){
                SelectableNode<?> child = ((SelectableNode<?>)e.nextElement());
                if (!child.getSeleccionado()){
                    seleccionado = false;
                }
                if (child.getContineneSeleccionado() || child.getSeleccionado()){
                    contieneSeleccionado = true;
                }
            }
            nodoSeleccionable = this;
            if (_hierarchySelection){
                _seleccionado = seleccionado;
            }
            _contineneSeleccionado = contieneSeleccionado;
        }
        if (getParent() instanceof SelectableNode<?>){
            ((SelectableNode<E>)getParent()).cambioEstado(nodoSeleccionable);
        }
    }

    public void add(SelectableNode<?> newChild) {
        super.add(newChild);
        if (newChild.getSeleccionado()){
            if (_hierarchySelection && getChildCount()==1 && _seleccionUnica){
                _seleccionado = true;
            }
            _contineneSeleccionado = true;
        }else{
            if (_hierarchySelection){
                _seleccionado = false;
            }
            if (newChild.getContineneSeleccionado()){
                _contineneSeleccionado = true;
            }
        }
        newChild.setParentNode(this);
        if (newChild.getVisible()){
            getVisibleChildren().add(newChild);
        }
    }

    public void setParentNode(SelectableNode<?> parent){
        _parent = parent;
    }

    private Vector<SelectableNode<?>> getVisibleChildren(){
	/*
	Vector<Object> visibleChildren = new Vector<Object>();
	for (Object child : children) {
	    if (child instanceof SelectableNode<?>){
		if (((SelectableNode<?>)child).getVisible()){
		    visibleChildren.add(child);
		}
	    }
	}
	return visibleChildren;*/
        return _visibleChildren;
    }

    public Enumeration<?> children() {
        if (children == null) {
            return EMPTY_ENUMERATION;
        } else {
            return getVisibleChildren().elements();
        }
    }

    public Enumeration<?> getAllchildren() {
        if (children == null) {
            return EMPTY_ENUMERATION;
        } else {

            return children.elements();
        }
    }

    public boolean isLeaf(){
        return !(children!=null && children.size()>0);
    }

    public TreeNode getChildAt(int index) {
        if (children == null) {
            throw new ArrayIndexOutOfBoundsException("node has no children");
        }
        return (TreeNode)getVisibleChildren().elementAt(index);
    }

    public int getChildCount() {
        if (children == null) {
            return 0;
        } else {
            return getVisibleChildren().size();
        }
    }


    public int getAllChildrenCount() {
        if (children == null) {
            return 0;
        } else {
            return children.size();
        }
    }


    public int getIndex(TreeNode aChild) {
        if (aChild == null) {
            throw new IllegalArgumentException("argument is null");
        }
        if (!isNodeChild(aChild)) {
            return -1;
        }
        return getVisibleChildren().indexOf(aChild);	// linear search
    }

    public SelectableNode<E> clone(){
        return new SelectableNode<E>(_descripcion, _objeto, _seleccionUnica, _seleccionado);
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
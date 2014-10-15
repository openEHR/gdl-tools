package se.cambio.openehr.view.trees;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import java.awt.*;
import java.util.Enumeration;
import java.util.Vector;

public class SelectableNode<E> extends DefaultMutableTreeNode {

    public enum SelectionMode {SINGLE, MULTIPLE}
    public enum SelectionPropagationMode {HIERARCHICAL, NONE}

    private static final long serialVersionUID = 1;
    private boolean selected = false;
    private boolean childrenSelected = false;
    private boolean visible = true;

    private String name = null;
    private String description = null;
    private SelectionMode selectionMode = SelectionMode.SINGLE;
    private SelectionPropagationMode selectionPropagationMode = SelectionPropagationMode.HIERARCHICAL;
    private boolean bold = false;
    private boolean italic = false;
    private Color foregroundColor = null;
    private E object = null;
    private Icon icon = null;

    private SelectableNode<?> parent = null;
    private Vector<SelectableNode<?>> visibleChildren = new Vector<SelectableNode<?>>();

    protected SelectableNode(String name, String description, E object, SelectionMode selectionMode, SelectionPropagationMode selectionPropagationMode, boolean selected, boolean bold, boolean italic, Color foregroundColor, Icon icon) {
        super(name);
        this.name = name;
        this.description = description;
        this.object = object;
        this.selectionMode = selectionMode;
        this.selected = selected;
        this.selectionPropagationMode = selectionPropagationMode;
        this.bold = bold;
        this.italic = italic;
        this.foregroundColor = foregroundColor;
        this.icon = icon;
    }

    public Boolean isSelected() {
        return this.selected;
    }

    public SelectionMode getSelectionMode() {
        return selectionMode;
    }

    public void setSelected(Boolean selected) {
        this.selected = selected;
    }

    public boolean hasChildrenSelected() {
        return this.childrenSelected;
    }

    public void setChildrenSelected(boolean childrenSelected) {
        this.childrenSelected = childrenSelected;
    }

    public void setAllSelected(Boolean selected) {
        setAllSelected(selected, false);
    }

    public void setAllSelected(Boolean selected, boolean force) {
        this.selected = selected;
        this.childrenSelected = selected;
        if (isHierarchicalSelectionPropagationMode() || force){
            if (isMultipleSelectionMode() || !selected){
                Enumeration<?> e = children();
                while (e.hasMoreElements()){
                    ((SelectableNode<?>)e.nextElement()).setAllSelected(selected, force);
                }
            }
        }
    }

    public Boolean getVisible() {
        return this.visible;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
        if (this.parent!=null){
            if (visible){
                if (!this.parent.getVisibleChildren().contains(this)){
                    this.parent.getVisibleChildren().add(this);
                }
            }else{
                this.parent.getVisibleChildren().remove(this);
            }
        }
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String tooltip) {
        this.description = tooltip;
    }

    public SelectionPropagationMode getSelectionPropagationMode() {
        return selectionPropagationMode;
    }

    public Icon getIcon() {
        return icon;
    }

    public E getObject() {
        return object;
    }

    public void setObject(E obj) {
        object = obj;
    }

    public Boolean isBold() {
        return this.bold;
    }

    public Color getForegroundColor() {
        return this.foregroundColor;
    }

    public void setForegroundColor(Color color) {
        this.foregroundColor = color;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    public void setBold(boolean bold) {
        this.bold = bold;
    }

    public void setItalic(boolean italic) {
        this.italic = italic;
    }

    public void setIcon(Icon icon) {
        this.icon = icon;
    }

    public boolean isItalic() {
        return italic;
    }

    public void stateChange(SelectableNode<?> selectableNode){
        if (isSingleSelectionMode() && this.equals(selectableNode)){
            SelectableNode<E> rootNode = this;
            while (rootNode.getParent()!=null){
                rootNode = (SelectableNode)rootNode.getParent();
            }

            //Delete all previous selections
            rootNode.setAllSelected(false);
            if (!this.equals(rootNode)){
                if (isHierarchicalSelectionPropagationMode()){
                    this.setSelected(true);
                }
                this.setChildrenSelected(true);
            }
        }
        if (this.children !=null && this.children.contains(selectableNode)){
            boolean selected = isMultipleSelectionMode() && getChildCount()>0;
            boolean containsSelected = false;
            Enumeration<?> e = getAllchildren();
            while (e.hasMoreElements()){
                SelectableNode<?> child = ((SelectableNode<?>)e.nextElement());
                if (!child.isSelected()){
                    selected = false;
                }
                if (child.hasChildrenSelected() || child.isSelected()){
                    containsSelected = true;
                }
            }
            selectableNode = this;
            if (isHierarchicalSelectionPropagationMode()){
                this.selected = selected;
            }
            this.childrenSelected = containsSelected;
        }
        if (getParent() instanceof SelectableNode<?>){
            ((SelectableNode<E>)getParent()).stateChange(selectableNode);
        }
    }

    public void add(SelectableNode<?> newChild) {
        super.add(newChild);
        if (newChild.isSelected()){
            if (isHierarchicalSelectionPropagationMode() && getChildCount() == 1 && isSingleSelectionMode()){
                this.selected = true;
            }
            this.childrenSelected = true;
        }else{
            if (isHierarchicalSelectionPropagationMode()){
                this.selected = false;
            }
            if (newChild.hasChildrenSelected()){
                this.childrenSelected = true;
            }
        }
        newChild.setParentNode(this);
        if (newChild.getVisible()){
            getVisibleChildren().add(newChild);
        }
    }

    public boolean isSingleSelectionMode() {
        return SelectionMode.SINGLE.equals(selectionMode);
    }

    public boolean isMultipleSelectionMode() {
        return SelectionMode.MULTIPLE.equals(selectionMode);
    }

    private boolean isHierarchicalSelectionPropagationMode() {
        return SelectionPropagationMode.HIERARCHICAL.equals(selectionPropagationMode);
    }

    public void setParentNode(SelectableNode<?> parent){
        this.parent = parent;
    }

    private Vector<SelectableNode<?>> getVisibleChildren(){
        return this.visibleChildren;
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
        return !(children!=null && children.size() > 0);
    }

    public TreeNode getChildAt(int index) {
        if (children == null) {
            throw new ArrayIndexOutOfBoundsException("node has no children");
        }
        return getVisibleChildren().elementAt(index);
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
        return getVisibleChildren().indexOf(aChild);
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
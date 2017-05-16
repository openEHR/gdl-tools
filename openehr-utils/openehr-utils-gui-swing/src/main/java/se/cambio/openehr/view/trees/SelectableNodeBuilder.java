package se.cambio.openehr.view.trees;

import javax.swing.*;
import java.awt.*;

public class SelectableNodeBuilder<E> {
    private String name;
    private String description;
    private E object;
    private SelectableNode.SelectionMode selectionMode = SelectableNode.SelectionMode.SINGLE;
    private SelectableNode.SelectionPropagationMode selectionPropagationMode = SelectableNode.SelectionPropagationMode.HIERARCHICAL;
    private boolean selected;
    private boolean bold;
    private boolean italic;
    private Color foregroundColor;
    private Icon icon;

    public SelectableNodeBuilder<E> setName(String name) {
        this.name = name;
        return this;
    }

    public SelectableNodeBuilder<E> setDescription(String description) {
        this.description = description;
        return this;
    }

    public SelectableNodeBuilder<E> setObject(E object) {
        this.object = object;
        return this;
    }

    public SelectableNodeBuilder<E> setSelectionMode(SelectableNode.SelectionMode selectionMode) {
        this.selectionMode = selectionMode;
        return this;
    }

    public SelectableNodeBuilder<E> setSelectionPropagationMode(SelectableNode.SelectionPropagationMode selectionPropagationMode) {
        this.selectionPropagationMode = selectionPropagationMode;
        return this;
    }

    public SelectableNodeBuilder<E> setSelected(boolean selected) {
        this.selected = selected;
        return this;
    }

    public SelectableNodeBuilder<E> setBold(boolean bold) {
        this.bold = bold;
        return this;
    }

    public SelectableNodeBuilder<E> setItalic(boolean italic) {
        this.italic = italic;
        return this;
    }

    public SelectableNodeBuilder<E> setForegroundColor(Color foregroundColor) {
        this.foregroundColor = foregroundColor;
        return this;
    }

    public SelectableNodeBuilder<E> setIcon(Icon icon) {
        this.icon = icon;
        return this;
    }

    public SelectableNode<E> createSelectableNode() {
        return new SelectableNode<>(name, description, object, selectionMode, selectionPropagationMode, selected, bold, italic, foregroundColor, icon);
    }
}
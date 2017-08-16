package se.cambio.cds.gdl.graph.model;

import java.awt.*;

public class GraphEdge {

    public enum Style {
        NORMAL, DASHED
    }

    private Color color;
    private String label;
    private Style style;
    private GraphNode graphNodeA;
    private GraphNode graphNodeB;

    public GraphEdge(Color color, GraphNode graphNodeA, GraphNode graphNodeB, String label, Style style) {
        this.color = color;
        this.graphNodeA = graphNodeA;
        this.graphNodeB = graphNodeB;
        this.label = label;
        this.style = style;
    }

    public Color getColor() {
        return color;
    }

    public void setColor(Color color) {
        this.color = color;
    }

    public GraphNode getGraphNodeA() {
        return graphNodeA;
    }

    public void setGraphNodeA(GraphNode graphNodeA) {
        this.graphNodeA = graphNodeA;
    }

    public GraphNode getGraphNodeB() {
        return graphNodeB;
    }

    public void setGraphNodeB(GraphNode graphNodeB) {
        this.graphNodeB = graphNodeB;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public Style getStyle() {
        return style;
    }

    public void setStyle(Style style) {
        this.style = style;
    }
}
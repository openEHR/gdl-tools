package se.cambio.cds.gdl.graph.model;

import java.awt.*;

public class GraphEdgeBuilder {
    private Color color;
    private GraphNode graphNodeA;
    private GraphNode graphNodeB;
    private String label;
    private GraphEdge.Style style;

    public GraphEdgeBuilder setColor(Color color) {
        this.color = color;
        return this;
    }

    public GraphEdgeBuilder setGraphNodeA(GraphNode graphNodeA) {
        this.graphNodeA = graphNodeA;
        return this;
    }

    public GraphEdgeBuilder setGraphNodeB(GraphNode graphNodeB) {
        this.graphNodeB = graphNodeB;
        return this;
    }

    public GraphEdgeBuilder setLabel(String label) {
        this.label = label;
        return this;
    }

    public GraphEdgeBuilder setStyle(GraphEdge.Style style) {
        this.style = style;
        return this;
    }

    public GraphEdge createGraphEdge() {
        return new GraphEdge(color, graphNodeA, graphNodeB, label, style);
    }
}
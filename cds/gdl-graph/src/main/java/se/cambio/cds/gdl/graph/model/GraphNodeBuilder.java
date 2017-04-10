package se.cambio.cds.gdl.graph.model;

import java.awt.*;

public class GraphNodeBuilder {
    private Color fillColor;
    private String label;
    private GraphNode.Shape shape;
    private GraphNode.Alignment textAlignment;

    public GraphNodeBuilder setFillColor(Color fillColor) {
        this.fillColor = fillColor;
        return this;
    }

    public GraphNodeBuilder setLabel(String label) {
        this.label = label;
        return this;
    }

    public GraphNodeBuilder setShape(GraphNode.Shape shape) {
        this.shape = shape;
        return this;
    }

    public GraphNodeBuilder setTextAlignment(GraphNode.Alignment textAlignment) {
        this.textAlignment = textAlignment;
        return this;
    }

    public GraphNode createGraphNode() {
        return new GraphNode(fillColor, label, shape, textAlignment);
    }
}
package se.cambio.cds.gdl.graph.model;

import java.awt.*;

public class GraphNode {

    private Alignment textAlignment;
    private Color fillColor;
    private String label;
    private Shape shape;

    public enum Alignment {
        LEFT, RIGHT, CENTER
    }

    public enum Shape {
        RECTANGLE, ELLIPSE, HEXAGON
    }

    public GraphNode(Color fillColor, String label, Shape shape, Alignment textAlignment) {
        this.fillColor = fillColor;
        this.label = label;
        this.shape = shape;
        this.textAlignment = textAlignment;
    }

    public Color getFillColor() {
        return fillColor;
    }

    public void setFillColor(Color fillColor) {
        this.fillColor = fillColor;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public Shape getShape() {
        return shape;
    }

    public void setShape(Shape shape) {
        this.shape = shape;
    }

    public Alignment getTextAlignment() {
        return textAlignment;
    }

    public void setTextAlignment(Alignment textAlignment) {
        this.textAlignment = textAlignment;
    }
}

package se.cambio.cds.gdl.graph.controller.renderer;

import se.cambio.cds.gdl.graph.controller.NodeExploder;
import se.cambio.cds.gdl.graph.model.GraphEdge;
import se.cambio.cds.gdl.graph.model.GraphNode;

import javax.swing.*;
import java.awt.image.BufferedImage;

public interface GraphRenderer {
    void insertGraphNode(GraphNode graphNode) throws GraphRenderingException;

    void insertGraphEdge(GraphEdge graphEdge) throws GraphRenderingException;

    JComponent generateGraphComponent();

    BufferedImage generateBufferedImage();

    void setNodeExploder(NodeExploder nodeExploder);
}

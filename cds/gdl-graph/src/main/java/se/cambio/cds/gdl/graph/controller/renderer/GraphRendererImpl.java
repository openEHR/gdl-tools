package se.cambio.cds.gdl.graph.controller.renderer;

import com.mxgraph.layout.hierarchical.mxHierarchicalLayout;
import com.mxgraph.model.mxGraphModel;
import com.mxgraph.swing.handler.mxRubberband;
import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.util.mxCellRenderer;
import com.mxgraph.util.mxConstants;
import com.mxgraph.util.mxRectangle;
import com.mxgraph.util.mxUtils;
import com.mxgraph.view.mxGraph;
import se.cambio.cds.gdl.graph.controller.NodeExploder;
import se.cambio.cds.gdl.graph.model.GraphEdge;
import se.cambio.cds.gdl.graph.model.GraphNode;
import se.cambio.cds.gdl.graph.mx.CustomGraphComponent;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

public class GraphRendererImpl implements GraphRenderer {

    private mxGraph graph;
    private Map<GraphNode, Object> graphNodeObjectMap;
    private NodeExploder nodeExploder;

    GraphRendererImpl() {
    }


    private mxGraph getGraph() {
        if (graph == null) {
            mxGraphModel model = new mxGraphModel();
            graph = new mxGraph(model);
            graph.setCellsDeletable(true);
            graph.getSelectionModel().setSingleSelection(false);
            graph.setHtmlLabels(true);
            graph.setAllowDanglingEdges(false);
        }
        return graph;
    }

    private Map<GraphNode, Object> getGraphNodeObjectMap() {
        if (graphNodeObjectMap == null) {
            graphNodeObjectMap = new HashMap<>();
        }
        return graphNodeObjectMap;
    }

    @Override
    public void insertGraphNode(GraphNode graphNode) throws GraphRenderingException {
        StringBuilder styleSB = new StringBuilder();
        if (graphNode.getShape() != null) {
            String mxShape = getMxShape(graphNode.getShape());
            styleSB.append(mxConstants.STYLE_SHAPE).append("=").append(mxShape).append(";");
        }
        GraphNode.Alignment textAlignment = graphNode.getTextAlignment();
        if (textAlignment != null) {
            styleSB.append(mxConstants.STYLE_ALIGN).append("=").append(getMxAlignment(textAlignment)).append(";");
        }
        Color color = graphNode.getFillColor();
        if (color != null) {
            String hexColor = "#" + Integer.toHexString(color.getRGB()).substring(2);
            styleSB.append(mxConstants.STYLE_FILLCOLOR).append("=").append(hexColor).append(";");
        }
        String label = graphNode.getLabel();
        mxRectangle rectangle = mxUtils.getSizeForHtml(label, new HashMap<>(), 1, 0);
        double width = rectangle.getWidth() + 35;
        double height = rectangle.getHeight() + 25;
        Object node = getGraph().insertVertex(getGraph().getDefaultParent(), null, label, 0, 0, width, height);
        getGraph().getModel().setStyle(node, styleSB.toString());
        getGraphNodeObjectMap().put(graphNode, node);
    }

    private String getMxShape(GraphNode.Shape shape) throws GraphRenderingException {
        if (GraphNode.Shape.RECTANGLE.equals(shape)) {
            return mxConstants.SHAPE_RECTANGLE;
        } else if (GraphNode.Shape.ELLIPSE.equals(shape)) {
            return mxConstants.SHAPE_ELLIPSE;
        } else if (GraphNode.Shape.HEXAGON.equals(shape)) {
            return mxConstants.SHAPE_HEXAGON;
        } else {
            throw new GraphRenderingException("Unknown shape " + shape + "'.");
        }
    }

    private String getMxAlignment(GraphNode.Alignment alignment) throws GraphRenderingException {
        if (GraphNode.Alignment.CENTER.equals(alignment)) {
            return mxConstants.ALIGN_CENTER;
        } else if (GraphNode.Alignment.LEFT.equals(alignment)) {
            return mxConstants.ALIGN_LEFT;
        } else if (GraphNode.Alignment.RIGHT.equals(alignment)) {
            return mxConstants.ALIGN_RIGHT;
        } else {
            throw new GraphRenderingException("Unknown alignment '" + alignment + "'");
        }
    }

    @Override
    public void insertGraphEdge(GraphEdge graphEdge) throws GraphRenderingException {
        Object nodeA = getGraphNodeObjectMap().get(graphEdge.getGraphNodeA());
        if (nodeA == null) {
            throw new GraphRenderingException("First node from edge has not been inserted yet.");
        }
        Object nodeB = getGraphNodeObjectMap().get(graphEdge.getGraphNodeB());
        if (nodeB == null) {
            throw new GraphRenderingException("Second node from edge has not been inserted yet.");
        }
        StringBuilder styleSB = new StringBuilder();
        if (graphEdge.getColor() != null) {
            styleSB.append(mxConstants.STYLE_STROKECOLOR).append("=").append(graphEdge.getColor()).append(";");
        }
        if (GraphEdge.Style.DASHED.equals(graphEdge.getStyle())) {
            styleSB.append(mxConstants.STYLE_DASHED).append("=true;");
        }
        styleSB.append(mxConstants.STYLE_ROUNDED).append("=true;");
        styleSB.append(mxConstants.STYLE_EDGE).append("=").append(mxConstants.EDGESTYLE_ENTITY_RELATION);
        String label = graphEdge.getLabel();
        Object edge = getGraph().insertEdge(getGraph().getDefaultParent(), null, label, nodeA, nodeB);
        getGraph().getModel().setStyle(edge, styleSB.toString());
    }

    @Override
    public JComponent generateGraphComponent() {
        mxGraphComponent graphComponent = new CustomGraphComponent(getGraph(), nodeExploder);
        new mxRubberband(graphComponent);
        layout(graphComponent);
        return graphComponent;
    }

    public static void layout(mxGraphComponent graphComponent) {
        final mxGraph graph = graphComponent.getGraph();
        final mxHierarchicalLayout layout = new mxHierarchicalLayout(graph);
        graph.getModel().beginUpdate();
        try {
            layout.execute(graph.getDefaultParent());
        } finally {
            graph.getModel().endUpdate();
        }
    }

    @Override
    public BufferedImage generateBufferedImage() {
        mxGraph graph = getGraph();
        return mxCellRenderer.createBufferedImage(graph, null, 1, Color.WHITE, true, null, createGraphComponent(graph).getCanvas());
    }

    private static mxGraphComponent createGraphComponent(mxGraph graph) {
        mxGraphComponent graphComponent = new mxGraphComponent(graph);
        new mxRubberband(graphComponent);
        layout(graphComponent);
        return graphComponent;
    }

    @Override
    public void setNodeExploder(NodeExploder nodeExploder) {
        this.nodeExploder = nodeExploder;
    }
}
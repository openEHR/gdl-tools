package se.cambio.cds.gdl.graph;

import com.mxgraph.layout.hierarchical.mxHierarchicalLayout;
import com.mxgraph.model.mxGraphModel;
import com.mxgraph.swing.handler.mxRubberband;
import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.util.mxConstants;
import com.mxgraph.util.mxRectangle;
import com.mxgraph.util.mxUtils;
import com.mxgraph.view.mxGraph;
import org.apache.batik.svggen.SVGGraphics2DIOException;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.TermDefinition;

import javax.swing.*;
import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.geom.AffineTransform;
import java.io.OutputStreamWriter;
import java.util.HashMap;

/**
 * User: Iago.Corbal
 * Date: 2014-07-02
 * Time: 13:30
 */
public class GDLGraphUtil {

    private static Font DEFAULT_FONT = new JLabel().getFont();
    private static FontRenderContext FONT_RENDER_CONTEXT = new FontRenderContext(new AffineTransform(),true,true);

    static{
        Toolkit.getDefaultToolkit().getSystemEventQueue().push(new GDLGraphContextMenu());
    }

    public static void exportGraph(OutputStreamWriter writer, mxGraph graph) throws SVGGraphics2DIOException {
        /*
        Object[] cells = graph.getChildCells(null);
        Rectangle2D bounds = graph.toScreen(graph.getCellBounds(cells));
        DOMImplementation domImpl = GenericDOMImplementation.getDOMImplementation();
        Document document = domImpl.createDocument(null, "svg", null);
        SVGGraphics2D svgGraphics = new SVGGraphics2D(document);
        svgGraphics.setSVGCanvasSize(new Dimension((int)Math.round(bounds.getWidth()+100), (int)Math.round(bounds.getHeight())));
        RepaintManager repaintManager = RepaintManager.currentManager(graph);
        repaintManager.setDoubleBufferingEnabled(false);
        BasicGraphUI gui = (BasicGraphUI) graph.getUI();
        gui.drawGraph(svgGraphics, bounds);
        svgGraphics.stream(writer, false);
        */
    }

    public static mxGraph createGraph(){
        mxGraphModel model = new mxGraphModel();
        mxGraph graph = new mxGraph(model);
        graph.setCellsDeletable(true);
        graph.getSelectionModel().setSingleSelection(false);
        graph.setPortsEnabled(true);
        graph.setHtmlLabels(true);
        graph.setAllowDanglingEdges(false);
        return graph;
    }

    public static mxGraphComponent createGraphComponent(mxGraph graph){
        mxGraphComponent graphComponent = new mxGraphComponent(graph);
        new mxRubberband(graphComponent);
        layout(graphComponent);
        return graphComponent;
    }

    public static Object insertNode(mxGraph mxGraph, String label, String color){
        mxRectangle rectangle = mxUtils.getSizeForHtml(label, new HashMap<String, Object>(), 1, 0);
        int textWidth = (int)(DEFAULT_FONT.getStringBounds(label, FONT_RENDER_CONTEXT).getWidth());
        return insertNode(mxGraph, label, 0, 0, rectangle.getWidth()+25, rectangle.getHeight()+15, color);
    }

    public static Object insertNode(mxGraph mxGraph, String label, double x, double y, double w, double h, String color) {
        // Create node with the given label
        Object node = mxGraph.insertVertex(mxGraph.getDefaultParent(), null, label, x, y, w, h);
        StringBuffer styleSB = new StringBuffer();
        styleSB.append(mxConstants.STYLE_SHAPE+"="+mxConstants.SHAPE_RECTANGLE+";");
        styleSB.append(mxConstants.STYLE_ALIGN+"="+mxConstants.ALIGN_CENTER+";");
        if (color!=null){
            styleSB.append(mxConstants.STYLE_FILLCOLOR + "=" + color + ";");
        }
        mxGraph.getModel().setStyle(node, styleSB.toString());
        return node;
    }

    public static void insertDirectionalEdge(mxGraph mxGraph, Object n1, Object n2){
        insertDirectionalEdge(mxGraph, null, n1, n2);
    }

    public static void insertDirectionalEdge(mxGraph mxGraph, String label, Object n1, Object n2){
        Object edge = mxGraph.insertEdge(mxGraph.getDefaultParent(), null, label, n1, n2);
        StringBuffer styleSB = new StringBuffer();
        //styleSB.append(mxConstants.STYLE_ALIGN+"="+mxConstants.ALIGN_CENTER+";");
        mxGraph.getModel().setStyle(edge, styleSB.toString());
    }

    public static void insertDirectionalEdgeDashed(mxGraph mxGraph, Object n1, Object n2){
        Object edge = mxGraph.insertEdge(mxGraph.getDefaultParent(), null, null, n1, n2);
        StringBuffer styleSB = new StringBuffer();
        styleSB.append(mxConstants.STYLE_DASHED +"=true;");
        mxGraph.getModel().setStyle(edge, styleSB.toString());
    }

    public static Object createElementNode(mxGraph graph, ElementBinding elementBinding, TermDefinition termDefinition){
        String label = termDefinition.getTerms().get(elementBinding.getId()).getText();
        return GDLGraphUtil.insertNode(graph, label, "#31b2e6");
    }

    public static Object createSpecialNode(mxGraph graph, String label, TermDefinition termDefinition){
        return GDLGraphUtil.insertNode(graph, label, "#cccccc");
    }

    public static Object createRuleNode(mxGraph graph, Rule rule, TermDefinition termDefinition){
        String label = termDefinition.getTerms().get(rule.getId()).getText();
        return GDLGraphUtil.insertNode(graph, label, "#a3e308");
    }

    public static Object createEndNode(mxGraph graph){
        return GDLGraphUtil.insertNode(graph, "",0,0,40,4, "#000000");
    }

    public static void layout(mxGraphComponent graphComponent){
        final mxGraph graph = graphComponent.getGraph();
        final mxHierarchicalLayout layout = new mxHierarchicalLayout(graph);
        graph.getModel().beginUpdate();
        try{
            layout.execute(graph.getDefaultParent());
        }finally{
            graph.getModel().endUpdate();
        }
    }
}

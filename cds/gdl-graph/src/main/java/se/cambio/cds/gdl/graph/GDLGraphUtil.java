package se.cambio.cds.gdl.graph;

import org.apache.batik.dom.GenericDOMImplementation;
import org.apache.batik.svggen.SVGGraphics2D;
import org.apache.batik.svggen.SVGGraphics2DIOException;
import org.jgraph.JGraph;
import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;
import org.jgraph.plaf.basic.BasicGraphUI;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;

import javax.swing.*;
import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.io.OutputStreamWriter;

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

    public static void exportGraph(OutputStreamWriter writer, JGraph graph) throws SVGGraphics2DIOException {
        Object[] cells = graph.getRoots();
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
    }

    public static DefaultGraphCell createNode(String label, Color color){
        int textWidth = (int)(DEFAULT_FONT.getStringBounds(label, FONT_RENDER_CONTEXT).getWidth());
        DefaultGraphCell node = createNode(label, 0, 0, textWidth + 20, 35, null, false);
        setBasicAttributes(node.getAttributes(), color);
        return node;
    }

    public static DefaultGraphCell createNode(String name, double x, double y, double w, double h, Color bg, boolean raised) {
        // Create vertex with the given name
        DefaultGraphCell cell = new DefaultGraphCell(name);
        // Set bounds
        GraphConstants.setBounds(cell.getAttributes(), new Rectangle2D.Double(x, y, w, h));
        // Set fill color
        if (bg != null) {
            GraphConstants.setGradientColor(cell.getAttributes(), bg);
            GraphConstants.setOpaque(cell.getAttributes(), true);
        }
        // Set raised border
        if (raised){
            GraphConstants.setBorder(cell.getAttributes(), BorderFactory
                    .createRaisedBevelBorder());
        }else{
            // Set black border
            GraphConstants.setBorderColor(cell.getAttributes(), Color.black);
        }
        // Add a Floating Port
        cell.addPort();
        return cell;
    }

    public static DefaultEdge createDirectionalEdge(DefaultGraphCell n1, DefaultGraphCell n2){
        DefaultEdge edge = new DefaultEdge();
        // Fetch the ports from the new vertices, and connect them with the edge
        edge.setSource(n1.getChildAt(0));
        edge.setTarget(n2.getChildAt(0));
        // Set Arrow Style for edge
        int arrow = GraphConstants.ARROW_CLASSIC;
        GraphConstants.setLineEnd(edge.getAttributes(), arrow);
        GraphConstants.setEndFill(edge.getAttributes(), true);
        return edge;
    }

    private static void setBasicAttributes(AttributeMap attMap, Color color){
        GraphConstants.setFont(attMap, DEFAULT_FONT);
        GraphConstants.setBorder(attMap, BorderFactory.createRaisedBevelBorder());
        GraphConstants.setBackground(attMap, color.darker().darker());
        GraphConstants
                .setGradientColor(attMap, color.brighter().brighter());
        GraphConstants.setForeground(attMap, Color.white);
        GraphConstants.setOpaque(attMap, true);
    }
}

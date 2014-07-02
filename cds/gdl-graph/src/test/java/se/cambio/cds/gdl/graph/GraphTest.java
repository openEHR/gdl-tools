package se.cambio.cds.gdl.graph;


import com.jgraph.layout.JGraphFacade;
import com.jgraph.layout.hierarchical.JGraphHierarchicalLayout;
import org.jgraph.JGraph;
import org.jgraph.graph.*;

import javax.swing.*;
import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.geom.AffineTransform;
import java.util.*;

/**
 * User: Iago.Corbal
 * Date: 2014-07-01
 * Time: 11:09
 */
public class GraphTest {
    public static void main(String[] args) {
        // Switch off D3D because of Sun XOR painting bug
        // See http://www.jgraph.com/forum/viewtopic.php?t=4066
        //System.setProperty("sun.java2d.d3d", "false");

        // Construct Model and Graph
        GraphModel model = new DefaultGraphModel();
        JGraph graph = new JGraph(model);
        // Control-drag should clone selection
        //graph.setCloneable(true);
        // Enable edit without final RETURN keystroke
        graph.setInvokesStopCellEditing(true);
        // When over a cell, jump to its default port
        graph.setJumpToDefaultPort(true);
        Collection<DefaultGraphCell> cells = new ArrayList<DefaultGraphCell>();
        Map<Integer, DefaultGraphCell> nodeMap = new HashMap<Integer, DefaultGraphCell>();
        AffineTransform affinetransform = new AffineTransform();
        FontRenderContext frc = new FontRenderContext(affinetransform,true,true);
        Font font = new JLabel().getFont();

        // Create Vertexs
        for(int i=0;i<20;i++){
            String label = "node sdadfadfasdfs" + i;
            int textWidth = (int)(font.getStringBounds(label, frc).getWidth());
            DefaultGraphCell node = GDLGraphUtil.createNode(label, 10, 20, textWidth + 20, 30, null, false);
            Color c = Color.GREEN;
            AttributeMap map = node.getAttributes();
            GraphConstants.setFont(map, font);
            GraphConstants.setBorder(map, BorderFactory.createRaisedBevelBorder());
            GraphConstants.setBackground(map, c.darker().darker());
            GraphConstants
                    .setGradientColor(map, c.brighter().brighter().brighter());
            GraphConstants.setForeground(map, Color.white);
            GraphConstants.setOpaque(map, true);
            cells.add(node);
            nodeMap.put(i, node);
        }

        // Create Edges
        for(int i=0;i<65;i++){
            DefaultEdge edge = new DefaultEdge();
            int n1 = new Random().nextInt(20);
            int n2 = new Random().nextInt(20);
            // Fetch the ports from the new vertices, and connect them with the edge
            edge.setSource(nodeMap.get(n1).getChildAt(0));
            edge.setTarget(nodeMap.get(n2).getChildAt(0));
            cells.add(edge);
            // Set Arrow Style for edge
            int arrow = GraphConstants.ARROW_CLASSIC;
            GraphConstants.setLineEnd(edge.getAttributes(), arrow);
            GraphConstants.setEndFill(edge.getAttributes(), true);
        }

        // Insert the cells via the cache, so they get selected
        graph.getGraphLayoutCache().insert(cells.toArray());

        final JGraphHierarchicalLayout hir = new JGraphHierarchicalLayout();
        //final JGraphFastOrganicLayout forl = new JGraphFastOrganicLayout();
        final JGraphFacade graphFacade = new JGraphFacade(graph);
        hir.run(graphFacade);
        final Map nestedMap = graphFacade.createNestedMap(true, true);
        graph.getGraphLayoutCache().edit(nestedMap);
        /*
        try {
            GDLDependencyGraph.exportGraph(new FileWriter(new File("test.svg")), graph);
        } catch (IOException e) {
            e.printStackTrace();
        } */
        // Show in Frame
        JFrame frame = new JFrame();
        frame.getContentPane().add(new JScrollPane(graph));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
}

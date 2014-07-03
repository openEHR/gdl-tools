package se.cambio.cds.gdl.graph;


import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.view.mxGraph;

import javax.swing.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * User: Iago.Corbal
 * Date: 2014-07-01
 * Time: 11:09
 */
public class GraphTest {
    public static void main(String[] args) {
        // Switch off D3D because of Sun XOR painting bug
        // See http://www.mxGraph.com/forum/viewtopic.php?t=4066
        //System.setProperty("sun.java2d.d3d", "false");

        // Construct Model and Graph
        mxGraph graph = new mxGraph();
        // Control-drag should clone selection
        //graph.setCloneable(true);
        // Enable edit without final RETURN keystroke
        //graph.setInvokesStopCellEditing(true);
        // When over a cell, jump to its default port
        //graph.setJumpToDefaultPort(true);
        graph.getModel().beginUpdate();
        Map<Integer, Object> nodeMap = new HashMap<Integer, Object>();
        // Create Vertexs
        for(int i=0;i<20;i++){
            String label = "node sdadfadfasdfs" + i;
            Object node = GDLGraphUtil.insertNode(graph, label, "#11FFFF");
            nodeMap.put(i, node);
        }

        // Create Edges
        for(int i=0;i<65;i++){
            int n1 = new Random().nextInt(20);
            int n2 = new Random().nextInt(20);
            GDLGraphUtil.insertDirectionalEdge(graph, nodeMap.get(n1), nodeMap.get(n2));
        }
        graph.getModel().endUpdate();
        mxGraphComponent graphComponent = new mxGraphComponent(graph);
        GDLGraphUtil.layout(graphComponent);
        /*
        try {
            GDLDependencyGraph.exportGraph(new FileWriter(new File("test.svg")), graph);
        } catch (IOException e) {
            e.printStackTrace();
        } */
        // Show in Frame
        JFrame frame = new JFrame();
        frame.getContentPane().add(new JScrollPane(graphComponent));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
}

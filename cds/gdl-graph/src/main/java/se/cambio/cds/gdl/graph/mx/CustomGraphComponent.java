package se.cambio.cds.gdl.graph.mx;

import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.view.mxGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.graph.controller.NodeExploder;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class CustomGraphComponent extends mxGraphComponent {

    private final NodeExploder nodeExploder;
    private static Logger logger = LoggerFactory.getLogger(CustomGraphComponent.class);


    public CustomGraphComponent(mxGraph graph, NodeExploder nodeExploder) {
        super(graph);
        if (nodeExploder == null) {
            logger.warn("Node exploder not set.");
        }
        this.nodeExploder = nodeExploder;
    }

    @Override
    protected void installDoubleClickHandler() {
        graphControl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseReleased(MouseEvent ev) {
                if (nodeExploder != null && !ev.isConsumed() && isEditEvent(ev)) {
                    Object cell = getCellAt(ev.getX(), ev.getY(), false);
                    if (cell != null && getGraph().isCellEditable(cell)) {
                        String nodeLabel = (String) graph.getModel().getValue(cell);
                        if (nodeLabel != null) {
                            nodeExploder.explode(nodeLabel);
                        }
                    }
                }
            }
        });
    }
}

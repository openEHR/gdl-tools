package se.cambio.cds.gdl.graph.controller.renderer;

public class GraphRendererFactory {

    public static GraphRenderer getGraphRenderer() {
        return new GraphRendererImpl();
    }
}

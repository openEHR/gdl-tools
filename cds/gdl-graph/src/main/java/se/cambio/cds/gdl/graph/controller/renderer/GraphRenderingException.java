package se.cambio.cds.gdl.graph.controller.renderer;

import se.cambio.openehr.util.exceptions.InternalErrorException;

public class GraphRenderingException extends InternalErrorException {

    public GraphRenderingException(String message) {
        super(new Exception("Graph rendering exception: " + message));
    }
}

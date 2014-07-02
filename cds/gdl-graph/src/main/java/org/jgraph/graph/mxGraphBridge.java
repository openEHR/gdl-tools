/*
 * $Id: mxGraphBridge.java,v 1.4 2010-12-01 17:59:41 david Exp $
 * Copyright (c) 2010, David Benson, Gaudenz Alder
 */
package org.jgraph.graph;

import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jgraph.JGraph;

import com.jgraph.layout.JGraphFacade;
import com.mxgraph.model.mxGeometry;
import com.mxgraph.model.mxGraphModel;
import com.mxgraph.model.mxIGraphModel;
import com.mxgraph.util.mxPoint;
import com.mxgraph.view.mxGraph;
import com.mxgraph.view.mxGraphView;

/**
 * Utility class to convert JGraph 5 data structures to JGraph 6 (X). Currently
 * it is only designed to bridge the JGraph 5 layout facade
 * 
 */
public class mxGraphBridge {

    /**
     * The mxGraph view
     */
    protected mxGraphView mxView;

    /**
     * The mxGraph
     */
    protected mxGraph graph;

    /**
     * The JGraph UI
     */
    protected JGraph jgraph;

    /**
     * The facade above the JGraph
     */
    protected JGraphFacade facade;

    /**
     * A map to associate mxGraph edges with JGraph edges
     */
    protected Map<Object, Object> edges;

    /**
     * A map to associate mxGraph vertices with JGraph vertices
     */
    protected Map<Object, Object> vertices;

    public mxGraphBridge() {
    }

    public mxGraphBridge(JGraphFacade inputFacade) {
	this.facade = inputFacade;
    }

    /**
     * Populate a fresh mxGraph data model from a JGraph data model
     */
    public void syncMx(JGraph inputGraph) {
	if (inputGraph != null) {
	    jgraph = inputGraph;
	} else {
	    return;
	}

	GraphModel jModel = jgraph.getModel();

	mxIGraphModel mxModel = null;

	if (graph == null) {
	    mxModel = new mxGraphModel();
	    this.graph = new mxGraph(mxModel);
	}

	mxModel = graph.getModel();
	mxModel.beginUpdate();
	try {
	    int rootCount = jModel.getRootCount();

	    // Insert everything in mx in the same order and
	    // hierarchy as jgraph
	    for (int i = 0; i < rootCount; i++) {
		Object cell = jModel.getRootAt(i);

		if (cell != null) {
		    insertCell(cell, graph.getDefaultParent());
		}
	    }

	    // Edges are inserted without source and targets,
	    // since the vertices may not exist at insertion
	    // time
	    if (edges != null && vertices != null) {
		Set<Object> keys = edges.keySet();

		for (Object edge : keys) {
		    Object source = jModel.getSource(edge);
		    Object target = jModel.getTarget(edge);

		    Object mxEdge = edges.get(edge);

		    if (facade != null) {
			source = facade.getSource(edge);
			target = facade.getTarget(edge);
		    }

		    if (source != null) {
			source = vertices.get(source);
			graph.connectCell(mxEdge, source, true);
		    }

		    if (target != null) {
			target = vertices.get(target);
			graph.connectCell(mxEdge, target, false);
		    }

		}
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	} finally {
	    mxModel.endUpdate();
	}
    }

    /**
     * Populate the stored JGraph 5 facade from the mxGraph data model.
     */
    public void syncJGraphFacade() {
	if (facade == null || graph == null) {
	    return;
	}

	mxIGraphModel model = graph.getModel();

	if (vertices != null) {
	    Set<Object> vertexKeys = vertices.keySet();
	    for (Object vertex : vertexKeys) {
		Object mxVertex = vertices.get(vertex);
		mxGeometry geo = model.getGeometry(mxVertex);
		facade.setLocation(vertex, geo.getX(), geo.getY());
	    }
	}

	if (edges != null) {
	    Set<Object> edgeKeys = edges.keySet();
	    for (Object edge : edgeKeys) {
		Object mxEdge = edges.get(edge);
		List<mxPoint> points = model.getGeometry(mxEdge).getPoints();

		if (points != null) {
		    facade.setIntermediatePoints(edge, points);
		}
	    }
	}
    }

    /**
     * Converts a JGraph cell into an mxGraph with the specified mxGraph parent
     * 
     * @param cell
     *            the JGraph cell
     * @param parent
     *            the mxGraph parent
     */
    protected void insertCell(Object cell, Object parent) {
	Object mxCell = null;
	GraphModel jModel = jgraph.getModel();
	Object value = jModel.getValue(cell);

	if (jModel.isEdge(cell)) {
	    mxCell = graph.insertEdge(parent, null, value, null, null);

	    if (edges == null) {
		edges = new Hashtable<Object, Object>();
	    }

	    edges.put(cell, mxCell);
	} else if (jModel.isPort(cell)) {
	    // TODO
	} else {
	    // A vertex
	    Rectangle2D bounds = null;

	    if (facade != null) {
		bounds = facade.getBounds(cell);
	    } else {
		GraphLayoutCache cache = jgraph.getGraphLayoutCache();
		CellView cellView = cache.getMapping(cell, false);
		bounds = cellView.getBounds();
	    }

	    if (bounds != null) {
		mxCell = graph.insertVertex(parent, null, value, bounds.getX(),
			bounds.getY(), bounds.getWidth(), bounds.getHeight());
	    } else {
		mxCell = graph.insertVertex(parent, null, value, 100, 100, 80,
			30);
	    }

	    if (vertices == null) {
		vertices = new Hashtable<Object, Object>();
	    }

	    vertices.put(cell, mxCell);
	}

	// Process all the children of this cell
	int childCount = jModel.getChildCount(cell);

	if (mxCell != null) {
	    for (int i = 0; i < childCount; i++) {
		Object childCell = jModel.getChild(cell, i);

		if (cell != null) {
		    insertCell(childCell, mxCell);
		}
	    }
	}
    }

    public Object getVertexMapping(Object jCell) {
	if (vertices != null) {
	    return vertices.get(jCell);
	}
	
	return null;
    }

    public mxGraph getGraph() {
	return graph;
    }
}
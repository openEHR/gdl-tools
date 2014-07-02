/*
 * Copyright (c) 2005-2010, David Benson, Gaudenz Alder
 *
 * All rights reserved.
 *
 * This file is licensed under the JGraph software license, a copy of which
 * will have been provided to you in the file LICENSE at the root of your
 * installation directory. If you are unable to locate this file please
 * contact JGraph sales for another copy.
 */
package com.jgraph.layout.hierarchical;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingConstants;

import org.jgraph.graph.mxGraphBridge;

import com.jgraph.layout.JGraphFacade;
import com.jgraph.layout.JGraphLayout;
import com.jgraph.layout.JGraphLayoutProgress;
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout;
import com.mxgraph.view.mxGraph;

/**
 * The top level compound layout of the hierarchical layout. The individual
 * elements of the layout are called in sequence. This layout does not inherit
 * from <code>JGraphCompoundLayout</code> as a complete model of the hierarchy
 * needs to be passed into each step and
 */
public class JGraphHierarchicalLayout implements JGraphLayout,
	JGraphLayout.Stoppable {

    private static final double INITIAL_X_POSITION = 100.0;

    /**
     * The spacing buffer added between cells on the same layer
     */
    protected double intraCellSpacing = 30.0;

    /**
     * The spacing buffer added between cell on adjacent layers
     */
    protected double interRankCellSpacing = 50.0;

    /**
     * The spacing buffer between unconnected hierarchies
     */
    protected double interHierarchySpacing = 60.0;

    /**
     * The distance between each parallel edge on each ranks for long edges
     */
    protected double parallelEdgeSpacing = 10.0;

    /**
     * The position of the root node(s) relative to the laid out graph in
     */
    protected int orientation = SwingConstants.NORTH;

    /**
     * Whether or not to perform local optimisations and iterate multiple times
     * through the algorithm
     */
    protected boolean fineTuning = true;

    /**
     * Whether or not to pull together sections of layout into empty space
     */
    protected boolean compactLayout = false;

    /**
     * Whether or not cells are ordered according to the order in the graph
     * model. Defaults to false since sorting usually produces quadratic
     * performance. Note that since MxGraph returns edges in a deterministic
     * order, it might be that this layout is always deterministic using that
     * JGraph regardless of this flag setting (i.e. leave it false in that case)
     */
    protected boolean deterministic = false;

    /**
     * Whether or not to fix the position of the root cells. Keep in mind to
     * turn off features such as move to origin when fixing the roots, move to
     * origin usually overrides this flag (in JGraph it does).
     */
    protected boolean fixRoots = false;

    /**
     * Whether or not the initial scan of the graph to determine the layer
     * assigned to each vertex starts from the sinks or source (the sinks being
     * vertices with the fewest, preferable zero, outgoing edges and sources
     * same with incoming edges). Starting from either direction can tight the
     * layout up and also produce better results for certain types of graphs. If
     * the result for the default is not good enough try a few sample layouts
     * with the value false to see if they improve
     */
    protected boolean layoutFromSinks = true;

    /**
     * The layout progress bar
     */
    protected JGraphLayoutProgress progress = new JGraphLayoutProgress();

    /** The logger for this class */
    private static Logger logger = Logger
	    .getLogger("JGraphHierarchicalLayout");

    /**
     * The default constructor
     * 
     */
    public JGraphHierarchicalLayout() {
	this(true);
    }

    /**
     * Creates a hierarchical layout, constructing the components of the layout
     * stages
     * 
     * @param deterministic
     *            whether or not this layout should be deterministic
     */
    public JGraphHierarchicalLayout(boolean deterministic) {
	this.deterministic = deterministic;
    }

    /**
     * The API method used to exercise the layout upon the facade description
     * and produce a separate description of the vertex position and edge
     * routing changes made. It runs each stage of the layout that has been
     * created.
     * 
     * @param facade
     *            the facade object that describes and filters the graph to be
     *            acted upon
     */
    public void run(JGraphFacade facade) {
	mxGraphBridge mxBridge = new mxGraphBridge(facade);
	facade.syncMxBridge(mxBridge);
	
	mxGraph graph = mxBridge.getGraph();
	mxHierarchicalLayout layout = new mxHierarchicalLayout(graph);
	
	// Install settings from this layout
	layout.setDeterministic(deterministic);
	layout.setFineTuning(fineTuning);
	layout.setFixRoots(fixRoots);
	layout.setInterHierarchySpacing(interHierarchySpacing);
	layout.setInterRankCellSpacing(interRankCellSpacing);
	layout.setLayoutFromSinks(layoutFromSinks);
	layout.setOrientation(orientation);
	layout.setParallelEdgeSpacing(parallelEdgeSpacing);

	List<Object> mxRoots = new ArrayList<Object>();
	
	// If the roots of this layout are set, pass them over
	if (facade.getRoots() != null || facade.getRoots().size() > 0) {
	    List roots = facade.getRoots();
	    for (Object vertex : roots) {
		mxRoots.add(mxBridge.getVertexMapping(vertex));
	    }
	}

	layout.execute(graph.getDefaultParent(), mxRoots);
	
	mxBridge.syncJGraphFacade();
    }

    /**
     * Returns <code>Hierarchical</code>, the name of this algorithm.
     */
    public String toString() {
	return "Hierarchical";
    }

    /**
     * @return Returns the progress.
     */
    public JGraphLayoutProgress getProgress() {
	return progress;
    }

    /**
     * @return Returns the intraCellSpacing.
     */
    public double getIntraCellSpacing() {
	return intraCellSpacing;
    }

    /**
     * @param intraCellSpacing
     *            The intraCellSpacing to set.
     */
    public void setIntraCellSpacing(double intraCellSpacing) {
	this.intraCellSpacing = intraCellSpacing;
    }

    /**
     * @return Returns the interRankCellSpacing.
     */
    public double getInterRankCellSpacing() {
	return interRankCellSpacing;
    }

    /**
     * @param interRankCellSpacing
     *            The interRankCellSpacing to set.
     */
    public void setInterRankCellSpacing(double interRankCellSpacing) {
	this.interRankCellSpacing = interRankCellSpacing;
    }

    /**
     * @return Returns the orientation.
     */
    public int getOrientation() {
	return orientation;
    }

    /**
     * @param orientation
     *            The orientation to set.
     */
    public void setOrientation(int orientation) {
	this.orientation = orientation;
    }

    /**
     * @return Returns the interHierarchySpacing.
     */
    public double getInterHierarchySpacing() {
	return interHierarchySpacing;
    }

    /**
     * @param interHierarchySpacing
     *            The interHierarchySpacing to set.
     */
    public void setInterHierarchySpacing(double interHierarchySpacing) {
	this.interHierarchySpacing = interHierarchySpacing;
    }

    public double getParallelEdgeSpacing() {
	return parallelEdgeSpacing;
    }

    public void setParallelEdgeSpacing(double parallelEdgeSpacing) {
	this.parallelEdgeSpacing = parallelEdgeSpacing;
    }

    /**
     * @return Returns the fineTuning.
     */
    public boolean isFineTuning() {
	return fineTuning;
    }

    /**
     * @param fineTuning
     *            The fineTuning to set.
     */
    public void setFineTuning(boolean fineTuning) {
	this.fineTuning = fineTuning;
    }

    /**
     * @return Returns the deterministic.
     */
    public boolean isDeterministic() {
	return deterministic;
    }

    /**
     * @param deterministic
     *            The deterministic to set.
     */
    public void setDeterministic(boolean deterministic) {
	this.deterministic = deterministic;
    }

    /**
     * @return Returns the compactLayout.
     */
    public boolean isCompactLayout() {
	return compactLayout;
    }

    /**
     * @param compactLayout
     *            The compactLayout to set.
     */
    public void setCompactLayout(boolean compactLayout) {
	this.compactLayout = compactLayout;
    }

    /**
     * @return Returns the fixRoots.
     */
    public boolean isFixRoots() {
	return fixRoots;
    }

    /**
     * @param fixRoots
     *            The fixRoots to set.
     */
    public void setFixRoots(boolean fixRoots) {
	this.fixRoots = fixRoots;
    }

    public boolean isLayoutFromSinks() {
	return layoutFromSinks;
    }

    public void setLayoutFromSinks(boolean layoutFromSinks) {
	this.layoutFromSinks = layoutFromSinks;
    }

    /**
     * Sets the logging level of this class
     * 
     * @param level
     *            the logging level to set
     */
    public void setLoggerLevel(Level level) {
	try {
	    logger.setLevel(level);
	} catch (SecurityException e) {
	    // Probably running in an applet
	}
    }
}

package se.cambio.openehr.view.util;

/*
File: CollapsablePanel.java

 Copyright 2010 - The Cytoscape Consortium (www.cytoscape.org)

 Code written by: Layla Oesper
 Authors: Layla Oesper, Ruth Isserlin, Daniele Merico

 This library is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with this project.  If not, see <http://www.gnu.org/licenses/>.
 */



import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import se.cambio.openehr.util.OpenEHRImageUtil;

/**
 *
 * User: Vuk Pavlovic
 * Date: Nov 29, 2006
 * Time: 5:34:46 PM
 * Description: The user-triggered collapsable panel containing the component (trigger) in the titled border
 */

/**
 * The user-triggered collapsable panel containing the component (trigger) in the titled border
 */
public class CollapsablePanel extends JPanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    //Border
    CollapsableTitledBorder border; // includes upper left component and line type
    Border collapsedBorderLine = BorderFactory.createEmptyBorder(2, 2, 2, 2); // no border
    Border expandedBorderLine = null; // because this is null, default is used, etched lowered border on MAC

    //Title
    AbstractButton titleComponent; // displayed in the titled border

    //Expand/Collapse button
    final static int COLLAPSED = 0, EXPANDED = 1; // image States
    ImageIcon[] iconArrow = createExpandAndCollapseIcon();
    JButton arrow = createArrowButton();

    //Content Pane
    JPanel panel;

    //Container State
    boolean collapsed; // stores curent state of the collapsable panel

    /**
     * Constructor for an option button controlled collapsable panel.
     * This is useful when a group of options each have unique sub contents. The radio buttons should be created,
     * grouped, and then used to construct their own collapsable panels. This way choosing a different option in
     * the same option group will collapse all unselected options. Expanded panels draw a border around the
     * contents and through the radio button in the fashion of a titled border.
     *
     * @param component Radio button that expands and collapses the panel based on if it is selected or not
     */
    public CollapsablePanel(JRadioButton component) {
	component.addItemListener(new CollapsablePanel.ExpandAndCollapseAction());
	titleComponent = component;
	collapsed = !component.isSelected();
	commonConstructor();
    }

    /**
     * Constructor for a label/button controlled collapsable panel. Displays a clickable title that resembles a
     * native titled border except for an arrow on the right side indicating an expandable panel. The actual border
     * only appears when the panel is expanded.
     *
     * @param text Title of the collapsable panel in string format, used to create a button with text and an arrow icon
     */
    public CollapsablePanel(String text) {
	arrow.setText(text);
	titleComponent = arrow;
	collapsed = true;
	commonConstructor();
    }

    public JButton getActionButton(){
	return arrow;
    }

    /**
     * Sets layout, creates the content panel and adds it and the title component to the container,
     * all constructors have this procedure in common.
     */
    private void commonConstructor () {
	setLayout(new BorderLayout());

	panel = new JPanel();
	panel.setLayout(new BorderLayout());

	add(titleComponent, BorderLayout.CENTER);
	add(panel, BorderLayout.CENTER);
	setCollapsed(collapsed);

	placeTitleComponent();
    }

    /**
     * Sets the bounds of the border title component so that it is properly positioned.
     */
    private void placeTitleComponent() {
	Insets insets = this.getInsets();
	Rectangle containerRectangle = this.getBounds();
	Rectangle componentRectangle = border.getComponentRect(containerRectangle, insets);
	titleComponent.setBounds(componentRectangle);
    }

    public void setTitleComponentText(String text) {
	if (titleComponent instanceof JButton) {
	    titleComponent.setText(text);
	}
	placeTitleComponent();
    }

    /**
     * This class requires that all content be placed within a designated panel, this method returns that panel.
     *
     * @return panel The content panel
     */
    public JPanel getContentPane() {
	return panel;
    }

    /**
     * Collapses or expands the panel.  This is done by adding or removing the content pane,
     * alternating between a frame and empty border, and changing the title arrow.
     * Also, the current state is stored in the collapsed boolean.
     *
     * @param collapse When set to true, the panel is collapsed, else it is expanded
     */
    public void setCollapsed(boolean collapse) {
	if (collapse) {
	    //collapse the panel, remove content and set border to empty border
	    remove(panel);
	    arrow.setIcon(iconArrow[COLLAPSED]);
	    border = new CollapsableTitledBorder(collapsedBorderLine, titleComponent);
	} else {
	    //expand the panel, add content and set border to titled border
	    add(panel, BorderLayout.NORTH);
	    arrow.setIcon(iconArrow[EXPANDED]);
	    border = new CollapsableTitledBorder(expandedBorderLine, titleComponent);
	}
	setBorder(border);
	collapsed = collapse;
	updateUI();
    }

    /**
     * Returns the current state of the panel, collapsed (true) or expanded (false).
     *
     * @return collapsed Returns true if the panel is collapsed and false if it is expanded
     */
    public boolean isCollapsed() {
	return collapsed;
    }

    // Layla 1/6/10 - update for SemanticSummaryPluginlocation
    /**
     * Returns an ImageIcon array with arrow images used for the different states of the panel.
     *
     * @return iconArrow An ImageIcon array holding the collapse and expanded versions of the right hand side arrow
     */
    private ImageIcon[] createExpandAndCollapseIcon () {
	ImageIcon[] iconArrow = new ImageIcon[2];
	iconArrow[COLLAPSED] = OpenEHRImageUtil.EXPAND_ICON;
	iconArrow[EXPANDED] = OpenEHRImageUtil.CONTRACT_ICON;
	return iconArrow;
    }

    /**
     * Returns a button with an arrow icon and a collapse/expand action listener.
     *
     * @return button Button which is used in the titled border component
     */
    private JButton createArrowButton () {
	JButton button = new JButton("arrow", iconArrow[COLLAPSED]);
	button.setBorder(BorderFactory.createEmptyBorder(0,1,5,1));
	button.setVerticalTextPosition(AbstractButton.CENTER);
	button.setHorizontalTextPosition(AbstractButton.LEFT);
	button.setMargin(new Insets(0,0,3,0));

	//We want to use the same font as those in the titled border font
	Font font = BorderFactory.createTitledBorder("Sample").getTitleFont();
	Color color = BorderFactory.createTitledBorder("Sample").getTitleColor();
	button.setFont(font);
	button.setForeground(color);
	button.setFocusable(false);
	button.setContentAreaFilled(false);

	button.addActionListener(new CollapsablePanel.ExpandAndCollapseAction());

	return button;
    }

    /**
     * Handles expanding and collapsing of extra content on the user's click of the titledBorder component.
     */
    private class ExpandAndCollapseAction extends AbstractAction implements ActionListener, ItemListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public void actionPerformed(ActionEvent e) {
	    setCollapsed(!isCollapsed());
	}
	public void itemStateChanged(ItemEvent e) {
	    setCollapsed(!isCollapsed());
	}
    }

    /**
     * Layla July 26, 2010 - Add methods to be able to set tooltip on title.
     */
    public AbstractButton getTitleComponent()
    {
	return titleComponent;
    }

    /**
     * Special titled border that includes a component in the title area
     */
    private class CollapsableTitledBorder extends TitledBorder {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	JComponent component;
	//Border border;

	public CollapsableTitledBorder(Border border, JComponent component) {
	    this(border, component, LEFT, TOP);
	}

	public CollapsableTitledBorder(Border border, JComponent component, int titleJustification, int titlePosition) {
	    //TitledBorder needs border, title, justification, position, font, and color
	    super(border, null, titleJustification, titlePosition, null, null);
	    this.component = component;
	    if (border == null) {
		this.border = super.getBorder();
	    }
	}

	public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
	    Rectangle borderR = new Rectangle(x + EDGE_SPACING, y + EDGE_SPACING, width - (EDGE_SPACING * 2), height - (EDGE_SPACING * 2));
	    Insets borderInsets;
	    if (border != null) {
		borderInsets = border.getBorderInsets(c);
	    } else {
		borderInsets = new Insets(0, 0, 0, 0);
	    }

	    Rectangle rect = new Rectangle(x, y, width, height);
	    Insets insets = getBorderInsets(c);
	    Rectangle compR = getComponentRect(rect, insets);
	    int diff;
	    switch (titlePosition) {
	    case ABOVE_TOP:
		diff = compR.height + TEXT_SPACING;
		borderR.y += diff;
		borderR.height -= diff;
		break;
	    case TOP:
	    case DEFAULT_POSITION:
		diff = insets.top / 2 - borderInsets.top - EDGE_SPACING;
		borderR.y += diff;
		borderR.height -= diff;
		break;
	    case BELOW_TOP:
	    case ABOVE_BOTTOM:
		break;
	    case BOTTOM:
		diff = insets.bottom / 2 - borderInsets.bottom - EDGE_SPACING;
		borderR.height -= diff;
		break;
	    case BELOW_BOTTOM:
		diff = compR.height + TEXT_SPACING;
		borderR.height -= diff;
		break;
	    }
	    border.paintBorder(c, g, borderR.x, borderR.y, borderR.width, borderR.height);
	    Color col = g.getColor();
	    g.setColor(c.getBackground());
	    g.fillRect(compR.x, compR.y, compR.width, compR.height);
	    g.setColor(col);
	}

	public Insets getBorderInsets(Component c, Insets insets) {
	    Insets borderInsets;
	    if (border != null) {
		borderInsets = border.getBorderInsets(c);
	    } else {
		borderInsets = new Insets(0, 0, 0, 0);
	    }
	    insets.top = EDGE_SPACING + TEXT_SPACING + borderInsets.top;
	    insets.right = EDGE_SPACING + TEXT_SPACING + borderInsets.right;
	    insets.bottom = EDGE_SPACING + TEXT_SPACING + borderInsets.bottom;
	    insets.left = EDGE_SPACING + TEXT_SPACING + borderInsets.left;

	    if (c == null || component == null) {
		return insets;
	    }

	    int compHeight = component.getPreferredSize().height;

	    switch (titlePosition) {
	    case ABOVE_TOP:
		insets.top += compHeight + TEXT_SPACING;
		break;
	    case TOP:
	    case DEFAULT_POSITION:
		insets.top += Math.max(compHeight, borderInsets.top) - borderInsets.top;
		break;
	    case BELOW_TOP:
		insets.top += compHeight + TEXT_SPACING;
		break;
	    case ABOVE_BOTTOM:
		insets.bottom += compHeight + TEXT_SPACING;
		break;
	    case BOTTOM:
		insets.bottom += Math.max(compHeight, borderInsets.bottom) - borderInsets.bottom;
		break;
	    case BELOW_BOTTOM:
		insets.bottom += compHeight + TEXT_SPACING;
		break;
	    }
	    return insets;
	}

	public Rectangle getComponentRect(Rectangle rect, Insets borderInsets) {
	    Dimension compD = component.getPreferredSize();
	    Rectangle compR = new Rectangle(0, 0, compD.width, compD.height);
	    switch (titlePosition) {
	    case ABOVE_TOP:
		compR.y = EDGE_SPACING;
		break;
	    case TOP:
	    case DEFAULT_POSITION:
		if (titleComponent instanceof JButton) {
		    compR.y = EDGE_SPACING + (borderInsets.top - EDGE_SPACING - TEXT_SPACING - compD.height) / 2;
		} else if (titleComponent instanceof JRadioButton) {
		    compR.y = (borderInsets.top - EDGE_SPACING - TEXT_SPACING - compD.height) / 2;
		}
		break;
	    case BELOW_TOP:
		compR.y = borderInsets.top - compD.height - TEXT_SPACING;
		break;
	    case ABOVE_BOTTOM:
		compR.y = rect.height - borderInsets.bottom + TEXT_SPACING;
		break;
	    case BOTTOM:
		compR.y = rect.height - borderInsets.bottom + TEXT_SPACING + (borderInsets.bottom - EDGE_SPACING - TEXT_SPACING - compD.height) / 2;
		break;
	    case BELOW_BOTTOM:
		compR.y = rect.height - compD.height - EDGE_SPACING;
		break;
	    }
	    switch (titleJustification) {
	    case LEFT:
	    case DEFAULT_JUSTIFICATION:
		//compR.x = TEXT_INSET_H + borderInsets.left;
		compR.x = TEXT_INSET_H + borderInsets.left - EDGE_SPACING;
		break;
	    case RIGHT:
		compR.x = rect.width - borderInsets.right - TEXT_INSET_H - compR.width;
		break;
	    case CENTER:
		compR.x = (rect.width - compR.width) / 2;
		break;
	    }
	    return compR;
	}
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */
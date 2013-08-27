package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.plaf.basic.BasicButtonUI;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.view.panels.interfaces.ClosableTabbebPane;

public class ButtonTabComponent extends JPanel {

    private static final long serialVersionUID = 3156550658557495889L;
    //private final JTabbedPane pane;
    private ClosableTabbebPane closableTabbebPane;
    private String _tooltip = null;

    public ButtonTabComponent(ClosableTabbebPane tabPanel, String closeButtonTooltip) {
	//unset default FlowLayout' gaps
	super(new FlowLayout(FlowLayout.LEFT, 0, 0));
	_tooltip = closeButtonTooltip;
	if (tabPanel == null) {
	    throw new NullPointerException("TabbedPane is null");
	}
	this.closableTabbebPane = tabPanel;
	final JTabbedPane pane = closableTabbebPane.getTabbedPane();
	setOpaque(false);

	//make JLabel read titles from JTabbedPane
	JLabel label = new JLabel() {
	    private static final long serialVersionUID = 1L;

	    public String getText() {
		int i = pane.indexOfTabComponent(ButtonTabComponent.this);
		if (i != -1) {
		    return pane.getTitleAt(i);
		}
		return null;
	    }
	};

	add(label);
	//add more space between the label and the button
	label.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
	//tab button
	JButton button = new TabButton();
	add(button);
	//add more space to the top of the component
	setBorder(BorderFactory.createEmptyBorder(2, 0, 0, 0));
    }

    private class TabButton extends JButton implements ActionListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public TabButton() {
	    int size = 17;
	    setPreferredSize(new Dimension(size, size));
	    setToolTipText(_tooltip);
	    //Make the button looks the same for all Laf's
	    setUI(new BasicButtonUI());
	    //Make it transparent
	    setContentAreaFilled(false);
	    //No need to be focusable
	    setFocusable(false);
	    setBorder(BorderFactory.createEtchedBorder());
	    setBorderPainted(false);
	    //Making nice rollover effect
	    //we use the same listener for all buttons
	    addMouseListener(buttonMouseListener);
	    setRolloverEnabled(true);
	    //Close the proper tab by clicking the button
	    addActionListener(this);
	}

	public void actionPerformed(ActionEvent e) {
	    int index = closableTabbebPane.getTabbedPane().indexOfTabComponent(ButtonTabComponent.this);
	    closableTabbebPane.deleteTab(index);
	}

	//we don't want to update UI for this button
	public void updateUI() {
	}

	//paint the cross
	protected void paintComponent(Graphics g) {
	    super.paintComponent(g);
	    Graphics2D g2 = (Graphics2D) g.create();
	    //shift the image for pressed buttons
	    if (getModel().isPressed()) {
		g2.translate(1, 1);
	    }
	    g2.setStroke(new BasicStroke(2));
	    g2.setColor(Color.BLACK);
	    if (getModel().isRollover()) {
		g2.setColor(Color.MAGENTA);
	    }
	    //int delta = 6;
	    //g2.drawLine(delta, delta, getWidth() - delta - 1, getHeight() - delta - 1);
	    //g2.drawLine(getWidth() - delta - 1, delta, delta, getHeight() - delta - 1);
	    g2.drawImage(GDLEditorImageUtil.DELETE_ICON.getImage(), 0, 0, 16, 16, this);
	    g2.dispose();
	}
    }

    private final static MouseListener buttonMouseListener = new MouseAdapter() {
	public void mouseEntered(MouseEvent e) {
	    Component component = e.getComponent();
	    if (component instanceof AbstractButton) {
		AbstractButton button = (AbstractButton) component;
		button.setBorderPainted(true);
	    }
	}

	public void mouseExited(MouseEvent e) {
	    Component component = e.getComponent();
	    if (component instanceof AbstractButton) {
		AbstractButton button = (AbstractButton) component;
		button.setBorderPainted(false);
	    }
	}
    };
}/*
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
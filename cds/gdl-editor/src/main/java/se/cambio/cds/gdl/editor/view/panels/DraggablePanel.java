package se.cambio.cds.gdl.editor.view.panels;
import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;

public abstract class DraggablePanel extends JPanel implements MouseListener, MouseMotionListener {
    private static final long serialVersionUID = 1L;
    private Point mLastPoint;
    private JLabel _dragLabel = null;

    public DraggablePanel(JComponent component) {
	super(new BorderLayout());
	JPanel aux = new JPanel(new BorderLayout());
	aux.add(getDragLabel(), BorderLayout.NORTH);
	this.add(aux, BorderLayout.WEST);
	this.add(component, BorderLayout.CENTER);
	this.setBounds(50, 50, (int)component.getSize().getWidth(), (int)component.getSize().getHeight());
	getDragLabel().addMouseListener(this);
	getDragLabel().addMouseMotionListener(this);
    }

    public JLabel getDragLabel(){
	if (_dragLabel==null){
	    _dragLabel = new JLabel();
	    _dragLabel.setIcon(GDLEditorImageUtil.DRAG_ICON);
	    _dragLabel.setToolTipText(GDLEditorLanguageManager.getMessage("MoveLine"));
	}
	return _dragLabel;
    }

    public void mouseDragged(MouseEvent event_) {
	int x, y;
	if(mLastPoint != null) {
	    x = 0;
	    y = super.getY() + (event_.getY() - (int)mLastPoint.getY());
	    super.setLocation(x, y);
	}
    }

    public void mouseMoved(MouseEvent event_) {
	setCursorType(event_.getPoint());
    }
    public void mouseClicked(MouseEvent event_) {}
    public void mouseEntered(MouseEvent event_) {}
    public void mouseExited(MouseEvent event_) {
	if(mLastPoint == null){
	    super.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
	}
    }

    public void mousePressed(MouseEvent event) {
	if(super.getCursor().equals(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))) {
	    mLastPoint = event.getPoint();
	    super.getParent().add(this, 0);
	    super.getParent().repaint();
	} else {
	    mLastPoint = null;
	}
    }

    public void mouseReleased(MouseEvent e) {
	mLastPoint = null;
	if (super.getParent() instanceof DropPanel){
	    ((DropPanel)super.getParent()).panelDragged(this);
	}
    }

    private void setCursorType(Point p) {
	Point loc = super.getLocation();
	Dimension size = super.getSize();

	if((p.y + 4 < loc.y + size.height) && (p.x + 4 < p.x + size.width)) {
	    super.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
	}
    }
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
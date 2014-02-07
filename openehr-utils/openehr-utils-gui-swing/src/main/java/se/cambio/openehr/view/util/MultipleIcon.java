/*
 * Creado el 14-dic-2007
 *


 */
package se.cambio.openehr.view.util;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;
import javax.swing.ImageIcon;

/**
 * @author icorram
 *


 */
public class MultipleIcon extends ImageIcon{
	/**
	 * Comentario para <code>serialVersionUID</code>
	 */
	private static final long serialVersionUID = -1451706192925963084L;

	/**
	 * the x position of the icon
	 */
	private int x_pos;
	
	/**
	 * the y position of the icon
	 */
	private int y_pos;
	
	/**
	 * the additional fileicon
	 */
	private Icon [] fileIcons;
	
	/**
	 * Creates a new instance of <code>CloseTabIcon</code>
	 * @param fileIcons the additional fileicons, if there is one set
	 */
	
	public MultipleIcon(Icon [] fileIcons) {
		this.fileIcons = fileIcons;
	}
	
	/**
	 * Draw the icon at the specified location. Icon implementations may use the
	 * Component argument to get properties useful for painting, e.g. the
	 * foreground or background color.
	 * @param c the component which the icon belongs to
	 * @param g the graphic object to draw on
	 * @param x the upper left point of the icon in the x direction
	 * @param y the upper left point of the icon in the y direction
	 */
	public void paintIcon(Component c, Graphics g, int x, int y) {
		x_pos = x;
		for (int i=0;i<fileIcons.length;i++){
			y_pos = y;
			fileIcons[i].paintIcon(c, g, x_pos, y_pos);
			x_pos = x_pos + fileIcons[i].getIconWidth();
		}
	}

	/* (sin Javadoc)
	 * @see javax.swing.Icon#getIconWidth()
	 */
	public int getIconWidth() {
		int totalWidth = 0;
		for (int i=0;i<fileIcons.length;i++){
			totalWidth = totalWidth + fileIcons[i].getIconWidth();
		}
		return totalWidth;
	}

	/* (sin Javadoc)
	 * @see javax.swing.Icon#getIconHeight()
	 */
	public int getIconHeight() {
		int maxHeight = 0;
		for (int i=0;i<fileIcons.length;i++){
			maxHeight = Math.max(maxHeight, fileIcons[i].getIconHeight());
		}
		return maxHeight;
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
package se.cambio.cds.gdl.editor.view.util;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;
import javax.swing.ImageIcon;

public class MultipleIcon extends ImageIcon {
    private static final long serialVersionUID = -1451706192925963084L;

    private int xPos;

    private int yPos;

    private Icon[] fileIcons;

    public MultipleIcon(Icon[] fileIcons) {
        this.fileIcons = fileIcons;
    }

    public void paintIcon(Component component, Graphics graphics, int posX, int posY) {
        xPos = posX;
        for (Icon fileIcon : fileIcons) {
            yPos = posY;
            fileIcon.paintIcon(component, graphics, xPos, yPos);
            xPos = xPos + fileIcon.getIconWidth();
        }
    }

    public int getIconWidth() {
        int totalWidth = 0;
        for (Icon fileIcon : fileIcons) {
            totalWidth = totalWidth + fileIcon.getIconWidth();
        }
        return totalWidth;
    }

    public int getIconHeight() {
        int maxHeight = 0;
        for (Icon fileIcon : fileIcons) {
            maxHeight = Math.max(maxHeight, fileIcon.getIconHeight());
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
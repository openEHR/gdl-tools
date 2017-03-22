package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.util.misc.Version;

import javax.swing.*;
import java.awt.*;

public class SplashPanel extends JPanel {
    private static final long serialVersionUID = 1L;
    private Image img;

    public SplashPanel() {
        this.img = GDLEditorImageUtil.SPLASH_IMAGE.getImage();
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(img.getWidth(null), img.getHeight(null));
    }

    @Override
    public void paintComponent(Graphics g) {
        g.drawImage(img, 0, 0, this);
        g.setColor(new Color(56, 114, 57));
        g.setFont(new Font("Arial", Font.BOLD, 17));
        g.setColor(new Color(255, 255, 255));
        g.setFont(new Font("Verdana", Font.BOLD, 12));
        g.setColor(Color.white);
        String versionNum = Version.getVersionNum();
        String buildNum = Version.getBuildNum();
        buildNum = buildNum == null ? "" : buildNum;
        if (versionNum != null) {
            g.drawString("v" + versionNum + " (" + buildNum + ")", 47, 281);
        }
        g.drawString(GDLEditorLanguageManager.getMessage("Developers") + ": Iago Corbal, Rong Chen", 47, 297);
        g.drawString(GDLEditorLanguageManager.getMessage("Contributors") + ": Konstantinos Kalliamvakos, Mihindu Wijesena", 47, 313);
        g.drawString(GDLEditorLanguageManager.getMessage("FundedBy") + ": Cambio Healthcare Systems (cambio.se)", 47, 329);
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
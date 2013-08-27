package se.cambio.cds.gdl.editor.view.panels;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;

import javax.swing.JLabel;
import javax.swing.JPanel;

import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.util.Version;

public class SplashPanel extends JPanel {
    private static final long serialVersionUID = 1L;

    public SplashPanel() {
	JLabel label = new JLabel();
	label.setIcon(GDLEditorImageUtil.SPLASH_IMAGE);
	add(label);
    }

    public void paint(Graphics g) {
	g.drawImage(GDLEditorImageUtil.SPLASH_IMAGE.getImage(), 0, 0, this);
	g.setColor(new Color(56,114,57));
	g.setFont(new Font("Arial",Font.BOLD,17));
	//g.drawString(LanguageManager.getMessage("ApplicationName"), 172, 104);
	g.setColor(new Color(255,255,255));
	//g.drawString(LanguageManager.getMessage("ApplicationName"), 173, 105);
	g.setFont(new Font("Verdana",Font.BOLD,13));
	g.setColor(Color.white);
	String versionNum = Version.getVersionNum();
	if (versionNum!=null){
	    g.drawString("v"+versionNum, 47, 281);
	}
	String buildNum = Version.getBuildNum();
	if (buildNum!=null){
	    g.drawString(buildNum, 47, 297);
	}
	g.drawString(GDLEditorLanguageManager.getMessage("Authors")+": Iago Corbal, Rong Chen", 47, 313);
	g.drawString(GDLEditorLanguageManager.getMessage("FundedBy")+": Cambio Healthcare Systems (cambio.se)", 47, 329);
	
	/*
			g.drawString(LanguageManager.getMessage("HospitalCenterName")
					+" ("+LanguageManager.getMessage("HospitalCenterWeb")+")", 140, 195);
	 */
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
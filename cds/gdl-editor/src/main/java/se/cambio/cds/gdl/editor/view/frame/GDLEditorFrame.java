package se.cambio.cds.gdl.editor.view.frame;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.util.Version;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.openehr.util.ExceptionHandler;



/**
 * @author iago.corbal
 *
 */

public class GDLEditorFrame extends JFrame implements EditorViewer{


    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private MainMenuBar principalMenuBar;

    /**
     * This is the default constructor
     */
    public GDLEditorFrame() {
	super();
	initialize();
    }

    /**
     * This method initializes this
     */
    private  void initialize() {
	ToolTipManager.sharedInstance().setDismissDelay(30000);
	Dimension screenSize =
		Toolkit.getDefaultToolkit().getScreenSize();
	Dimension labelSize = this.getSize();
	this.setSize(new Dimension(1000, 600));
	int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
	int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
	this.setLocation(locx,locy);
	this.setResizable(true);
	this.addWindowListener(new CancelarCambiosAction());
	this.setJMenuBar(getMainMenuBar());
	this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
	this.setIconImage(GDLEditorImageUtil.LOGO.getImage());
    }


    protected class CancelarCambiosAction extends WindowAdapter{

	public void windowOpened(WindowEvent e){
	}

	public void actionPerformed(ActionEvent e) {
	    EditorManager.closeEditor();
	}

	public void windowClosing(WindowEvent we) {
	    EditorManager.getActiveGDLEditor().getEditorPanel().requestFocusInWindow();
	    try {
		SwingUtilities.invokeLater(new Runnable() {
		    @Override
		    public void run() {
			EditorManager.closeEditor();
		    }
		});
	    } catch (Exception e) {
		ExceptionHandler.handle(e);
	    }

	}
    }

    public void initController(GDLEditor controller) {
	String buildNum = Version.getBuildNum();
	setTitle(GDLEditorLanguageManager.getMessage("GDLEditor") +" - "+controller.getTitle()+(buildNum!=null?" - ("+buildNum+")":""));
	controller.init();
    }

    public MainMenuBar getMainMenuBar() {
	if (principalMenuBar == null) {
	    principalMenuBar = new MainMenuBar();
	}
	return principalMenuBar;
    }

    public void setContent(JPanel panel) {
	setContentPane(panel);
	this.validate();
	this.repaint();
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
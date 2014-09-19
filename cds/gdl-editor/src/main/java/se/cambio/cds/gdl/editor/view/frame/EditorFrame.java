package se.cambio.cds.gdl.editor.view.frame;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorController;
import se.cambio.cds.gdl.editor.controller.interfaces.EditorViewer;
import se.cambio.cds.gdl.editor.util.GDLEditorImageUtil;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.cds.util.misc.Version;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;



/**
 * @author iago.corbal
 *
 */

public class EditorFrame extends JFrame implements EditorViewer{


    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private MainMenuBar principalMenuBar;

    /**
     * This is the default constructor
     */
    public EditorFrame() {
        super();
        initialize();
    }

    /**
     * This method initializes this
     */
    private  void initialize() {
        setTooltipDelay();
        setPositionAndDimension();
        this.setResizable(true);
        this.addWindowListener(new WindowListener());
        this.setJMenuBar(getMainMenuBar());
        this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        this.setIconImage(GDLEditorImageUtil.LOGO.getImage());
    }

    private void setPositionAndDimension() {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension labelSize = this.getSize();
        this.setSize(new Dimension(1000, 600));
        int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
        int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
        this.setLocation(locx,locy);
    }

    private void setTooltipDelay() {
        ToolTipManager.sharedInstance().setDismissDelay(30000);
    }


    protected class WindowListener extends WindowAdapter{
        public void windowClosing(WindowEvent we) {
            EditorManager.requestFocusInWindow();
            EditorManager.closeEditor();
        }
    }

    public void initController(EditorController controller) {
        String buildNum = Version.getBuildNum();
        setTitle(controller.getTitle()+(buildNum!=null?" - ("+buildNum+")":""));
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

package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.view.panels.ProgressBarPanel;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author icorram
 *
 */
public class MultiProgressDialog extends JDialog {

    /**
     *
     */
    private static final long serialVersionUID = -2310821412359230220L;

    private JPanel mainPanel = null;  //  @jve:decl-index=0:visual-constraint="372,178"
    private Collection<ProgressBarPanel> progressBarPanels = null;

    /**
     * This method initializes
     *
     */
    public MultiProgressDialog(Window owner, Collection<ProgressBarPanel> progressBarPanels) {
        super(owner, "", ModalityType.APPLICATION_MODAL);
        this.progressBarPanels = progressBarPanels;
        initialize();
    }
    /**
     * This method initializes this
     */
    private void initialize() {
        this.setSize(600, 480);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setContentPane(getMainPanel());
        this.setResizable(true);
    }


    /**
     * This method initializes jPanel1	
     *
     * @return javax.swing.JPanel
     */
    private JPanel getMainPanel() {
        if (mainPanel == null) {
            mainPanel = new JPanel(new BorderLayout());
            JScrollPane jScrollPane = new JScrollPane();
            mainPanel.add(jScrollPane, BorderLayout.NORTH);
            JPanel listPanel = new JPanel();
            listPanel.setLayout(new BoxLayout(listPanel, BoxLayout.Y_AXIS));
            jScrollPane.setViewportView(listPanel);
            for(ProgressBarPanel progressBarPanel: progressBarPanels){
                 listPanel.add(progressBarPanel);
            }
        }
        return mainPanel;
    }

    public static void main(String[] args){
        Collection<ProgressBarPanel> progressBarPanels = new ArrayList<ProgressBarPanel>();
        ProgressBarPanel pbp = new ProgressBarPanel();
        pbp.start();
        pbp.changeLoadingText("Testing1");
        progressBarPanels.add(pbp);
        pbp = new ProgressBarPanel();
        pbp.start();
        pbp.changeLoadingText("Testing2");
        progressBarPanels.add(pbp);
        pbp = new ProgressBarPanel();
        pbp.start();
        pbp.changeLoadingText("Testing3");
        progressBarPanels.add(pbp);
        new MultiProgressDialog(null, progressBarPanels).setVisible(true);
        System.exit(0);
    }

}  //  @jve:decl-index=0:visual-constraint="10,10"
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
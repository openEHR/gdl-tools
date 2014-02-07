
package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.util.ProgressManager;
import se.cambio.openehr.view.panels.ProgressBarPanel;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.Future;

/**
 * @author icorram
 *
 */
public class InfoDialog extends JDialog implements ProgressManager {

    /**
     *
     */
    private static final long serialVersionUID = -2310821412359230220L;

    private ProgressBarPanel progressBarPanel = null;  //  @jve:decl-index=0:visual-constraint="372,178"


    /**
     * This method initializes 
     *
     */
    public InfoDialog(Window owner) {
        super(owner, "", ModalityType.APPLICATION_MODAL);
        initialize();
    }
    /**
     * This method initializes this
     */
    private void initialize() {
        this.setSize(200, 80);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setContentPane(getProgressBarPanel());
        this.setUndecorated(true);
    }

    public void changeLoadingText(String description){
        getProgressBarPanel().changeLoadingText(description);
    }

    public void start(){
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        getProgressBarPanel().start();
        this.setVisible(true);
    }

    public void stop(){
        getProgressBarPanel().stop();
        this.setVisible(false);
    }

    public void setCurrentProgress(String msg, double progress){
        getProgressBarPanel().setCurrentProgress(msg, progress);
    }

    public void setCurrentThread(Future<?> currentThread){
        getProgressBarPanel().setCurrentThread(currentThread);
    }

    public Future<?> getCurrentThread(){
        return getProgressBarPanel().getCurrentThread();
    }

    /**
     * This method initializes jPanel1	
     *
     * @return javax.swing.JPanel
     */
    private ProgressBarPanel getProgressBarPanel() {
        if (progressBarPanel == null) {
            progressBarPanel = new ProgressBarPanel();
        }
        return progressBarPanel;
    }

    public static void main(String[] args){
        new InfoDialog(null).setVisible(true);
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
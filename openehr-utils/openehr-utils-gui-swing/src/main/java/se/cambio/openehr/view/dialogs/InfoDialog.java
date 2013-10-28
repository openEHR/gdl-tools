
package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Future;
/**
 * @author icorram
 *
 */
public class InfoDialog extends JDialog{

    /**
     * 
     */
    private static final long serialVersionUID = -2310821412359230220L;
    private JProgressBar jProgressBar = null;
    private JPanel jPanel1 = null;  //  @jve:decl-index=0:visual-constraint="372,178"
    private JLabel jLabel2 = null;

    private Future<?> _currentThread = null;
    private boolean _isProgresoActivo = false;
    private int _progressValue = 0;
    private String _description = null;
    private JButton cancelButton;

    /**
     * This method initializes 
     * 
     */
    public InfoDialog(Window owner) {
	super(owner, "", ModalityType.APPLICATION_MODAL);
	_description = "";
	initialize();
    }
    /**
     * This method initializes this
     */
    private void initialize() {
	Dimension screenSize =
		Toolkit.getDefaultToolkit().getScreenSize();
	Dimension labelSize = this.getSize();
	this.setSize(200, 80);
	int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
	int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
	this.setLocation(locx,locy);
	this.setContentPane(getJPanel1());
	this.setUndecorated(true);
	getSplashLogo().setText(_description);
    }

    public void changeLoadingText(String description){
	_description = description;
	getSplashLogo().setText(_description);
	this.repaint();
	this.validate();
    }

    public void start(){
	_isProgresoActivo = true;
	_progressValue = 1;
	new Thread(new ProgresoActivo()).start();
	this.setVisible(true);
    }

    public void stop(){
	_isProgresoActivo = false;
	_progressValue = -100;
	_currentThread = null;
	getCancelButton().setVisible(false);
	this.setVisible(false);
    }

    public void setCurrentProgress(String msg, double progress){
	_isProgresoActivo = false;
	_progressValue = (int)(100*progress);
	_description = msg;
	stateUpdated();
    }
    
    public void setCurrentThread(Future<?> currentThread){
	_currentThread = currentThread;
	getCancelButton().setVisible(true);
	//this.repaint();
	//this.validate();
    }

    public Future<?> getCurrentThread(){
	return _currentThread;
    }

    private JButton getCancelButton(){
	if (cancelButton==null){
	    cancelButton = new JButton(OpenEHRLanguageManager.getMessage("Cancel"));
	    cancelButton.setIcon(OpenEHRImageUtil.STOP_ICON);
	    cancelButton.setBackground(null);
	    cancelButton.setBorder(BorderFactory.createEmptyBorder());
	    cancelButton.setVisible(false);
	    cancelButton.setFocusable(false);
	    cancelButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    _currentThread.cancel(true);

		    stop();
		}
	    });
	}
	return cancelButton;
    }

    private JLabel getSplashLogo(){
	if (jLabel2 == null){
	    jLabel2 = new JLabel();
	    jLabel2.setText("");
	}
	return jLabel2;
    }

    /**
     * This method initializes jProgressBar	
     * 	
     * @return javax.swing.JProgressBar	
     */    
    private JProgressBar getJProgressBar() {
	if (jProgressBar == null) {
	    jProgressBar = new JProgressBar();
	    jProgressBar.setName("jProgressBar");
	    jProgressBar.setPreferredSize(new java.awt.Dimension(200,14));
	}
	return jProgressBar;
    }

    /* (non-Javadoc)
     * @see es.sergas.canalejo.sisegtx.model.facade.vo.StatusObserver#stateUpdated()
     */


    public void stateUpdated() {
	getSplashLogo().setText(_description);
	if (_progressValue>=0){
	    //getJProgressBar().setVisible(true);
	    getJProgressBar().setValue(_progressValue);
	}
    }

    private class ProgresoActivo implements Runnable{
	private boolean up = true;
	public void run() {
	    while (_isProgresoActivo){
		if (up) _progressValue = _progressValue+3;
		else _progressValue = _progressValue-3;
		if (_progressValue>=100) up = false;
		else if (_progressValue<=1) up = true;
		stateUpdated();
		try {
		    Thread.sleep(50);
		} catch (InterruptedException e) {
		}
	    }
	}
    }

    /**
     * This method initializes jPanel1	
     * 	
     * @return javax.swing.JPanel	
     */    
    private JPanel getJPanel1() {
	if (jPanel1 == null) {
	    GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
	    jPanel1 = new JPanel();
	    jPanel1.setLayout(new GridBagLayout());
	    jPanel1.setBackground(Color.WHITE);
	    jPanel1.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.LOWERED));
	    gridBagConstraints1.gridx = 0;
	    gridBagConstraints1.gridy = 0;
	    gridBagConstraints1.weightx=1;
	    gridBagConstraints1.weighty=1;
	    gridBagConstraints1.insets = new java.awt.Insets(0,5,0,0);
	    gridBagConstraints1.fill = java.awt.GridBagConstraints.HORIZONTAL;
	    gridBagConstraints1.anchor = java.awt.GridBagConstraints.WEST;
	    gridBagConstraints1.insets = new java.awt.Insets(5,10,0,10);
	    jPanel1.add(getSplashLogo(), gridBagConstraints1);
	    gridBagConstraints1.gridy++;
	    jPanel1.add(getJProgressBar(), gridBagConstraints1);
	    gridBagConstraints1.gridy++;
	    gridBagConstraints1.fill = java.awt.GridBagConstraints.NONE;
	    gridBagConstraints1.anchor = java.awt.GridBagConstraints.CENTER;
	    jPanel1.add(getCancelButton(), gridBagConstraints1);
	}
	return jPanel1;
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
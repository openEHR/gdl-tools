
package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.util.GDLLoadingUtility;
import se.cambio.cds.gdl.editor.view.panels.SplashPanel;
import se.cambio.openehr.controller.InitialLoadingObservable;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.Future;
/**
 * @author icorram
 *
 */
public class DialogSplash extends JDialog implements Observer{

    /**
     *
     */
    private static final long serialVersionUID = -2310821412359230220L;
    private JProgressBar jProgressBar = null;
    private JPanel jPanel1 = null;  //  @jve:decl-index=0:visual-constraint="372,178"
    private JLabel jLabel2 = null;

    private Future<?> _currentThread = null;
    //private boolean _isProgresoActivo = false;
    private int _progressValue = 0;
    private String _description = null;
    private JButton cancelButton;
    private boolean _loading = false;
    private JButton closeButton;
    private static Integer NUM_OBJ_TO_LOAD = 4;

    /**
     * This method initializes 
     *
     */
    public DialogSplash(Window owner, boolean loading) {
        super(owner, "", ModalityType.APPLICATION_MODAL);
        _description = "";
        _loading = loading;
        initialize();
    }
    /**
     * This method initializes this
     */
    private void initialize() {
        Dimension screenSize =
                Toolkit.getDefaultToolkit().getScreenSize();
        Dimension labelSize = this.getSize();
        this.setSize(633, 416);
        int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
        int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
        this.setLocation(locx,locy);
        this.setContentPane(getJPanel1());
        this.setUndecorated(true);
    }

    public void changeLoadingText(String description){
        _description = description;
        getSplashLabel().setText(_description);
        this.repaint();
        this.validate();
    }

    /*
    public void start(){
	_isProgresoActivo = true;
	_progressValue = 1;
	new Thread(new ProgresoActivo()).start();
	this.setVisible(true);
    }*/

    public void stop(){
        //_isProgresoActivo = false;
        _progressValue = -100;
        _currentThread = null;
        InitialLoadingObservable.getDelegate().deleteObserver(this);
        getCancelButton().setVisible(false);
        this.setVisible(false);
    }

    /*
    private void setCurrentProgress(String msg, double progress){
	_isProgresoActivo = false;
	_progressValue = (int)(100*progress);
	_description = msg;
	update();
    }*/

    public void setCurrentThread(Future<?> currentThread){
        _currentThread = currentThread;
        getCancelButton().setVisible(true);
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


    private JLabel getSplashLabel(){
        if (jLabel2 == null){
            jLabel2 = new JLabel();
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
            jProgressBar.setPreferredSize(new java.awt.Dimension(300,20));
        }
        return jProgressBar;
    }

    /* (non-Javadoc)
     * @see es.sergas.canalejo.sisegtx.model.facade.vo.StatusObserver#stateUpdated()
     */

    public void update() {
        getSplashLabel().setText(_description);
        _progressValue =  (int)(100*getTotalLoadingProgress());
        if (_progressValue>=0){
            //getJProgressBar().setVisible(true);
            getJProgressBar().setValue(_progressValue);
        }
    }

    public static Double getTotalLoadingProgress(){
        return ((double)InitialLoadingObservable.getNumLoaded()/NUM_OBJ_TO_LOAD) +
                InitialLoadingObservable.getCurrentStageProgress()/NUM_OBJ_TO_LOAD;
    }
    @Override
    public void update(Observable o, Object arg) {
        _description = GDLLoadingUtility.getCurrentLoadingStageName();
        update();
    }



    /*
    private class ProgresoActivo implements Runnable{
	private boolean up = true;
	public void run() {
	    while (_isProgresoActivo){
		if (up) _progressValue = _progressValue+3;
		else _progressValue = _progressValue-3;
		if (_progressValue>=100) up = false;
		else if (_progressValue<=1) up = true;
		update();
		try {
		    Thread.sleep(50);
		} catch (InterruptedException e) {
		}
	    }
	}
    }
     */
    /**
     * This method initializes jPanel1	
     *
     * @return javax.swing.JPanel
     */
    private JPanel getJPanel1() {
        if (jPanel1 == null) {
            jPanel1 = new JPanel(new BorderLayout());
            jPanel1.setBackground(Color.WHITE);
            jPanel1.add(new SplashPanel(), BorderLayout.CENTER);
            JPanel panelAux = new JPanel(new BorderLayout(5,5));
            panelAux.setBackground(Color.WHITE);
            jPanel1.add(panelAux, BorderLayout.SOUTH);
            panelAux.setBorder(BorderFactory.createEmptyBorder(10, 20, 15, 20));
            if (_loading){
                panelAux.add(getSplashLabel(), BorderLayout.NORTH);
                panelAux.add(getJProgressBar(), BorderLayout.CENTER);
            }else{
                JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
                panelAux2.setBackground(Color.WHITE);
                panelAux2.add(getCloseButton());
                panelAux.add(panelAux2, BorderLayout.CENTER);
            }
        }
        return jPanel1;
    }

    public JButton getCloseButton(){
        if (closeButton==null){
            closeButton = new JButton(GDLEditorLanguageManager.getMessage("Close"));
            closeButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    setVisible(false);
                    dispose();
                }
            });
        }
        return  closeButton;
    }

    public static void main(String[] args){
        new DialogSplash(null, false).setVisible(true);
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
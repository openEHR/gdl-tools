
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

public class DialogSplash extends JDialog implements Observer{

    private static final long serialVersionUID = -2310821412359230220L;
    private JProgressBar jProgressBar = null;
    private JPanel jPanel1 = null;
    private JLabel jLabel2 = null;

    private Future<?> _currentThread = null;
    private int _progressValue = 0;
    private String _description = null;
    private JButton cancelButton;
    private boolean _loading = false;
    private JButton closeButton;

    public DialogSplash(Window owner, boolean loading) {
        super(owner, "", ModalityType.APPLICATION_MODAL);
        _description = "";
        _loading = loading;
        initialize();
    }

    private void initialize() {
        Dimension screenSize =
                Toolkit.getDefaultToolkit().getScreenSize();
        Dimension labelSize = this.getSize();
        this.setUndecorated(true);
        this.setSize(633, 416);
        int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
        int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
        this.setLocation(locx,locy);
        this.setContentPane(getJPanel1());
    }

    public void changeLoadingText(String description){
        _description = description;
        getSplashLabel().setText(_description);
        this.repaint();
        this.validate();
    }

    public void stop(){
        _progressValue = -100;
        _currentThread = null;
        InitialLoadingObservable.getDelegate().deleteObserver(this);
        getCancelButton().setVisible(false);
        this.setVisible(false);
    }

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

    private JProgressBar getJProgressBar() {
        if (jProgressBar == null) {
            jProgressBar = new JProgressBar();
            jProgressBar.setName("jProgressBar");
            jProgressBar.setPreferredSize(new java.awt.Dimension(300,20));
            jProgressBar.setVisible(false);
        }
        return jProgressBar;
    }

    public void update() {
        getSplashLabel().setText(_description);
        _progressValue =  (int)(100*InitialLoadingObservable.getTotalLoadingProgress());
        if (_progressValue>=0){
            getJProgressBar().setValue(_progressValue);
        }
    }

    @Override
    public void update(Observable o, Object arg) {
        _description = GDLLoadingUtility.getCurrentLoadingStageName();
        update();
    }

    private JPanel getJPanel1() {
        if (jPanel1 == null) {
            jPanel1 = new JPanel(new BorderLayout());
            jPanel1.setBackground(Color.WHITE);
            Container container = this.getContentPane();
            container.add(new SplashPanel());
            pack();
            jPanel1.add(container, BorderLayout.CENTER);
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
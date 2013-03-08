/*
 * Creado el 04/01/2010
 */
package se.cambio.cds.openehr.view.dialogs;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;


public abstract class DialogEditor extends JDialog {

    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    private AceptarCambiosAction aceptarCambiosAction = null;
    private CancelarCambiosAction cancelarCambiosAction = null;
    private JPanel jPanel = null; 
    private boolean _respuesta = false;
    private JComponent _componentWithFirstFocus = null;
    private JButton acceptButton = null;
    private JButton cancelButton = null;


    public DialogEditor(Window owner, String title, Dimension size, boolean modal){
	super(owner, title, modal?ModalityType.APPLICATION_MODAL:ModalityType.MODELESS);
	init(size);
    }

    public DialogEditor(Window owner, String titulo, Dimension size, boolean modal, boolean resizable){
	super(owner, titulo, modal?ModalityType.APPLICATION_MODAL:ModalityType.MODELESS);
	init(size);
	this.setResizable(resizable);
    }

    public DialogEditor(Window owner, String titulo, Dimension size){
	super(owner, titulo);
	init(size);
    }

    private void init(Dimension size){
	Dimension screenSize =
		Toolkit.getDefaultToolkit().getScreenSize();
	Dimension labelSize = this.getSize();
	this.setSize(size);
	int locx = (screenSize.width/2) - (labelSize.width/2) - (this.getWidth()/2);
	int locy = (screenSize.height/2) - (labelSize.height/2) - (this.getHeight()/2);
	this.setLocation(locx,locy);
	this.setResizable(false);
	this.addWindowListener(getCancelChangesAction());
	this.setContentPane(getJPanel());
	/* Enter KeyStroke */
	KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
	getJPanel().registerKeyboardAction(getAceptarCambiosAction(), enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
	KeyStroke esc = KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE,0,true);
	getJPanel().registerKeyboardAction(getCancelChangesAction(), esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
    }

    protected JPanel getJPanel() {
	if (jPanel == null) {
	    jPanel = new JPanel();
	}
	return jPanel;
    }

    protected AceptarCambiosAction getAceptarCambiosAction(){
	if (aceptarCambiosAction == null){
	    aceptarCambiosAction = new AceptarCambiosAction();
	}
	return aceptarCambiosAction;
    }

    protected CancelarCambiosAction getCancelChangesAction(){
	if (cancelarCambiosAction == null){
	    cancelarCambiosAction = new CancelarCambiosAction();
	}
	return cancelarCambiosAction;
    }

    protected class CancelarCambiosAction extends WindowAdapter implements ActionListener{

	public void windowOpened(WindowEvent e){
	    if (_componentWithFirstFocus!=null){
		_componentWithFirstFocus.requestFocus();
	    }
	}

	public void actionPerformed(ActionEvent e) {
	    exit();
	}

	public void windowClosing(WindowEvent we) {
	    exit();
	}
    }

    public class AceptarCambiosAction extends AbstractAction{

	/**
	 * Comentario para <code>serialVersionUID</code>
	 */
	private static final long serialVersionUID = -8058749276509227718L;

	public void actionPerformed(ActionEvent e) {
	    accept();
	}
    }

    /**
     * This method initializes jButton	
     * 	
     * @return javax.swing.JButton	
     */    
    protected JButton getAcceptButton() {
	if (acceptButton == null) {
	    acceptButton = new JButton();
	    acceptButton.setText(OpenEHRLanguageManager.getMessage("Accept"));
	    acceptButton.setIcon(ImageUtil.ACCEPT_ICON);
	    acceptButton.setEnabled(true);
	    acceptButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
	    acceptButton.addActionListener(getAceptarCambiosAction());
	}
	return acceptButton;
    }

    /**
     * This method initializes jButton1	
     * 	
     * @return javax.swing.JButton	
     */    
    protected JButton getCancelButton() {
	if (cancelButton == null) {
	    cancelButton = new JButton();
	    cancelButton.setText(OpenEHRLanguageManager.getMessage("Cancel"));
	    cancelButton.setIcon(ImageUtil.CANCEL_ICON);
	    cancelButton.setEnabled(true);
	    cancelButton.addActionListener(getCancelChangesAction());
	}
	return cancelButton;
    }

    public final void accept(){
	if (acceptDialog()){
	    _respuesta = true;
	    setVisible(false);
	}
    }

    public final void exit(){
	if (cancelDialog()){
	    _respuesta = false;
	    setVisible(false);
	}
    }

    public final boolean getAnswer(){
	return  _respuesta;
    }

    protected void registerComponentWithFirstFocus(JComponent componentWithFirstFocus){
	_componentWithFirstFocus = componentWithFirstFocus;
    }

    protected final void setRespuesta(boolean respuesta){
	_respuesta = respuesta;
    }

    protected boolean cancelDialog(){
	return true;
    }

    protected boolean acceptDialog(){
	return true;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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
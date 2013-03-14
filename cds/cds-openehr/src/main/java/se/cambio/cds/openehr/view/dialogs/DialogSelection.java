/*
 * Created on 26-oct-2006
 *


 */
package se.cambio.cds.openehr.view.dialogs;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.openehr.view.panels.SelectionPanel;
import se.cambio.cds.openehr.view.trees.SelectableNode;
import se.cambio.cds.openehr.view.util.NodeConversor;
/**
 * @author icorram
 * 
 * Dialogo que muestra un SeleccionPanel con los botones de aceptar y cancelar.
 * El SeleccionPanel se compone de un arbol de objetos (definido en _nodoRaiz)con 
 * un campo filtro, para limitar los nodos que se muestran.
 *
 */
public class DialogSelection extends DialogEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2770907170844293126L;
	private JButton botonAceptar = null;  //  @jve:decl-index=0:visual-constraint="413,166"
	private JButton botonCancelar = null;  //  @jve:decl-index=0:visual-constraint="599,179"
	private SelectionPanel seleccionPanel = null;
	private SelectableNode<?> _nodoRaiz = null;
	private JPanel botonesPanel = null;

	/**
	 * This is the default constructor
	 */
	public DialogSelection(Window owner, String titulo,SelectableNode<?> nodoRaiz, boolean expandTree, Dimension dimension) {
		super(owner, titulo, dimension, true, true);
		_nodoRaiz = nodoRaiz;
		initialize();
		if (expandTree){
			expandTree();
		}
	}
	public DialogSelection(Window owner, String titulo,SelectableNode<?> nodoRaiz) {
		super(owner, titulo, new Dimension(300, 500), true);
		_nodoRaiz = nodoRaiz;
		initialize();
		expandTree();
	}
	
	/**
	 * This method initializes this
	 */
	private  void initialize() {
	    /* Enter KeyStroke */
		KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
		getJPanel().registerKeyboardAction(null, enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
		NodeConversor.setAllVisible(_nodoRaiz);
		registerComponentWithFirstFocus(getSeleccionPanel().getTextWithCleanButtonPanel().getJTextField());
		GridBagConstraints gbc = new GridBagConstraints();
		getJPanel().setLayout(new GridBagLayout());
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1.0;
		gbc.fill = java.awt.GridBagConstraints.BOTH;
		gbc.gridwidth = 1;
		gbc.weighty = 1;
		getJPanel().add(getSeleccionPanel(), gbc);
		gbc.weighty = 0;
		gbc.gridy++;
		getJPanel().add(getBotonesPanel(), gbc);
	}


	public JPanel getBotonesPanel(){
		if (botonesPanel==null){
			botonesPanel = new JPanel();
			GridBagConstraints gbc = new GridBagConstraints();
			botonesPanel.setLayout(new GridBagLayout());
			gbc.gridwidth = 1;
			gbc.gridx = 0;
			gbc.gridy = 0;
			gbc.fill = GridBagConstraints.NONE;
			gbc.anchor = java.awt.GridBagConstraints.EAST;
			gbc.insets = new java.awt.Insets(3,10,3,5);
			botonesPanel.add(getBotonAceptar(), gbc);
			gbc.anchor = java.awt.GridBagConstraints.WEST;
			gbc.gridx++;
			gbc.insets = new java.awt.Insets(3,5,3,0);
			botonesPanel.add(getBotonCancelar(), gbc);
		}
		return botonesPanel;
	}

	public SelectionPanel getSeleccionPanel(){
		if (seleccionPanel == null){
			seleccionPanel = new SelectionPanel(_nodoRaiz);
			seleccionPanel.getJTree().addExtraMouseListener(new DoubleClickMouseListener());
		}
		return seleccionPanel;
	}

	/**
	 * This method initializes jButton	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	public JButton getBotonAceptar() {
		if (botonAceptar == null) {
			botonAceptar = new JButton();
			botonAceptar.setText(OpenEHRLanguageManager.getMessage("Accept"));
			botonAceptar.setIcon(ImageUtil.ACCEPT_ICON);
			botonAceptar.setEnabled(true);
			botonAceptar.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
			botonAceptar.addActionListener(getAceptarCambiosAction());
		}
		return botonAceptar;
	}


	protected boolean cancelDialog(){
		_nodoRaiz.setAllSeleccionado(Boolean.FALSE);
		return true;
	}

	class DoubleClickMouseListener extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			if(e.getClickCount()>1){
				if (NodeConversor.getSelectedObject(_nodoRaiz, true)!=null){
					accept();
				}
			}
		}
	}

	public void expandTree(){
		getSeleccionPanel().getJTree().expand(_nodoRaiz);
	}

	/**
	 * This method initializes jButton1	
	 * 	
	 * @return javax.swing.JButton	
	 */    
	public JButton getBotonCancelar() {
		if (botonCancelar == null) {
			botonCancelar = new JButton();
			botonCancelar.setText(OpenEHRLanguageManager.getMessage("Cancel"));
			botonCancelar.setIcon(ImageUtil.CANCEL_ICON);
			botonCancelar.setEnabled(true);
			botonCancelar.addActionListener(getCancelChangesAction());
		}
		return botonCancelar;
	}

}  //  @jve:decl-index=0:visual-constraint="124,21"
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
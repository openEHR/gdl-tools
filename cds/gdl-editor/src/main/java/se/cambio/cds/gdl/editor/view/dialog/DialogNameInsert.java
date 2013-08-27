/*
 * Created on 26-oct-2006
 *


 */
package se.cambio.cds.gdl.editor.view.dialog;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Window;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.openehr.view.dialogs.DialogEditor;
/**
 * @author icorram
 *


 */
public class DialogNameInsert extends DialogEditor {

    /**
     * 
     */
    private static final long serialVersionUID = 2562412853124970610L;
    private String _oldValue = null;
    private JTextField valueTextField = null;

    /**
     * This is the default constructor
     */
    public DialogNameInsert(Window owner, String title, String oldValue) {
	super(owner, title, new Dimension(250, 100), true);
	_oldValue = oldValue;
	initialize();
    }
    /**
     * This method initializes this

     */
    private  void initialize() {
	registerComponentWithFirstFocus(getValueTextField());
	getJPanel().setLayout(new BorderLayout());
	JPanel jPanel1 = new JPanel(new FlowLayout(FlowLayout.CENTER));
	jPanel1.add(new JLabel(GDLEditorLanguageManager.getMessage("Name")+":"));
	jPanel1.add(getValueTextField());
	getJPanel().add(jPanel1, BorderLayout.CENTER);
	JPanel jPanel2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
	jPanel2.add(getAcceptButton());
	jPanel2.add(getCancelButton());
	getJPanel().add(jPanel2, BorderLayout.SOUTH);
	setVisible(true);
    }



    public String getValue(){
	return getValueTextField().getText();
    }

    private JTextField getValueTextField(){
	if (valueTextField==null){
	    valueTextField = new JTextField();
	    valueTextField.setPreferredSize(new Dimension(150,18));
	    if (_oldValue!=null){
		valueTextField.setText(_oldValue);
	    }
	}
	return valueTextField;
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
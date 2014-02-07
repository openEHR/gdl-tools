
package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import se.cambio.openehr.util.OpenEHRImageUtil;

public class TerminologyCodesWithButtonPanel extends JPanel{

    private static final long serialVersionUID = 3687161456388975032L;
    private JButton button;
    private JTextField textField;
    
    public TerminologyCodesWithButtonPanel(){
	this.setLayout(new BorderLayout());
	this.add(getTextField(), BorderLayout.CENTER);
	this.add(getSearchButton(), BorderLayout.EAST);
	this.setBackground(getTextField().getBackground());
    }

    public JTextField getTextField(){
	if (textField==null){
	    textField = new JTextField();
	    textField.setBorder(null);
	}
	return textField;
    }
    
    public JButton getSearchButton(){
	if  (button==null){
	    button = new JButton();
	    button.setIcon(OpenEHRImageUtil.SEARCH_ICON);
	    button.setContentAreaFilled(false);
	    button.setBorderPainted(false);
	}
	return button;
    }
}/*
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
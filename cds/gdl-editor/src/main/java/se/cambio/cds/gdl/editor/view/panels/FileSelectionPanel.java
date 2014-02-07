/*
 * Creado el 03/07/2008
 */
package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

import se.cambio.cds.gdl.editor.view.menubar.CleanFileSelectionAction;
import se.cambio.cds.gdl.editor.view.menubar.FileSelectionAction;

public class FileSelectionPanel extends JPanel{

    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 8614448310448465673L;
    private JPanel jPanel;
    private JFileChooser _fileChooser = null;
    private JTextField fileNameJTextField = null;
    private JButton selectFileButton = null;
    private JButton cleanSelectionButton = null;

    public FileSelectionPanel(JFileChooser fileChooser) {
	super();
	_fileChooser = fileChooser;
	initialize();
    }

    private  void initialize() {
	this.setLayout(new BorderLayout());
	this.add(getPanel());
    }


    private JPanel getPanel(){
	if (jPanel==null){
	    jPanel = new JPanel(new GridBagLayout());
	    GridBagConstraints gbc = new GridBagConstraints();
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.weightx = 1.0;
	    gbc.insets = new Insets(5,5,5,5);
	    gbc.fill = java.awt.GridBagConstraints.BOTH;
	    jPanel.add(getFileNameJTextField(),gbc);
	    gbc.gridx++;
	    gbc.weightx = 0;
	    jPanel.add(getSelectFileButton(),gbc);
	    gbc.gridx++;
	    gbc.weightx = 0;
	    jPanel.add(getLimpiarSeleccionButton(),gbc);
	}
	return jPanel;
    }

    public JTextField getFileNameJTextField(){
	if (fileNameJTextField==null){
	    fileNameJTextField = new JTextField();
	    if (_fileChooser.getSelectedFile()!=null){
		String path = getRelativePath(_fileChooser.getSelectedFile().getAbsolutePath());
		fileNameJTextField.setText(path);
	    }
	}
	return fileNameJTextField;
    }

    public JButton getSelectFileButton(){
	if (selectFileButton==null){
	    selectFileButton = new JButton(new FileSelectionAction(_fileChooser, getFileNameJTextField()));
	    selectFileButton.setText("");
	}
	return selectFileButton;
    }

    public JButton getLimpiarSeleccionButton(){
	if (cleanSelectionButton==null){
	    cleanSelectionButton = new JButton(new CleanFileSelectionAction(_fileChooser, getFileNameJTextField()));
	    cleanSelectionButton.setText("");
	}
	return cleanSelectionButton;
    }

    private String getRelativePath(String path){
	String userPath = System.getProperty("user.dir");
	return path.replace(userPath+"\\", "");
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
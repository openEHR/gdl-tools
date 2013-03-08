package se.cambio.cds.gdl.editor.view.dialog;

import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.cds.openehr.util.ImageUtil;

public class DialogGTCodeSelection extends DialogSelection{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private JButton addGTCodeButton;
    private String _gtCodeCreated = null;
    
    public DialogGTCodeSelection(Window owner, GDLEditor controller) {
	super(
		owner, 
		LanguageManager.getMessage("SelectLocalTerm"), 
		NodeDefinitionConversor.getNodeGTCodes(controller.getCurrentTermsMap(), controller.getGTCodesUsedInDefinitions()), 
		true, 
		new Dimension(500,500));
	getSelectionPanel().getFilterPanel().add(getAddGTCodeButton());
    }
    
    private JButton getAddGTCodeButton(){
	if (addGTCodeButton==null){
	    addGTCodeButton = new JButton(LanguageManager.getMessage("AddLocalTerm"));
	    addGTCodeButton.setIcon(ImageUtil.ADD_ICON);
	    addGTCodeButton.addActionListener(new AddGTTermActionListener(this));
	}
	return addGTCodeButton;
    }

    public String getSelectedObject(){
	if (_gtCodeCreated!=null){
	    return _gtCodeCreated;
	}else{
	    return (String)super.getSelectedObject();
	}
    }
    
    private class AddGTTermActionListener implements ActionListener{
	private JDialog _dialog = null;
	public AddGTTermActionListener(JDialog dialog){
	    _dialog = dialog;
	}
	public void actionPerformed(ActionEvent e) {
	    DialogNameInsert dialog = new DialogNameInsert(_dialog, LanguageManager.getMessage("AddLocalTerm"), null);
	    if (dialog.getAnswer()){
		GDLEditor controller = EditorManager.getActiveGDLEditor();
		_gtCodeCreated = controller.createNextGTCode();
        	controller.getCurrentTermDefinition().getTerms().get(_gtCodeCreated).setText(dialog.getValue());
        	accept();
	    }
	}
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
/*
 * Created on 30-ago-2006
 *


 */
package se.cambio.cds.gdl.editor.view.menubar;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;

public class AddLanguageToGuideAction extends AbstractAction {

    /**
     * 
     */
    private static final long serialVersionUID = -3561842193285119707L;

    public AddLanguageToGuideAction(){
	super();
	putValue(NAME, GDLEditorLanguageManager.getMessage("AddLanguageToGuide")+"...");
	putValue(SMALL_ICON, null);
	putValue(SHORT_DESCRIPTION, GDLEditorLanguageManager.getMessage("AddLanguageToGuideSD"));
	putValue(LONG_DESCRIPTION, GDLEditorLanguageManager.getMessage("AddLanguageToGuideD"));
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
	boolean invalidCode = true;
	while (invalidCode){
	    String lang = JOptionPane.showInputDialog(EditorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("EnterNewLanguageCode"));
	    if (lang!=null){
		if (lang.length()==2 && Character.isLetter(lang.charAt(0)) && Character.isLetter(lang.charAt(1))){
		    invalidCode = false;
		    EditorManager.getActiveGDLEditor().changeLanguage(lang);
		}else{
		    JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("InvalidLanguageCode", lang));
		}
	    }else{
		break;
	    }
	}
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
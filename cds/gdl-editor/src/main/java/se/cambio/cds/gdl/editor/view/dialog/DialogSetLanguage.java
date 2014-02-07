
package se.cambio.cds.gdl.editor.view.dialog;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.Languages;
import se.cambio.cds.gdl.editor.view.renderers.LanguageRenderer;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.DialogEditor;
/**
 * @author icorram
 *


 */
public class DialogSetLanguage extends DialogEditor {

    /**
     * 
     */
    private static final long serialVersionUID = 2562412853124970610L;
    private JComboBox _languageSelection = null;

    /**
     * This is the default constructor
     */
    public DialogSetLanguage() {
	super(EditorManager.getActiveEditorWindow(),
		GDLEditorLanguageManager.getMessage("SetEditorLanguage"),
		new Dimension(250, 110),true);
	initialize();
    }

    /**
     * This method initializes this
     */
    private  void initialize() {
	getJPanel().setLayout(new BorderLayout());
	JPanel panelAux = new JPanel(new BorderLayout());
	getJPanel().add(panelAux, BorderLayout.NORTH);
	
	JPanel panelAux1 = new JPanel(new FlowLayout(FlowLayout.CENTER));
	panelAux1.add(new JLabel(GDLEditorLanguageManager.getMessage("SetEditorLanguage")+":"));
	panelAux1.add(getLanguageSelectorComboBox());
	panelAux.add(panelAux1, BorderLayout.NORTH);
	JPanel panelAux2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
	panelAux2.add(getAcceptButton());
	panelAux2.add(getCancelButton());
	getJPanel().add(panelAux2, BorderLayout.SOUTH); 

    }

    private JComboBox getLanguageSelectorComboBox(){
	if (_languageSelection==null){
	    _languageSelection = new JComboBox(Languages.getSupportedLanguages().toArray());
	    _languageSelection.setRenderer(new LanguageRenderer());
	    String lang = UserConfigurationManager.getLanguage();
	    if (Languages.getSupportedLanguages().contains(lang)){
		_languageSelection.setSelectedItem(lang);
	    }
	}
	return _languageSelection;
    }


    protected boolean acceptDialog(){
	String language = (String)getLanguageSelectorComboBox().getSelectedItem();
	UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.LANGUAGE, language);
	UserConfigurationManager.setParameterWithDefault(UserConfigurationManager.COUNTRY, language.toUpperCase());
	JOptionPane.showMessageDialog(EditorManager.getActiveEditorWindow(), GDLEditorLanguageManager.getMessage("MustRestartForChangesToTakeEffect"));
	return UserConfigurationManager.saveConfig();
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
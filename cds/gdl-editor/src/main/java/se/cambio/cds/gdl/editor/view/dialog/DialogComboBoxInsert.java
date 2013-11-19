/*
 * Created on 26-oct-2006
 *


 */
package se.cambio.cds.gdl.editor.view.dialog;

import se.cambio.openehr.view.dialogs.DialogEditor;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;
/**
 * @author icorram
 *


 */
public class DialogComboBoxInsert extends DialogEditor {

    /**
     *
     */
    private static final long serialVersionUID = 2562412853124970610L;
    private JComboBox comboBox = null;

    /**
     * This is the default constructor
     */
    public DialogComboBoxInsert(Window owner, String title, String oldValue, Collection<String> options) {
        super(owner, title, new Dimension(250, 100), true);
        for (String option : options) {
            getComboBox().addItem(option);
        }
        if (oldValue!=null){
            comboBox.setSelectedItem(oldValue);
        }
        initialize();
    }
    /**
     * This method initializes this

     */
    private  void initialize() {
        registerComponentWithFirstFocus(getComboBox());
        getJPanel().setLayout(new BorderLayout());
        JPanel jPanel1 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        jPanel1.add(new JLabel(getTitle()+": "));
        jPanel1.add(getComboBox());
        getJPanel().add(jPanel1, BorderLayout.CENTER);
        JPanel jPanel2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
        jPanel2.add(getAcceptButton());
        jPanel2.add(getCancelButton());
        getJPanel().add(jPanel2, BorderLayout.SOUTH);
    }

    public String getSelectedItem(){
        return getComboBox().getSelectedItem().toString();
    }

    private JComboBox getComboBox(){
        if (comboBox==null){
            comboBox = new JComboBox();
        }
        return comboBox;
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
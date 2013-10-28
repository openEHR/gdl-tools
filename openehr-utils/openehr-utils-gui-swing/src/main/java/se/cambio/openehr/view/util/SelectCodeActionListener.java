package se.cambio.openehr.view.util;

import se.cambio.openehr.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.panels.DVHierarchyCodedTextPanel;
import se.cambio.openehr.view.util.NodeConversor.SearchType;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

public class SelectCodeActionListener implements ActionListener {

    private boolean _enable = true;
    private DVHierarchyCodedTextPanel _panel = null;

    public SelectCodeActionListener(DVHierarchyCodedTextPanel panel){
        _panel = panel;
    }
    @Override
    public void actionPerformed(ActionEvent e) {
        if (_enable){
            DialogSelection dialog =
                    new DialogSelection(null, OpenEHRLanguageManager.getMessage("SelectTerm"), _panel.getRootNode());
            dialog.setVisible(true);
            if (dialog.getAnswer()){
                Collection<Object> objs = NodeConversor.getSelectedObjects(_panel.getRootNode(), SearchType.SEARCH_ONLY_PARENT);
                Collection<CodedTextVO> codedTextVOs = new ArrayList<CodedTextVO>();
                for (Object object : objs) {
                    if (object instanceof CodedTextVO){
                        codedTextVOs.add((CodedTextVO)object);
                    }
                }
                _panel.addCodedTextCollection(codedTextVOs);
            }
        }
    }

    public void setEnable(boolean enable){
        _enable = enable;
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
package se.cambio.openehr.controller.sw;

import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.OpenEHRUtilSwingWorker;
import se.cambio.openehr.view.util.WindowManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.panels.SelectionPanel;
import se.cambio.openehr.view.util.NodeConversor;

import javax.swing.plaf.TreeUI;

public class FilterTreeRSW extends OpenEHRUtilSwingWorker {

    private WindowManager windowManager;
    private SelectionPanel selectionPanel = null;
    private TreeUI treeUI = null;

    public FilterTreeRSW(WindowManager windowManager, SelectionPanel selectionPanel) {
        super();
        this.windowManager = windowManager;
        this.selectionPanel = selectionPanel;
    }

    protected void executeSW() throws InternalErrorException {
        windowManager.setBusy(OpenEHRLanguageManager.getMessage("Loading") + "...");
        treeUI = selectionPanel.getJTree().getUI();
        selectionPanel.getJTree().setUI(null);
        filterNode(selectionPanel);
    }

    public static void filterNode(SelectionPanel selectionPanel) {
        NodeConversor.setAllVisible(selectionPanel.getNode());
        NodeConversor.filterByText(
                selectionPanel.getNode(), selectionPanel.getTextWithCleanButtonPanel().getJTextField().getText());
        selectionPanel.changeRootNode(selectionPanel.getNode());
    }


    protected void done() {
        windowManager.setFree();
        selectionPanel.getJTree().setUI(treeUI);
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
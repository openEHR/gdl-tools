package se.cambio.openehr.controller.sw;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.util.WindowManager;

import java.util.ArrayList;
import java.util.Collection;

public class LoadTerminologyViewerRSW extends OpenEHRUtilSwingWorker {

    private final SelectableNode.SelectionMode selectionMode;
    private TerminologyDialogManager terminologyDialogManager;
    private String _terminologyId = null;
    private WindowManager windowManager;
    private Collection<String> _selectedCodes = null;
    private DialogSelection _dialog = null;
    private TerminologyCodesManager _terminologyCodesManager = null;

    public LoadTerminologyViewerRSW(
            WindowManager windowManager,
            TerminologyCodesManager terminologyCodesManager,
            String terminologyId, Collection<String> selectedCodes,
            SelectableNode.SelectionMode selectionMode,
            TerminologyDialogManager terminologyDialogManager) {
        super();
        this.windowManager = windowManager;
        _selectedCodes = selectedCodes;
        _terminologyId = terminologyId;
        _terminologyCodesManager = terminologyCodesManager;
        this.selectionMode = selectionMode;
        this.terminologyDialogManager = terminologyDialogManager;
        windowManager.setBusy(OpenEHRLanguageManager.getMessage("Loading") + "...");
    }

    protected void executeSW() throws InternalErrorException {
        _dialog = terminologyDialogManager.getTerminologyDialog(windowManager.getMainWindow(), _terminologyId, selectionMode, _selectedCodes);
    }


    protected void done() {
        windowManager.setFree();
        _dialog.setVisible(true);
        if (_dialog.getAnswer()) {
            Collection<String> terminologyCodes = new ArrayList<>();
            for (Object object : _dialog.getSelectedObjects()) {
                if (object instanceof DvCodedText) {
                    terminologyCodes.add(((DvCodedText) object).getCode());
                }
            }
            _terminologyCodesManager.setSelectedTerminologyCodes(terminologyCodes);
            _terminologyCodesManager.update();
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
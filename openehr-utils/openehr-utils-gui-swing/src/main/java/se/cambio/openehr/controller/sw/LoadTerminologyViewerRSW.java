package se.cambio.openehr.controller.sw;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author iago.corbal
 *
 */
public class LoadTerminologyViewerRSW extends OpenEHRUtilSwingWorker {

    private final SelectableNode.SelectionMode selectionMode;
    private final Window owner;
    private String _terminologyId = null;
    private Collection<String> _selectedCodes = null;
    private DialogSelection _dialog = null;
    private TerminologyCodesManager _terminologyCodesManager = null;

    public LoadTerminologyViewerRSW(Window owner, TerminologyCodesManager terminologyCodesManager, String terminologyId, Collection<String> selectedCodes, SelectableNode.SelectionMode selectionMode) {
        super();
        _selectedCodes = selectedCodes;
        _terminologyId = terminologyId;
        _terminologyCodesManager = terminologyCodesManager;
        this.selectionMode = selectionMode;
        this.owner = owner;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                WindowManager.setBusy(OpenEHRLanguageManager.getMessage("Loading") + "...");
            }
        });
    }

    protected void executeSW() throws InternalErrorException {
        try{
            _dialog = TerminologyDialogs.getTerminologyDialog(owner, _terminologyId, selectionMode, _selectedCodes);
        }catch(Exception e){
            ExceptionHandler.handle(e);
        }
    }


    protected void done() {
        WindowManager.setFree();
        _dialog.setVisible(true);
        if (_dialog.getAnswer()){
            Collection<String> terminologyCodes = new ArrayList<String>();
            for (Object object : _dialog.getSelectedObjects()) {
                if (object instanceof DvCodedText){
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
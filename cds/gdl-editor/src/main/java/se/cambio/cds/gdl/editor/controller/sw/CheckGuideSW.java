package se.cambio.cds.gdl.editor.controller.sw;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;

public class CheckGuideSW extends CDSSwingWorker {
    private String _errorMsg = null;
    private GDLEditor _controller = null;
    private Guide _guide = null;
    private String _guideStr = null;
    private boolean _checkOk = false;
    private Runnable _pendingRunnable = null;

    public CheckGuideSW(GDLEditor controller, String guideStr, Runnable pendingRunnable){
        _controller = controller;
        _guideStr = guideStr;
        _pendingRunnable = pendingRunnable;
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException{
        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(_guideStr.getBytes("UTF-8"));
            _guide = _controller.parseGuide(bais);
            if (_guide!=null){
                GuideImporter guideImporter = new GuideImporter(ArchetypeManager.getInstance());
                guideImporter.importGuide(_guide, _controller.getCurrentLanguageCode());
                GuideExportPluginDirectory.compile(_guide);
                _checkOk = true;
            }
        }catch(Exception e){
            ExceptionHandler.handle(e);
            _errorMsg = e.getMessage();
        }
    }

    public String getErrorMsg(){
        return _errorMsg;
    }

    protected void done() {
        _controller.gdlEditingChecked(_guide, _checkOk, _errorMsg, _pendingRunnable);
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
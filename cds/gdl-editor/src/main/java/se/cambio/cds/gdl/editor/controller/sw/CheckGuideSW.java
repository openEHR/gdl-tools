package se.cambio.cds.gdl.editor.controller.sw;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;

public class CheckGuideSW extends CDSSwingWorker{
    private String _errorMsg = null;
    private GDLEditor _controller = null;
    private Guide _guide = null;
    private String _guideStr = null;
    private boolean _checkOk = true;
    private Runnable _pendingRunnable = null;

    public CheckGuideSW(GDLEditor controller, String guideStr, Runnable pendingRunnable){
        _controller = controller;
        _guideStr = guideStr;
        _pendingRunnable = pendingRunnable;
//        SwingUtilities.invokeLater(new Runnable() {
//            @Override
//            public void run() {
//                _controller.setBusy(GDLEditorLanguageManager.getMessage("Compiling"));
//            }
//        });
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException{
        try{
            ByteArrayInputStream bais = new ByteArrayInputStream(_guideStr.getBytes());
            _guide = GuideUtil.parseGuide(bais);
            if (_guide!=null){
                GuideExportPluginDirectory.compile(_guide);
            }
        }catch(Throwable e){
            _errorMsg = e.getMessage();
            Logger.getLogger(CheckGuideSW.class).warn("ERROR parsing/compiling guide '"+_controller.getGuide().getId()+"': "+e.getMessage());
            ExceptionHandler.handle(e);
            _checkOk = false;
        }
    }

    public String getErrorMsg(){
        return _errorMsg;
    }

    protected void done() {
        _controller.gdlEditingChecked(_guide, _checkOk, _pendingRunnable);
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
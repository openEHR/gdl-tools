package se.cambio.cds.gdl.editor.controller.sw;


import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.controller.exportplugins.GuideExportPluginDirectory;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class CompileGuideSW extends CDSSwingWorker {
    private String errorMsg = null;
    private byte[] compiledGuide = null;
    private Guide guide = null;
    private GDLEditor controller = null;

    public CompileGuideSW() {
        controller = EditorManager.getActiveGDLEditor();
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException {
        try {
            guide = controller.getEntity();
            if (guide != null) {
                compiledGuide = GuideExportPluginDirectory.compile(guide);
            }
        } catch (Throwable e) {
            errorMsg = e.getMessage();
            LoggerFactory.getLogger(CompileGuideSW.class).warn("ERROR Compiling guide '" + controller.getEntity().getId() + "': " + e.getMessage());
            ExceptionHandler.handle(e);
        }
    }

    protected byte[] getCompiledGuide() {
        return compiledGuide;
    }

    public Guide getGuide() {
        return guide;
    }

    protected String getErrorMsg() {
        return errorMsg;
    }

    public GDLEditor getController() {
        return controller;
    }

    protected void done() {
        controller.compilationFinished(errorMsg);
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
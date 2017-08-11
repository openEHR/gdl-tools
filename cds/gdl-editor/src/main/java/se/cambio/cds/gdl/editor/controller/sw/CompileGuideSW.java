package se.cambio.cds.gdl.editor.controller.sw;


import lombok.extern.slf4j.Slf4j;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.openehr.util.exceptions.InternalErrorException;

@Slf4j
public class CompileGuideSW extends CDSSwingWorker {
    private String errorMsg = null;
    private byte[] compiledGuide = null;
    private Guide guide = null;
    private GDLEditor gdlEditor;

    protected CompileGuideSW(GDLEditor gdlEditor) {
        this.gdlEditor = gdlEditor;
    }

    @Override
    protected void executeCDSSW() throws InternalErrorException {
        try {
            guide = gdlEditor.getEntity();
            if (guide != null) {
                compiledGuide = gdlEditor.getGuideExportPluginDirectory().compile(guide);
            }
        } catch (Throwable ex) {
            errorMsg = ex.getMessage();
            log.warn("ERROR Compiling guide '" + gdlEditor.getEntity().getId() + "': " + ex.getMessage());
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
        return gdlEditor;
    }

    protected void done() {
        gdlEditor.compilationFinished(errorMsg);
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
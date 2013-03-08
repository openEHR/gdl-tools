package se.cambio.cds.gdl.editor.controller.sw;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.openehr.util.ExceptionHandler;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice.MessageType;
import se.cambio.cds.util.CDSSwingWorker;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.exceptions.InternalErrorException;
import difflib.Delta;
import difflib.DiffUtils;
import difflib.Patch;

/**
 * @author iago.corbal
 *
 */
public class LoadGuideFromFileRSW extends CDSSwingWorker {

    private File _guideFile = null;
    private GDLEditor _editor = null;
    private String _guideStr = null;

    public LoadGuideFromFileRSW() {
	super();
    }

    public LoadGuideFromFileRSW(File guideFile) {
	super();
	_guideFile = guideFile;
    }

    protected void executeCDSSW() throws InternalErrorException {
	try{
	    if (_guideFile==null){
		JFileChooser fileChooser = new JFileChooser(EditorManager.getLastFolderLoaded());
		FileNameExtensionFilter filter = new FileNameExtensionFilter(
			LanguageManager.getMessage("Guide"),new String[]{"gdl"});
		fileChooser.setDialogTitle(LanguageManager.getMessage("LoadGuide"));
		fileChooser.setFileFilter(filter);
		int result = fileChooser.showOpenDialog(EditorManager.getActiveEditorWindow());
		if (result != JFileChooser.CANCEL_OPTION){
		    _guideFile = fileChooser.getSelectedFile();
		}
	    }
	    if (_guideFile!=null){
		FileInputStream fis = new FileInputStream(_guideFile);
		InputStreamReader in = new InputStreamReader(fis, "UTF-8");
		_guideStr = IOUtils.toString(in);
		Guide guide = GDLEditor.parseGuide(new ByteArrayInputStream(_guideStr.getBytes()));
		if (guide!=null){
		    _editor = new GDLEditor(guide);
		}
	    }else{
		this.cancel(true);
	    }
	}catch(Exception e){
	    ExceptionHandler.handle(e);
	}
    }

    public static boolean checkParsedGuide(String guideSrc, Guide guide){
	String guideSrcAux = GDLEditor.serializeGuide(guide);
	if (guide!=null){
	    if (guideSrc.equals(guideSrcAux)){
		return true;
	    }else{
		Patch patch = DiffUtils.diff(stringToLines(guideSrc), stringToLines(guideSrcAux));

		StringBuffer diff = new StringBuffer();

		for (Delta delta : patch.getDeltas()) {
		    diff.append("-------------------------------------\n");
		    diff.append(" line:"+delta.getOriginal().getPosition()+1+"\n");
		    diff.append(" original:"+delta.getOriginal().getLines()+"\n");
		    diff.append(" revised:"+delta.getRevised().getLines()+"\n");
		}
		DialogLongMessageNotice dialog = 
			new DialogLongMessageNotice(
				EditorManager.getActiveEditorWindow(),
				LanguageManager.getMessage("ErrorLoadingGuideT"),
				LanguageManager.getMessage("ErrorLoadingGuide"),
				diff.toString(),
				MessageType.WARNING_WITH_CANCEL
				);
		dialog.setVisible(true);
		boolean result = dialog.getAnswer();
		if (result){
		    return true;
		}else{
		    return false;
		}
	    }
	}else{
	    return false;
	}
    }

    private static List<String> stringToLines(String str) {
	final List<String> lines = new ArrayList<String>();
	for (String string : str.split("\n")) {
	    lines.add(string.trim());
	}
	return lines;
    }

    protected void done() {
	if (_editor!=null){
	    if (checkParsedGuide(_guideStr, _editor.getGuide())){
		EditorManager.setLastFileLoaded(_guideFile);
		EditorManager.setLastFolderLoaded(_guideFile.getParentFile());
		try {
		    EditorManager.initController(_editor);
		} catch (InternalErrorException e) {
		    ExceptionHandler.handle(e);
		}
	    }
	}
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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
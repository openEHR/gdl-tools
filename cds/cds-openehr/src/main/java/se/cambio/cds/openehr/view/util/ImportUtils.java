package se.cambio.cds.openehr.view.util;

import java.awt.Window;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import openEHR.v1.template.TEMPLATE;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.Flattener;
import org.openehr.am.template.OETParser;

import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.model.template.dto.TemplateDTO;
import se.cambio.cds.openehr.model.facade.archetype.delegate.ArchetypeFacadeDelegateFactory;
import se.cambio.cds.openehr.model.facade.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cds.openehr.model.facade.archetype.vo.TemplateObjectBundleCustomVO;
import se.cambio.cds.openehr.util.ExceptionHandler;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.openehr.view.applicationobjects.Templates;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.cds.openehr.view.dialogs.DialogLongMessageNotice.MessageType;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.UnicodeBOMInputStream;

public class ImportUtils {

    public static int showImportArchetypeDialog(Window owner, File selectedFile){
	JFileChooser fileChooser = new JFileChooser();
	FileNameExtensionFilter filter = new FileNameExtensionFilter(
		OpenEHRLanguageManager.getMessage("Archetype"),new String[]{"adl"});
	fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ImportArchetype"));
	fileChooser.setFileFilter(filter);
	if (selectedFile!=null){
	    fileChooser.setSelectedFile(selectedFile);
	}
	int result = fileChooser.showOpenDialog(owner);
	if (result != JFileChooser.CANCEL_OPTION){
	    addArchetype(owner,fileChooser.getSelectedFile());
	}
	return result;
    }

    public static int showImportTemplateDialog(Window owner, File selectedFile){
	JFileChooser fileChooser = new JFileChooser();
	FileNameExtensionFilter filter = new FileNameExtensionFilter(
		OpenEHRLanguageManager.getMessage("Template"),new String[]{"oet"});
	fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ImportTemplate"));
	fileChooser.setFileFilter(filter);
	if (selectedFile!=null){
	    fileChooser.setSelectedFile(selectedFile);
	}
	int result = fileChooser.showOpenDialog(owner);
	if (result != JFileChooser.CANCEL_OPTION){
	    addTemplate(owner, fileChooser.getSelectedFile());
	}
	return result;
    }

    //TODO Should be on a SW
    private static void addArchetype(Window owner, File file){
	String fileName = file.getName().toLowerCase();
	if (fileName.endsWith(".adl")){
	    InputStream fis = null;
	    try{
		fis = new FileInputStream(file.getAbsolutePath());
		UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
		ubis.skipBOM();
		String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
		String idArchetype = ArchetypeFacadeDelegateFactory.getDelegate().getIdArchetype(archetypeSrc);
		/*
		String idArchetypeName = fileName.substring(0,fileName.length()-4);
		if (!idArchetype.toLowerCase().equals(idArchetypeName.toLowerCase())){
		    int answer = JOptionPane.showConfirmDialog(owner, 
			    LanguageManager.getMessage("InsertingTemplateInsteadOfArchetype", fileName),
			    LanguageManager.getMessage("TemplateDetected"), 
			    JOptionPane.YES_NO_CANCEL_OPTION);
		    if (answer==JOptionPane.YES_OPTION){
			addTemplate(owner, file);
		    }
		    return;
		}*/
		ArchetypeDTO archetypeVO = 
			new ArchetypeDTO(idArchetype, idArchetype, idArchetype, null, null, archetypeSrc);
		ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO =
			ArchetypeFacadeDelegateFactory.getDelegate().addArchetype(archetypeVO);
		Archetypes.loadArchetypeObjectBundle(archetypeObjectBundleCustomVO);
	    }catch(Exception e){
		ExceptionHandler.handle(e);
		DialogLongMessageNotice dialog = 
			new DialogLongMessageNotice(
				owner,
				OpenEHRLanguageManager.getMessage("ErrorParsingArchetypeT"),
				OpenEHRLanguageManager.getMessage("ErrorParsingArchetype"),
				e.getMessage(),
				MessageType.ERROR
				);
		dialog.setVisible(true);
	    }finally{
		if (fis!=null){
		    try {
			fis.close();
		    } catch (IOException e) {
			ExceptionHandler.handle(e);
		    }
		}
	    }
	}
    }

    private static void addTemplate(Window owner, File file){
	String fileName = file.getName().toLowerCase();
	InputStream fis = null;
	if (fileName.endsWith(".oet")){
	    try{
		fis = new FileInputStream(file.getAbsolutePath());
		UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
		ubis.skipBOM();
		String idTemplate = fileName.substring(0,fileName.length()-4);
		String archetypeSrc = IOUtils.toString(ubis, "UTF-8");

		/*
  		if (idArchetype.toLowerCase().equals(idTemplate.toLowerCase())){
  		    int answer = JOptionPane.showConfirmDialog(owner, 
  			    LanguageManager.getMessage("InsertingArchetypeInsteadOfTemplate", idArchetype), 
  			    LanguageManager.getMessage("ArchetypeDetected"),
  			    JOptionPane.YES_NO_CANCEL_OPTION);
  		    if (answer==JOptionPane.YES_OPTION){
  			addArchetype(owner, file);
  		    }
  		    return;
  		}
  		if (Archetypes.getArchetypeVO(idArchetype)==null){
  		    JOptionPane.showMessageDialog(owner, 
  			    LanguageManager.getMessage("ArchetypeNotRegisteredTitle"),
  			    LanguageManager.getMessage("ArchetypeNotRegitered"), 
  			    JOptionPane.ERROR_MESSAGE);
  		    return;
  		}*/
		importTemplate(owner, idTemplate, archetypeSrc);
	    }catch(Exception e){
		ExceptionHandler.handle(e);
		DialogLongMessageNotice dialog = 
			new DialogLongMessageNotice(
				owner,
				OpenEHRLanguageManager.getMessage("ErrorParsingTemplateT"),
				OpenEHRLanguageManager.getMessage("ErrorParsingTemplate"),
				e.getMessage(),
				MessageType.ERROR
				);
		dialog.setVisible(true);
	    }finally{
		if (fis!=null){
		    try {
			fis.close();
		    } catch (IOException e) {
			ExceptionHandler.handle(e);
		    }
		}
	    }
	}
    }

    public static TemplateDTO importTemplate(Window owner, String idTemplate, String archetypeSrc) throws Exception{
	TemplateDTO templateVO = 
		new TemplateDTO(idTemplate, null, idTemplate, idTemplate, null, null, archetypeSrc);
	OETParser parser = new OETParser();
	InputStream bis = new ByteArrayInputStream(templateVO.getArchetype().getBytes());
	TEMPLATE template = parser.parseTemplate(bis).getTemplate();
	String idArchetype = template.getDefinition().getArchetypeId();
	ArchetypeDTO archetypeVO = Archetypes.getArchetypeVO(idArchetype);
	if (archetypeVO==null){
	    int result = showImportArchetypeDialog(owner, new File(idArchetype+".adl"));
	    if (result==JFileChooser.CANCEL_OPTION){
		return null;
	    }
	}
	Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
	Archetype ar = new Flattener().toFlattenedArchetype(template, archetypeMap);
	byte[] aomByteArray = IOUtils.getBytes(ar);
	templateVO.setAom(aomByteArray);
	templateVO.setIdArchetype(ar.getArchetypeId().getValue());
	templateVO.setRMName(ar.getArchetypeId().rmEntity());

	TemplateObjectBundleCustomVO templateObjectBundleCustomVO =
		ArchetypeFacadeDelegateFactory.getDelegate().addTemplate(templateVO);
	Templates.loadTemplateObjectBundle(templateObjectBundleCustomVO);
	return templateVO;
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
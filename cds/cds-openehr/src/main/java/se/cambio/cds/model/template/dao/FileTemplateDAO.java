package se.cambio.cds.model.template.dao;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import openEHR.v1.template.TEMPLATE;

import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.Flattener;
import org.openehr.am.template.OETParser;

import se.cambio.cds.model.template.dto.TemplateDTO;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.UserConfigurationManager;
import se.cambio.cds.util.exceptions.FolderNotFoundException;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class FileTemplateDAO implements GenericTemplateDAO{

    public Collection<TemplateDTO> getAllTemplates()
	    throws InternalErrorException{
	File folder = UserConfigurationManager.getTemplateFolder();
	if (!folder.isDirectory()){
	    throw new FolderNotFoundException(folder.getAbsolutePath());
	}
	File[] listOfFiles = folder.listFiles();
	Collection<TemplateDTO> templateVOs = new ArrayList<TemplateDTO>();
	for (int i = 0; i < listOfFiles.length; i++) {
	    if (listOfFiles[i].isFile()) {
		String fileName = listOfFiles[i].getName().toLowerCase();
		try{
		    if (fileName.endsWith(".oet")){
			InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
			String idTemplate = fileName.substring(0,fileName.length()-4);
			OETParser parser = new OETParser();
			TEMPLATE template = parser.parseTemplate(fis).getTemplate();
			Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
			Archetype flattened = new Flattener().toFlattenedArchetype(template, archetypeMap);
			byte[] aomByteArray = IOUtils.getBytes(flattened);
			templateVOs.add(new TemplateDTO(idTemplate, null, idTemplate, idTemplate, null, aomByteArray, null));
		    }
		}catch(Exception e){
		    Logger.getLogger(FileTemplateDAO.class).warn("Problem loading template '"+fileName+"'");
		    ExceptionHandler.handle(e);
		}
	    }
	}
	return templateVOs;
    }



    public TemplateDTO getTemplate(String templateId)
	    throws InternalErrorException, InstanceNotFoundException {
	File folder = UserConfigurationManager.getTemplateFolder();
	if (!folder.isDirectory()){
	    throw new FolderNotFoundException(folder.getAbsolutePath());
	}
	File[] listOfFiles = folder.listFiles();
	for (int i = 0; i < listOfFiles.length; i++) {
	    if (listOfFiles[i].isFile()) {
		String fileName = listOfFiles[i].getName().toLowerCase();
		try{
		    if (fileName.equals(templateId+".oet")){
			InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
			OETParser parser = new OETParser();
			TEMPLATE template = parser.parseTemplate(fis).getTemplate();
			Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
			Archetype flattened = new Flattener().toFlattenedArchetype(template, archetypeMap);
			byte[] aomByteArray = IOUtils.getBytes(flattened);
			return new TemplateDTO(templateId, null, templateId, templateId, null, aomByteArray, null);
		    }
		}catch(Exception e){
		    Logger.getLogger(FileTemplateDAO.class).warn("Problem loading template '"+fileName+"'");
		    ExceptionHandler.handle(e);
		}
	    }
	}
	throw new InstanceNotFoundException(templateId, FileTemplateDAO.class.getName());
    }

    public void addTemplate(TemplateDTO templateVO)
	    throws InternalErrorException, ModelException{
	File folder = UserConfigurationManager.getTemplateFolder();
	if (!folder.isDirectory()){
	    throw new FolderNotFoundException(folder.getAbsolutePath());
	}
	String fileName = templateVO.getIdTemplate()+".oet";
	File newFile = new File(folder.getAbsolutePath()+"/"+fileName);
	Writer output = null;
	try{
	    try {
		output = new BufferedWriter(new FileWriter(newFile));
		output.write(templateVO.getArchetype());
	    } catch (Throwable e) {
		ExceptionHandler.handle(e);
	    }finally{
		if (output!=null){
		    output.close();
		}
	    }
	}catch(IOException e){
	    throw new InternalErrorException(e);
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
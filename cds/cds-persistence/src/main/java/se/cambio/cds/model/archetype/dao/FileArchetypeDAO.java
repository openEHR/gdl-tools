package se.cambio.cds.model.archetype.dao;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.UnicodeBOMInputStream;
import se.cambio.cds.util.UserConfigurationManager;
import se.cambio.cds.util.exceptions.FolderNotFoundException;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class FileArchetypeDAO implements GenericArchetypeDAO{

    public Collection<ArchetypeDTO> getAllArchetypes()
	    throws InternalErrorException{
	File folder = UserConfigurationManager.getArchetypeFolder();
	if (!folder.isDirectory()){
	    throw new FolderNotFoundException(folder.getAbsolutePath());
	}
	File[] listOfFiles = folder.listFiles();
	Collection<ArchetypeDTO> archetypeVOs = new ArrayList<ArchetypeDTO>();
	
	for (int i = 0; i < listOfFiles.length; i++) {
	    if (listOfFiles[i].isFile()) {
		String fileName = listOfFiles[i].getName().toLowerCase();
		if (fileName.endsWith(".adl")){
		    try{
			InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
			UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
			ubis.skipBOM();
			String idArchetype = fileName.substring(0,fileName.length()-4);
			String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
			archetypeVOs.add(new ArchetypeDTO(idArchetype, idArchetype, idArchetype, null, null, archetypeSrc));
		    }catch(Exception e){
			ExceptionHandler.handle(e);
		    }
		}
	    }
	}
	return archetypeVOs;
    }
    
    public ArchetypeDTO getArchetype(String idArchetype)
	    throws InternalErrorException, InstanceNotFoundException {
	File folder = UserConfigurationManager.getArchetypeFolder();
	if (!folder.isDirectory()){
	    throw new FolderNotFoundException(folder.getAbsolutePath());
	}
	File[] listOfFiles = folder.listFiles();
	for (int i = 0; i < listOfFiles.length; i++) {
	    if (listOfFiles[i].isFile()) {
		String fileName = listOfFiles[i].getName().toLowerCase();
		if (fileName.equals(idArchetype+".adl")){
		    try{
			InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
			UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
			ubis.skipBOM();
			String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
			return new ArchetypeDTO(idArchetype, idArchetype, idArchetype, null, null, archetypeSrc);
		    }catch(Exception e){
			ExceptionHandler.handle(e);
		    }
		}
	    }
	}
	throw new InstanceNotFoundException(idArchetype, FileArchetypeDAO.class.getName());
    }
    
    public void addArchetype(ArchetypeDTO archetypeVO)
	    throws InternalErrorException, ModelException{
	File folder = UserConfigurationManager.getArchetypeFolder();
	if (!folder.isDirectory()){
	    throw new FolderNotFoundException(folder.getAbsolutePath());
	}
	String fileName = archetypeVO.getIdArchetype()+".adl";
	File newFile = new File(folder.getAbsolutePath()+"/"+fileName);
	Writer output = null;
	try{
	    try {
		output = new BufferedWriter(new FileWriter(newFile));
		output.write(archetypeVO.getArchetype());
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
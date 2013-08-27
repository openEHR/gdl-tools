package se.cambio.openehr.model.ontology.dao;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.openehr.model.ontology.dto.OntologyDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class FileOntologyDAO implements GenericOntologyDAO{

    public InputStream search(String ontologyId)
	    throws InternalErrorException, InstanceNotFoundException{
	File folder = UserConfigurationManager.getOntologiesFolder();
	if (folder.exists() && folder.isDirectory()){
	    for (File file : folder.listFiles()) {
		if (file.isFile()) {
		    String fileName = file.getName();
		    if (fileName.equals(ontologyId+".owl")){
			try {
			    return new FileInputStream(file);
			} catch (FileNotFoundException e) {
			    throw new InternalErrorException(e);
			}
		    }
		}
	    }
	}else{
	    throw new FolderNotFoundException(folder.toString());
	}
	throw new InstanceNotFoundException(ontologyId, FileOntologyDAO.class.getName());
    }

    public Collection<OntologyDTO> searchAll() throws InternalErrorException {
	try{
	    Collection<OntologyDTO> ontologiesDTO = new ArrayList<OntologyDTO>();
	    File folder = UserConfigurationManager.getOntologiesFolder();
	    if (folder.exists() && folder.isDirectory()){
		for (File file : folder.listFiles()) {
		    if (file.isFile()) {
			String fileName = file.getName();
			if (fileName.endsWith(".owl")){
			    String terminologyId = fileName.substring(0, fileName.length()-4);
			    InputStream is = new FileInputStream(file);
			    byte[] terminologySrc = IOUtils.toByteArray(is);
			    ontologiesDTO.add(new OntologyDTO(terminologyId, terminologySrc));
			}
		    }
		    
		}
	    }else{
		throw new FolderNotFoundException(folder.toString());
	    }
	    return ontologiesDTO;
	}catch(Exception e){
	    throw new InternalErrorException(e);
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
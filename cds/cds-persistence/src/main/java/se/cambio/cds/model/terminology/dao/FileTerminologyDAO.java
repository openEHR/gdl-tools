package se.cambio.cds.model.terminology.dao;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.terminology.dto.TerminologyDTO;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.UserConfigurationManager;
import se.cambio.cds.util.exceptions.FolderNotFoundException;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;

public class FileTerminologyDAO implements GenericTerminologyDAO{

    public InputStream search(String terminologyId)
	    throws InternalErrorException, InstanceNotFoundException{
	
	File terminologyFolder = UserConfigurationManager.getTerminologiesFolder();
	if (terminologyFolder.exists() && terminologyFolder.isDirectory()){
	    for (File file : terminologyFolder.listFiles()) {
		if (file.isFile()) {
		    String fileName = file.getName();
		    if (fileName.equals(terminologyId+".csv")){
			try {
			    return new FileInputStream(file);
			} catch (FileNotFoundException e) {
			    throw new InternalErrorException(e);
			}
		    }
		}
	    }
	}else{
	    throw new FolderNotFoundException(terminologyFolder.toString());
	}
	throw new InstanceNotFoundException(terminologyId, FileTerminologyDAO.class.getName());
    }

    public Collection<TerminologyDTO> searchAll() throws InternalErrorException {
	try{
	    Collection<TerminologyDTO> terminologiesDTO = new ArrayList<TerminologyDTO>();
	    File terminologyFolder = UserConfigurationManager.getTerminologiesFolder();
	    if (terminologyFolder.exists() && terminologyFolder.isDirectory()){
		for (File file : terminologyFolder.listFiles()) {
		    if (file.isFile()) {
			String fileName = file.getName();
			if (fileName.endsWith(".csv")){
			    String terminologyId = fileName.substring(0, fileName.length()-4);
			    InputStream is = new FileInputStream(file);
			    byte[] terminologySrc = IOUtils.toByteArray(is);
			    terminologiesDTO.add(new TerminologyDTO(terminologyId, terminologySrc));
			}
		    }
		}
	    }else{
		throw new FolderNotFoundException(terminologyFolder.toString());
	    }
	    return terminologiesDTO;
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
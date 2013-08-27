package se.cambio.openehr.model.terminology.dao;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class InJarTerminologyDAO implements GenericTerminologyDAO{

    private static String TERMINOLOGY_FOLDER = "Terminologies";

    public InputStream search(String teminologyId)
	    throws InternalErrorException, InstanceNotFoundException{
	String fileName = TERMINOLOGY_FOLDER+"/"+teminologyId+".csv";
	InputStream is = InJarTerminologyDAO.class.getClassLoader().getResourceAsStream(fileName);
	if (is==null) {
	    throw new InstanceNotFoundException(teminologyId, GenericTerminologyDAO.class.getSimpleName());
	}else{
	    return is;
	}
    }

    public Collection<TerminologyDTO> searchAll() 
	    throws InternalErrorException {
	try{
	    InputStream is = InJarTerminologyDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
	    Collection<TerminologyDTO> terminologiesDTO = new ArrayList<TerminologyDTO>();
	    if (is!=null) {
		Collection<String> terminologyFileNames = new ArrayList<String>();
		String resourceList = IOUtils.toString(is, "UTF-8");
		for (String string : resourceList.split("\n")) {
		    string = string.trim();
		    if (string.endsWith(".csv")){
			//Remove the leading '\'
			string = string.replaceAll("\\\\", "/");
			terminologyFileNames.add(string.substring(1, string.length()));
		    }
		}
		for (String terminologyFileName : terminologyFileNames) {
		    try{
			InputStream fis = InJarTerminologyDAO.class.getClassLoader().getResourceAsStream(terminologyFileName);
			String terminilogyId = terminologyFileName.substring(terminologyFileName.lastIndexOf("/")+1,terminologyFileName.length()-4);
			byte[] src = IOUtils.toByteArray(fis);
			terminologiesDTO.add(new TerminologyDTO(terminilogyId, src));
		    }catch(Exception e){
			throw new InternalErrorException(e);
		    }
		}
	    }else{
		throw new Exception("Resource list not found!");
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
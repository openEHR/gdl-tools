package se.cambio.openehr.model.ontology.dao;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.openehr.model.ontology.dto.OntologyDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class InJarOntologyDAO implements GenericOntologyDAO{

    private static String ONTOLOGY_FOLDER = "Ontologies";

    public InputStream search(String teminologyId)
	    throws InternalErrorException, InstanceNotFoundException{
	String fileName = ONTOLOGY_FOLDER+"/"+teminologyId+".owl";
	InputStream is = InJarOntologyDAO.class.getClassLoader().getResourceAsStream(fileName);
	if (is==null) {
	    throw new InstanceNotFoundException(teminologyId, GenericOntologyDAO.class.getSimpleName());
	}else{
	    return is;
	}
    }

    public Collection<OntologyDTO> searchAll() throws InternalErrorException {
	try{
	    InputStream is = InJarOntologyDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
	    Collection<OntologyDTO> ontologiesDTO = new ArrayList<OntologyDTO>();
	    if (is!=null) {
		Collection<String> ontologyFileNames = new ArrayList<String>();
		String resourceList = IOUtils.toString(is, "UTF-8");
		for (String string : resourceList.split("\n")) {
		    string = string.trim();
		    if (string.endsWith(".owl")){
			//Remove the leading '\'
			string = string.replaceAll("\\\\", "/");
			ontologyFileNames.add(string.substring(1, string.length()));
		    }
		}
		for (String ontologyFileName : ontologyFileNames) {
		    try{
			InputStream fis = InJarOntologyDAO.class.getClassLoader().getResourceAsStream(ontologyFileName);
			String terminilogyId = ontologyFileName.substring(ontologyFileName.lastIndexOf("/")+1,ontologyFileName.length()-4);
			byte[] src = IOUtils.toByteArray(fis);
			ontologiesDTO.add(new OntologyDTO(terminilogyId, src));
		    }catch(Exception e){
			throw new InternalErrorException(e);
		    }
		}
	    }else{
		throw new Exception("Resource list not found!");
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
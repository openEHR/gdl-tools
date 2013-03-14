package se.cambio.cds.model.overview.dao;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import se.cambio.cds.model.overview.dto.OverviewDTO;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.Resources;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;

public class InJarOverviewDAO implements GenericOverviewDAO{

    private static String OVERVIEWS_FOLDER = "Overviews";

    public OverviewDTO search(String idOverview) 
	    throws InternalErrorException, InstanceNotFoundException {
	String fileName = OVERVIEWS_FOLDER+"/"+idOverview+".iov";
	InputStream is = InJarOverviewDAO.class.getClassLoader().getResourceAsStream(fileName);
	if (is!=null){
	    try {
		String overviewSrc = IOUtils.toString(is, "UTF-8");
		return new OverviewDTO(idOverview, idOverview, idOverview, overviewSrc);
	    } catch (IOException e) {
		throw new InternalErrorException(e);
	    }
	}
	throw new InstanceNotFoundException(idOverview, OverviewDTO.class.getName());
    }

    public Collection<OverviewDTO> searchAll() throws InternalErrorException {
	Collection<OverviewDTO> overviews = new ArrayList<OverviewDTO>();
	try{
	    InputStream is = InJarOverviewDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
	    if (is!=null) {
		Collection<String> fileNames = new ArrayList<String>();
		String resourceList = IOUtils.toString(is, "UTF-8");
		for (String string : resourceList.split("\n")) {
		    string = string.trim();
		    if (string.endsWith(".iov")){
			//Remove the leading '\'
			string = string.replaceAll("\\\\", "/");
			fileNames.add(string.substring(1, string.length()));
		    }
		}
		for (String fileName : fileNames) {
		    try{
			is =InJarOverviewDAO.class.getClassLoader().getResourceAsStream(fileName);
			if (is!=null){
			    try {
				String overviewSrc = IOUtils.toString(is, "UTF-8");
				overviews.add(new OverviewDTO(fileName, fileName, fileName, overviewSrc));
			    } catch (IOException e) {
				throw new InternalErrorException(e);
			    }
			}
		    }catch(Exception e){
			throw new InternalErrorException(e);
		    }
		}
	    }else{
		throw new InternalErrorException(new Exception("Resource list not found!"));
	    }
	}catch(IOException e){
	    throw new InternalErrorException(e);
	}
	return overviews;
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
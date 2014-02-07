package se.cambio.cds.model.guide.dao;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public class InJarGuideDAO implements GenericGuideDAO{

    private static String GUIDES_FOLDER = "Guides";

    public GuideDTO searchByGuideId(String idGuide) throws InternalErrorException,
    GuideNotFoundException {
	String fileName = GUIDES_FOLDER+"/"+idGuide+".guide";
	InputStream is = InJarGuideDAO.class.getClassLoader().getResourceAsStream(fileName);

	if (is!=null) {
	    try{
		try{
		    ObjectInputStream objectIn =
			    new ObjectInputStream(new BufferedInputStream(is));
		    try{
			GuideDTO guide = (GuideDTO) objectIn.readObject();
			guide.setIdGuide(idGuide);
			return guide;
		    }catch(Exception e){
			ExceptionHandler.handle(e);
		    }finally{
			objectIn.close();
		    }
		    is.close();
		}catch(Exception e){
		    ExceptionHandler.handle(e);
		}
	    }catch(NumberFormatException e){
		ExceptionHandler.handle(e);
	    }
	}
	throw new GuideNotFoundException(idGuide);
    }

    public Collection<GuideDTO> searchAll() throws InternalErrorException {
	Collection<GuideDTO> guides = new ArrayList<GuideDTO>();
	try{
	    InputStream is = InJarGuideDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
	    if (is!=null) {
		Collection<String> guideFileNames = new ArrayList<String>();
		String resourceList = IOUtils.toString(is, "UTF-8");
		for (String string : resourceList.split("\n")) {
		    string = string.trim();
		    if (string.endsWith(".guide")){
			//Remove the leading '\'
			string = string.replaceAll("\\\\", "/");
			guideFileNames.add(string.substring(1, string.length()));
		    }
		}
		for (String guideFileName : guideFileNames) {
		    try{
			is =InJarGuideDAO.class.getClassLoader().getResourceAsStream(guideFileName);
			ObjectInputStream objectIn =
				new ObjectInputStream(is);
			try{
			    GuideDTO guideDTO = (GuideDTO) objectIn.readObject();
			    guides.add(guideDTO);
			}catch(Exception e){
			    ExceptionHandler.handle(e);
			}finally{
			    objectIn.close();
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
	return guides;
    }

    @Override
    public Collection<GuideDTO> searchAllDefinitions() throws InternalErrorException {
        return searchAll();
    }

    public GuideDTO add(GuideDTO guideDTO) throws InternalErrorException{
	return null;
    }

    public void update(GuideDTO guideDTO) throws InternalErrorException,
    GuideNotFoundException {

    }

    public void remove(String idGuide) throws InternalErrorException,
    GuideNotFoundException {
    }

    public Date getLastUpdateDate()
            throws InternalErrorException{
        return null;
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
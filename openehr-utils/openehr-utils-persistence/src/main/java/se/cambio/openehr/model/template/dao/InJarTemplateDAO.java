package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

public class InJarTemplateDAO implements GenericTemplateDAO{

    public InJarTemplateDAO(){
    }

    public Collection<TemplateDTO> searchAll()
	    throws InternalErrorException{
	try{
	    InputStream is = InJarTemplateDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
	    Collection<String> templateFileNames = new ArrayList<String>();
	    Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
	    if (is!=null) {
		String resourceList = IOUtils.toString(is, "UTF-8");
		for (String string : resourceList.split("\n")) {
		    string = string.trim();
		    if (string.endsWith(".oet")){
			//Remove the leading '\'
			string = string.replaceAll("\\\\", "/");
			templateFileNames.add(string.substring(1, string.length()));
		    }
		}
		for (String templateFileName : templateFileNames) {
		    try{
			InputStream fis =InJarTemplateDAO.class.getClassLoader().getResourceAsStream(templateFileName);
			String templateId = templateFileName.substring(templateFileName.lastIndexOf("/")+1,templateFileName.length()-4);
			String archetype = IOUtils.toString(fis);
			templateDTOs.add(new TemplateDTO(templateId,templateId, templateId, null, null, archetype, null, null));
		    }catch(Exception e){
			throw new InternalErrorException(e);
		    }
		}
	    }else{
		throw new Exception("Resource list not found!");
	    }

	    return templateDTOs;
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    @Override
    public Collection<TemplateDTO> searchAllDefinitions() throws InternalErrorException {
        return searchAll();
    }

    public Collection<TemplateDTO> searchByTemplateIds(Collection<String> templateIds)
	    throws InternalErrorException, InstanceNotFoundException {
	try{
	    Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
	    for (String templateId : templateIds) {
		InputStream fis =InJarTemplateDAO.class.getClassLoader().getResourceAsStream(templateId+".oet");
		String archetype = IOUtils.toString(fis);
		templateDTOs.add(new TemplateDTO(templateId,templateId,templateId, null, null, archetype, null, null));
	    }
	    return templateDTOs;
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    public void insert(TemplateDTO templateVO)
	    throws InternalErrorException, ModelException{
	throw new InternalErrorException(new Exception("It's Not possible to add templates into resources!"));
    }

    @Override
    public void update(TemplateDTO templateDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("It's Not possible to add templates into resources!"));
    }

    @Override
    public void remove(String templateId) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("It's Not possible to delete templates from resources!"));
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
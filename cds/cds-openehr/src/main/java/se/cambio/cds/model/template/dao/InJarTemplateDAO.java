package se.cambio.cds.model.template.dao;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import openEHR.v1.template.TEMPLATE;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.Flattener;
import org.openehr.am.template.OETParser;

import se.cambio.cds.model.guide.dao.InJarGuideDAO;
import se.cambio.cds.model.template.dao.GenericTemplateDAO;
import se.cambio.cds.model.template.dto.TemplateDTO;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.Resources;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

public class InJarTemplateDAO implements GenericTemplateDAO{

    public InJarTemplateDAO(){
    }

    public Collection<TemplateDTO> getAllTemplates()
	    throws InternalErrorException{
	try{
	    InputStream is = InJarGuideDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
	    Collection<String> templateFileNames = new ArrayList<String>();
	    Collection<TemplateDTO> templateVOs = new ArrayList<TemplateDTO>();
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
			String idTemplate = templateFileName.substring(templateFileName.lastIndexOf("/")+1,templateFileName.length()-4);
			OETParser parser = new OETParser();
			TEMPLATE template = parser.parseTemplate(fis).getTemplate();
			Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
			Archetype flattened = new Flattener().toFlattenedArchetype(template, archetypeMap);
			byte[] aomByteArray = IOUtils.getBytes(flattened);
			templateVOs.add(new TemplateDTO(idTemplate, null, idTemplate, idTemplate, null, aomByteArray, null));
		    }catch(Exception e){
			throw new InternalErrorException(e);
		    }
		}
	    }else{
		throw new Exception("Resource list not found!");
	    }

	    return templateVOs;
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    public TemplateDTO getTemplate(String templateId)
	    throws InternalErrorException, InstanceNotFoundException {
	try{
	    InputStream fis =InJarTemplateDAO.class.getClassLoader().getResourceAsStream(templateId+".oet");
	    OETParser parser = new OETParser();
	    TEMPLATE template = parser.parseTemplate(fis).getTemplate();
	    Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
	    Archetype flattened = new Flattener().toFlattenedArchetype(template, archetypeMap);
	    byte[] aomByteArray = IOUtils.getBytes(flattened);
	    return new TemplateDTO(templateId, null, templateId, templateId, null, aomByteArray, null);
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    public void addTemplate(TemplateDTO templateVO)
	    throws InternalErrorException, ModelException{
	throw new InternalErrorException(new Exception("It's Not possible to add templates into resources!"));
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
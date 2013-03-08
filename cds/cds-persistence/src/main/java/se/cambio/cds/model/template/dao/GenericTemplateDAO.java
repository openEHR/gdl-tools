package se.cambio.cds.model.template.dao;

import java.util.Collection;

import se.cambio.cds.model.template.dto.TemplateDTO;
import se.cambio.cds.util.exceptions.InstanceNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;


/**
 * @author iago.corbal
 */
public interface GenericTemplateDAO {

    public Collection<TemplateDTO> getAllTemplates()
	    throws InternalErrorException;
    
    public TemplateDTO getTemplate(String templateId)
	    throws InternalErrorException, InstanceNotFoundException;
    
    public void addTemplate(TemplateDTO templateVO)
	    throws InternalErrorException, ModelException;
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
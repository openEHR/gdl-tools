package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.util.Collection;


/**
 * @author iago.corbal
 */
public interface GenericTemplateDAO {

    public Collection<TemplateDTO> searchAll()
	    throws InternalErrorException;

    public Collection<TemplateDTO> searchAllDefinitions()
            throws InternalErrorException;
    
    public Collection<TemplateDTO> searchByTemplateIds(Collection<String> templateIds)
	    throws InternalErrorException, InstanceNotFoundException;
    
    public void insert(TemplateDTO templateDTO)
	    throws InternalErrorException, ModelException;

    public void update(TemplateDTO templateDTO)
            throws InternalErrorException, InstanceNotFoundException;

    public void remove(String templateId)
            throws InternalErrorException, InstanceNotFoundException;
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
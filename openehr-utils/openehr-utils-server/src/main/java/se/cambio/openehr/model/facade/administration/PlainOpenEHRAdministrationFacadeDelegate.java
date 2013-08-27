package se.cambio.openehr.model.facade.administration;

import se.cambio.openehr.model.archetype.dao.GenericArchetypeDAO;
import se.cambio.openehr.model.archetype.dao.GenericArchetypeFactory;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegate;
import se.cambio.openehr.model.template.dao.GenericTemplateDAO;
import se.cambio.openehr.model.template.dao.GenericTemplateFactory;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.util.Collection;

public class PlainOpenEHRAdministrationFacadeDelegate implements OpenEHRAdministrationFacadeDelegate{

    @Override
    public Collection<ArchetypeDTO> searchAllArchetypes()
	    throws InternalErrorException {
	GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
	return dao.searchAll();
    }

    @Override
    public Collection<TemplateDTO> searchAllTemplates()
	    throws InternalErrorException {
	GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
	return dao.searchAll();
    }

    @Override
    public void addArchetype(ArchetypeDTO archetypeDTO)
	    throws InternalErrorException, ModelException {
	GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
	dao.insert(archetypeDTO);
    }

    @Override
    public void addTemplate(TemplateDTO templateDTO)
	    throws InternalErrorException, ModelException {
	GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
	dao.insert(templateDTO);
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
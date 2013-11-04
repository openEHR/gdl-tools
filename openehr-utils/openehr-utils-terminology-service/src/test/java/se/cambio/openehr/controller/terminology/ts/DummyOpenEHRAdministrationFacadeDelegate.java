package se.cambio.openehr.controller.terminology.ts;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegate;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dao.GenericTerminologyDAO;
import se.cambio.openehr.model.terminology.dao.GenericTerminologyFactory;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.util.Collection;

public class DummyOpenEHRAdministrationFacadeDelegate implements OpenEHRAdministrationFacadeDelegate{

    public DummyOpenEHRAdministrationFacadeDelegate(){
    }

    @Override
    public Collection<ArchetypeDTO> searchAllArchetypes()
            throws InternalErrorException {
        return null;
    }

    @Override
    public Collection<TemplateDTO> searchAllTemplates()
            throws InternalErrorException {
        return null;
    }

    @Override
    public Collection<TerminologyDTO> searchAllTerminologies() throws InternalErrorException {
        GenericTerminologyDAO dao = GenericTerminologyFactory.getDAO();
        return dao.searchAll();
    }

    @Override
    public Collection<ArchetypeDTO> searchAllArchetypesDefinitions() throws InternalErrorException {
        return null; //Generated
    }

    @Override
    public Collection<TemplateDTO> searchAllTemplatesDefinitions() throws InternalErrorException {
        return null; //Generated
    }

    @Override
    public int getArchetypesHashCode() throws InternalErrorException {
        return 0; //Generated
    }

    @Override
    public int getTemplatesHashCode() throws InternalErrorException {
        return 0; //Generated
    }

    @Override
    public int getTerminologiesHashCode() throws InternalErrorException {
        return 0; //Generated
    }

    @Override
    public void upsertArchetype(ArchetypeDTO archetypeDTO)
            throws InternalErrorException, ModelException {
    }

    @Override
    public void upsertTemplate(TemplateDTO templateDTO)
            throws InternalErrorException, ModelException {
    }

    @Override
    public void upsertTerminology(TerminologyDTO terminologyDTO) throws InternalErrorException, ModelException {
    }

    @Override
    public void removeArchetype(String archetypeId) throws InternalErrorException, ModelException {
    }

    @Override
    public void removeTemplate(String templateId) throws InternalErrorException, ModelException {
    }

    @Override
    public void removeTerminology(String terminologyId) throws InternalErrorException, ModelException {
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
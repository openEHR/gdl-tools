package se.cambio.openehr.model.facade.administration.plain;

import se.cambio.openehr.model.archetype.dao.GenericArchetypeDAO;
import se.cambio.openehr.model.archetype.dao.GenericArchetypeFactory;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegate;
import se.cambio.openehr.model.template.dao.GenericTemplateDAO;
import se.cambio.openehr.model.template.dao.GenericTemplateFactory;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dao.GenericTerminologyDAO;
import se.cambio.openehr.model.terminology.dao.GenericTerminologyFactory;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.model.util.comparators.ArchetypeComparator;
import se.cambio.openehr.model.util.comparators.TemplateComparator;
import se.cambio.openehr.model.util.comparators.TerminologyComparator;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.util.*;

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
    public Collection<TerminologyDTO> searchAllTerminologies() throws InternalErrorException {
        GenericTerminologyDAO dao = GenericTerminologyFactory.getDAO();
        return dao.searchAll();
    }

    @Override
    public Collection<ArchetypeDTO> searchAllArchetypesDefinitions() throws InternalErrorException {
        GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
        return dao.searchAllDefinitions();
    }

    @Override
    public Collection<TemplateDTO> searchAllTemplatesDefinitions() throws InternalErrorException {
        GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
        return dao.searchAllDefinitions();
    }

    @Override
    public int getArchetypesHashCode() throws InternalErrorException {
        GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
        List<ArchetypeDTO> archetypeDTOs = new ArrayList<ArchetypeDTO>(dao.searchAllDefinitions());
        Collections.sort(archetypeDTOs, new ArchetypeComparator());
        List<String> defs = new ArrayList<String>();
        for (ArchetypeDTO archetypeDTO: archetypeDTOs){
            defs.add(archetypeDTO.getArchetype());
        }
        return defs.hashCode();
    }

    @Override
    public int getTemplatesHashCode() throws InternalErrorException {
        GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
        List<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>(dao.searchAll());
        Collections.sort(templateDTOs, new TemplateComparator());
        List<String> defs = new ArrayList<String>();
        for (TemplateDTO templateDTO: templateDTOs){
            defs.add(templateDTO.getArchetype());
        }
        return defs.hashCode();
    }

    @Override
    public int getTerminologiesHashCode() throws InternalErrorException {
        GenericTerminologyDAO dao = GenericTerminologyFactory.getDAO();
        List<TerminologyDTO> terminologyDTOs = new ArrayList<TerminologyDTO>(dao.searchAll());
        Collections.sort(terminologyDTOs, new TerminologyComparator());
        List<Integer> defs = new ArrayList<Integer>();
        for (TerminologyDTO terminologyDTO: terminologyDTOs){
            defs.add(terminologyDTO.getTerminologyId().hashCode()+ Arrays.hashCode(terminologyDTO.getSrc()));
        }
        return defs.hashCode();
    }

    @Override
    public void upsertArchetype(ArchetypeDTO archetypeDTO)
            throws InternalErrorException, ModelException {
        GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
        try{
            dao.update(archetypeDTO);
        }catch (InstanceNotFoundException e){
            dao.insert(archetypeDTO);
        }
    }

    @Override
    public void upsertTemplate(TemplateDTO templateDTO)
            throws InternalErrorException, ModelException {
        GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
        try{
            dao.update(templateDTO);
        }catch (InstanceNotFoundException e){
            dao.insert(templateDTO);
        }
    }

    @Override
    public void upsertTerminology(TerminologyDTO terminologyDTO) throws InternalErrorException, ModelException {
        GenericTerminologyDAO dao = GenericTerminologyFactory.getDAO();
        try{
            dao.update(terminologyDTO);
        }catch (InstanceNotFoundException e){
            dao.insert(terminologyDTO);

        }
    }

    @Override
    public void removeArchetype(String archetypeId) throws InternalErrorException, ModelException {
        GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
        dao.remove(archetypeId);
    }

    @Override
    public void removeTemplate(String templateId) throws InternalErrorException, ModelException {
        GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
        dao.remove(templateId);
    }

    @Override
    public void removeTerminology(String terminologyId) throws InternalErrorException, ModelException {
        GenericTerminologyDAO dao = GenericTerminologyFactory.getDAO();
        dao.remove(terminologyId);
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
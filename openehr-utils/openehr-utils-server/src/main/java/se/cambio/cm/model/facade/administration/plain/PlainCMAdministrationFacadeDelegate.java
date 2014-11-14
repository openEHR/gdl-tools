package se.cambio.cm.model.facade.administration.plain;

import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.session.data.AbstractCMManager;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.cm.element.dao.GenericCMElementDAO;
import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.util.CMElementDAOFactory;
import se.cambio.openehr.template.generator.controller.TemplateGen;
import se.cambio.openehr.template.generator.model.Template;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class PlainCMAdministrationFacadeDelegate implements CMAdministrationFacadeDelegate {

    @Override
    public Template getSimpleTemplate(String templateId, String lang) throws InternalErrorException, InstanceNotFoundException {
        Collection<TemplateDTO> templateDTOs = searchCMElementsByIds(TemplateDTO.class, Collections.singleton(templateId));
        Template simpleTemplate = null;
        if (!templateDTOs.isEmpty()){
            Collection<ArchetypeDTO> archetypeDTOs = getAllCMElements(ArchetypeDTO.class);
            Map<String, Archetype> archetypeMap = new HashMap<String, Archetype>();
            for(ArchetypeDTO archetypeDTO: archetypeDTOs){
                archetypeMap.put(archetypeDTO.getId(), (Archetype) IOUtils.getObject(archetypeDTO.getAom()));
            }
            TemplateDTO templateDTO = templateDTOs.iterator().next();
            Archetype templateAOM = (Archetype) IOUtils.getObject(templateDTO.getAom());
            TemplateGen templateGen = new TemplateGen();
            simpleTemplate = templateGen.toTemplate(templateDTO.getId(), templateAOM, archetypeMap, lang);
        }
        return simpleTemplate;
    }

    @Override
    public <E extends CMElement> Collection<E> getAllCMElements(Class<E> cmElementClass) throws InternalErrorException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        return dao.searchAll();
    }

    @Override
    public <E extends CMElement> Collection<String> getAllCMElementIds(Class<E> cmElementClass) throws InternalErrorException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        return dao.searchAllIds();
    }

    @Override
    public <E extends CMElement> Collection<E> searchCMElementsByIds(Class<E> cmElementClass, Collection<String> ids) throws InternalErrorException, InstanceNotFoundException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        return dao.searchByIds(ids);
    }

    @Override
    public <E extends CMElement> void upsertCMElement(E cmElement) throws InternalErrorException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElement.getClass());
        try {
            dao.update(cmElement);
        } catch (InstanceNotFoundException e) {
            dao.insert(cmElement);
        }
    }

    @Override
    public <E extends CMElement> void removeCMElement(Class<E> cmElementClass, String id) throws InternalErrorException, InstanceNotFoundException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        dao.remove(id);
    }

    @Override
    public <E extends CMElement> void removeAllCMElements(Class<E> cmElementClass) throws InternalErrorException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        dao.removeAll();
    }

    @Override
    public <E extends CMElement> String getChecksumForCMElements(Class<E> cmElementClass) throws InternalErrorException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        return AbstractCMManager.generateChecksum(dao.searchAll());
    }

    @Override
    public <E extends CMElement> Date getLastUpdate(Class<E> cmElementClass) throws InternalErrorException {
        GenericCMElementDAO<E> dao = CMElementDAOFactory.getInstance().getDAO(cmElementClass);
        return dao.getLastUpdateDate();
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
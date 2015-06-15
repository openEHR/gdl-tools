package se.cambio.openehr.util;

import org.springframework.context.annotation.Profile;
import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.util.CMElementDAOFactory;
import se.cambio.openehr.controller.session.data.AbstractCMManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;

@Profile("cm-admin-dummy-service")
public class DummyCMAdministrationFacadeDelegate implements CMAdministrationFacadeDelegate {

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
/*
 * Created on 02-jun-2006
 *


 */
package se.cambio.openehr.model.facade.administration.ejb;


import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegate;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;
import se.cambio.openehr.util.util.EJBConst;
import se.cambio.openehr.util.util.OpenEHRInitialContext;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.rmi.RemoteException;
import java.util.Collection;

/**
 * @author icorram
 *
 */
public class EJBOpenEHRAdministrationFacadeDelegate implements OpenEHRAdministrationFacadeDelegate {

    private OpenEHRAdministrationFacade _adminFacade;

    public EJBOpenEHRAdministrationFacadeDelegate() throws InternalErrorException {
        try {
            InitialContext ic = OpenEHRInitialContext.getInitialContext();
            _adminFacade = (OpenEHRAdministrationFacade) ic.lookup(getLookupName());
        } catch (NamingException e) {
            throw new InternalErrorException(e);
        }
    }


    @Override
    public Collection<ArchetypeDTO> searchAllArchetypes() throws InternalErrorException {
        try {
            return _adminFacade.searchAllArchetypes();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public Collection<TemplateDTO> searchAllTemplates() throws InternalErrorException {
        try {
            return _adminFacade.searchAllTemplates();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public Collection<TerminologyDTO> searchAllTerminologies() throws InternalErrorException {
        try {
            return _adminFacade.searchAllTerminologies();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public Collection<ArchetypeDTO> searchAllArchetypesDefinitions() throws InternalErrorException {
        try {
            return _adminFacade.searchAllArchetypesDefinitions();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public Collection<TemplateDTO> searchAllTemplatesDefinitions() throws InternalErrorException {
        try {
            return _adminFacade.searchAllTemplatesDefinitions();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public int getArchetypesHashCode() throws InternalErrorException {
        try {
            return _adminFacade.getArchetypesHashCode();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public int getTemplatesHashCode() throws InternalErrorException {
        try {
            return _adminFacade.getTemplatesHashCode();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public int getTerminologiesHashCode() throws InternalErrorException {
        try {
            return _adminFacade.getTerminologiesHashCode();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void upsertArchetype(ArchetypeDTO archetypeDTO) throws InternalErrorException, ModelException {
        try {
            _adminFacade.upsertArchetype(archetypeDTO);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void upsertTemplate(TemplateDTO templateDTO) throws InternalErrorException, ModelException {
        try {
            _adminFacade.upsertTemplate(templateDTO);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void upsertTerminology(TerminologyDTO terminologyDTO) throws InternalErrorException, ModelException {
        try {
            _adminFacade.upsertTerminology(terminologyDTO);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void removeArchetype(String archetypeId) throws InternalErrorException, ModelException {
        try {
            _adminFacade.removeArchetype(archetypeId);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void removeTemplate(String templateId) throws InternalErrorException, ModelException {
        try {
            _adminFacade.removeTemplate(templateId);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void removeTerminology(String terminologyId) throws InternalErrorException, ModelException {
        try {
            _adminFacade.removeTerminology(terminologyId);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }
    private static String getLookupName() {
        String beanName = "OpenEHRAdministrationFacadeBean";
        final String interfaceName = OpenEHRAdministrationFacade.class.getName();
        return "ejb:" + EJBConst.APP_NAME + "/" + EJBConst.MODULE_NAME + "//" +
                beanName + "!" + interfaceName;
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
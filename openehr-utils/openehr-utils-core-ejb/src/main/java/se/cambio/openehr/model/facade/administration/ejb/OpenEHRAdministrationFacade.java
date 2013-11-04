
package se.cambio.openehr.model.facade.administration.ejb;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import javax.ejb.Remote;
import java.rmi.RemoteException;
import java.util.Collection;


/**
 * @author icorram
 *
 */
@Remote
public interface OpenEHRAdministrationFacade{

    public Collection<ArchetypeDTO> searchAllArchetypes()
            throws InternalErrorException, RemoteException;
    public Collection<TemplateDTO> searchAllTemplates()
            throws InternalErrorException, RemoteException;
    public Collection<TerminologyDTO> searchAllTerminologies()
            throws InternalErrorException, RemoteException;

    public Collection<ArchetypeDTO> searchAllArchetypesDefinitions()
            throws InternalErrorException, RemoteException;
    public Collection<TemplateDTO> searchAllTemplatesDefinitions()
            throws InternalErrorException, RemoteException;

    public int getArchetypesHashCode()
            throws InternalErrorException, RemoteException;
    public int getTemplatesHashCode()
            throws InternalErrorException, RemoteException;
    public int getTerminologiesHashCode()
            throws InternalErrorException, RemoteException;

    public void upsertArchetype(ArchetypeDTO archetypeDTO)
            throws InternalErrorException, ModelException, RemoteException;
    public void upsertTemplate(TemplateDTO templateDTO)
            throws InternalErrorException, ModelException, RemoteException;
    public void upsertTerminology(TerminologyDTO terminologyDTO)
            throws InternalErrorException, ModelException, RemoteException;

    public void removeArchetype(String archetypeId)
            throws InternalErrorException, ModelException, RemoteException;
    public void removeTemplate(String templateId)
            throws InternalErrorException, ModelException, RemoteException;
    public void removeTerminology(String terminologyId)
            throws InternalErrorException, ModelException, RemoteException;
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
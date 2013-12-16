package se.cambio.cds.model.facade.ehr.delegate;

import se.cambio.cds.model.facade.ehr.vo.EHREventVO;
import se.cambio.cds.model.facade.ehr.vo.EHRTriggerVO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.InvalidAQLForEHRIdsException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.Map;


/**
 * @author iago.corbal
 *
 */
public interface EHRFacadeDelegate {

    public Map<String,String> getEHRIds(Collection<String> externalEHRIds)
	    throws InternalErrorException, PatientNotFoundException; 

    public Collection<String> queryForEHRIds(String aql) 
	    throws InternalErrorException, InvalidAQLForEHRIdsException;

    public List<List<Object>> query(String aql)
            throws InternalErrorException, InvalidAQLForEHRIdsException;

    public Map<String, Collection<ElementInstance>> queryEHRElements(
	    Collection<String> ehrIds,
	    Collection<ArchetypeReference> archetypeReferences,
        Calendar date)
		    throws InternalErrorException, PatientNotFoundException;    
   
    public boolean storeCDSResults(
            String ehrId,
            Collection<String> guideIds,
            Collection<ArchetypeReference> archetypeReferences)
		    throws InternalErrorException, PatientNotFoundException;
    
    public void upsertEHRTriggerVO(EHRTriggerVO ehrTriggerVO)
	    throws InternalErrorException;

    public EHREventVO getEHREventFromQueueMessage(Object message)
        throws InternalErrorException;
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
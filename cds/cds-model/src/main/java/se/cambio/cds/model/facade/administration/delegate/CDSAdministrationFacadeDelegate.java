package se.cambio.cds.model.facade.administration.delegate;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.overview.dto.OverviewDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;


/**
 * @author iago.corbal
 *
 */
public interface CDSAdministrationFacadeDelegate {

    public Collection<GuideDTO> searchAllGuides() throws InternalErrorException;

    public Collection<GuideDTO> searchAllGuidesDefinitions() throws InternalErrorException;

    public Collection<GuideDTO> searchByGuideIds(Collection<String> guideIds) 
	    throws InternalErrorException, GuideNotFoundException;
    
    public Collection<String> searchInactiveGuideIds() throws InternalErrorException;

    public int getGuidesHashCode() throws InternalErrorException;

    public void setActive(String guideId, boolean active) throws InternalErrorException, GuideNotFoundException;
    
    public void upsertGuide(GuideDTO guideDTO) throws InternalErrorException, GuideNotFoundException;
    
    public void removeGuide(String guideId) throws InternalErrorException, GuideNotFoundException;
    
    public Collection<OverviewDTO> searchAllOverviews() throws InternalErrorException;

    //TODO input argument should be a collection
    public OverviewDTO searchOverview(String overviewId) throws InternalErrorException, InstanceNotFoundException;

    public int getOverviewsHashCode() throws InternalErrorException;

    public void upsertOverview(OverviewDTO overviewDTO) throws InternalErrorException, InstanceNotFoundException;

    public void removeOverview(String overviewId) throws InternalErrorException, InstanceNotFoundException;
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
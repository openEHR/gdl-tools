package se.cambio.cds.model.facade.administration.delegate;

import se.cambio.cds.model.app.dto.CDSAppDTO;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.cds.model.orderset.dto.OrderSetDTO;
import se.cambio.cds.model.study.dto.StudyDTO;
import se.cambio.cds.model.view.dto.DSViewDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;


/**
 * @author iago.corbal
 *
 */
public interface CDSAdministrationFacadeDelegate {

    //Guides
    public Collection<GuideDTO> searchAllGuides() throws InternalErrorException;

    public Collection<GuideDTO> searchAllGuidesDefinitions() throws InternalErrorException;

    public Collection<GuideDTO> searchByGuideIds(Collection<String> guideIds) 
	    throws InternalErrorException, GuideNotFoundException;
    
    public Collection<String> searchInactiveGuideIds() throws InternalErrorException;

    public int getGuidesHashCode() throws InternalErrorException;

    public void setActive(String guideId, boolean active) throws InternalErrorException, GuideNotFoundException;
    
    public void upsertGuide(GuideDTO guideDTO) throws InternalErrorException, GuideNotFoundException;
    
    public void removeGuide(String guideId) throws InternalErrorException, GuideNotFoundException;

    //Views
    public Collection<DSViewDTO> searchAllDSViews() throws InternalErrorException;

    //TODO input argument should be a collection
    public DSViewDTO searchDSView(String dsViewId) throws InternalErrorException, InstanceNotFoundException;

    public int getDSViewsHashCode() throws InternalErrorException;

    public void upsertDSView(DSViewDTO dsViewDTO) throws InternalErrorException, InstanceNotFoundException;

    public void removeDSView(String dsViewId) throws InternalErrorException, InstanceNotFoundException;


    //Studies
    public Collection<StudyDTO> searchAllStudies() throws InternalErrorException;

    //TODO input argument should be a collection
    public StudyDTO searchStudy(String studyId) throws InternalErrorException, InstanceNotFoundException;

    public int getStudiesHashCode() throws InternalErrorException;

    public void upsertStudy(StudyDTO studyDTO) throws InternalErrorException, InstanceNotFoundException;

    public void removeStudy(String studyId) throws InternalErrorException, InstanceNotFoundException;

    //Apps
    public Collection<CDSAppDTO> searchAllCDSApps() throws InternalErrorException;

    public Collection<CDSAppDTO> searchCDSApp(Collection<String> cdsAppId) throws InternalErrorException, InstanceNotFoundException;

    public int getCDSAppHashCode() throws InternalErrorException;

    public void upsertCDSApp(CDSAppDTO cdsAppDTO) throws InternalErrorException, InstanceNotFoundException;

    public void removeCDSApp(String cdsAppId) throws InternalErrorException, InstanceNotFoundException;


    //KBInstances
    public Collection<KBInstanceDTO> searchAllKBInstances() throws InternalErrorException;

    public Collection<KBInstanceDTO> searchKBInstances(Collection<String> kbInstances) throws InternalErrorException, InstanceNotFoundException;

    public int getKBInstanceHashCode() throws InternalErrorException;

    public void upsertKBInstance(KBInstanceDTO kbInstanceDTO) throws InternalErrorException, InstanceNotFoundException;

    public void removeKBInstance(String kbInstanceId) throws InternalErrorException, InstanceNotFoundException;

    //OrderSets
    public Collection<OrderSetDTO> searchAllOrderSets() throws InternalErrorException;

    public Collection<OrderSetDTO> searchOrderSets(Collection<String> kbInstances) throws InternalErrorException, InstanceNotFoundException;

    public int getOrderSetHashCode() throws InternalErrorException;

    public void upsertOrderSet(OrderSetDTO orderSetDTO) throws InternalErrorException, InstanceNotFoundException;

    public void removeOrderSet(String orderSetId) throws InternalErrorException, InstanceNotFoundException;

    //Generic
    public <E extends CMElement>Collection<E> getAllCMElements(Class<E> cmElementClass) throws InternalErrorException;
    public <E extends CMElement>Collection<String> getAllCMElementIds(Class<E> cmElementClass) throws InternalErrorException;
    public <E extends CMElement>Collection<E> searchCMElementsByIds(Class<E> cmElementClass, Collection<String> ids) throws InternalErrorException, InstanceNotFoundException;
    public <E extends CMElement>void upsertCMElement(E cmElement) throws InternalErrorException;
    public <E extends CMElement>void removeCMElement(Class<E> cmElementClass, String id) throws InternalErrorException, InstanceNotFoundException;
    public <E extends CMElement>String getChecksumForCMElements(Class<E> cmElementClass) throws InternalErrorException;
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
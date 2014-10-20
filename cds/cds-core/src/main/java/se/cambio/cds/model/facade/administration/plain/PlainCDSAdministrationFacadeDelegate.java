package se.cambio.cds.model.facade.administration.plain;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.session.data.DecisionSupportViews;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.model.app.dto.CDSAppDTO;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.cds.model.orderset.dto.OrderSetDTO;
import se.cambio.cds.model.study.dto.StudyDTO;
import se.cambio.cds.model.view.dao.GenericDSViewDAO;
import se.cambio.cds.model.view.dao.GenericDSViewFactory;
import se.cambio.cds.model.view.dto.DSViewDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author iago.corbal
 *
 */
public class PlainCDSAdministrationFacadeDelegate implements CDSAdministrationFacadeDelegate{

    private static Logger logger = Logger.getLogger(PlainCDSAdministrationFacadeDelegate.class);

    public PlainCDSAdministrationFacadeDelegate(){
    }

    public Collection<GuideDTO> searchAllGuides() throws InternalErrorException {
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        return dao.searchAll();
    }

    @Override
    public Collection<GuideDTO> searchAllGuidesDefinitions() throws InternalErrorException {
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        return dao.searchAllDefinitions();
    }

    public Collection<GuideDTO> searchByGuideIds(Collection<String> guideIds)
            throws InternalErrorException, GuideNotFoundException{
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        Collection<GuideDTO> guideDTOs =  new ArrayList<GuideDTO>();
        for (String guideId : guideIds) {
            guideDTOs.add(dao.searchByGuideId(guideId));
        }
        return guideDTOs;
    }

    public Collection<String> searchInactiveGuideIds()throws InternalErrorException {
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        Collection<GuideDTO> guideDTOs = dao.searchAll();
        Collection<String> inactiveGuideIds = new ArrayList<String>();
        for (GuideDTO guideDTO : guideDTOs) {
            if (!guideDTO.isActive()){
                inactiveGuideIds.add(guideDTO.getId());
            }
        }
        return inactiveGuideIds;
    }

    @Override
    public int getGuidesHashCode() throws InternalErrorException {
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        return Guides.generateHashCode(dao.searchAllDefinitions());
    }

    public void setActive(String guideId, boolean active)throws InternalErrorException, GuideNotFoundException {
        logger.info("Updating guide to active="+active+". ("+guideId+")");
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        GuideDTO guideDTO = dao.searchByGuideId(guideId);
        guideDTO.setActive(active);
        dao.update(guideDTO);
        updateServerEvents();
    }

    public void upsertGuide(GuideDTO guideDTO) throws InternalErrorException {
        logger.info("Updating guide. ("+guideDTO.getId()+")");
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        try{
            dao.update(guideDTO);
        }catch(GuideNotFoundException e){
            dao.add(guideDTO);
        }
        updateServerEvents();
    }

    public void removeGuide(String guideId) throws InternalErrorException, GuideNotFoundException {
        logger.info("Removing guide. ("+guideId+")");
        GenericGuideDAO dao = GenericGuideFactory.getDAO();
        dao.remove(guideId);
        updateServerEvents();
    }

    public Collection<DSViewDTO> searchAllDSViews() throws InternalErrorException{
        GenericDSViewDAO dao = GenericDSViewFactory.getDAO();
        return dao.searchAll();
    }
    public DSViewDTO searchDSView(String dsViewId) throws InternalErrorException, InstanceNotFoundException{
        GenericDSViewDAO dao = GenericDSViewFactory.getDAO();
        return dao.searchByDSViewId(dsViewId);
    }

    @Override
    public int getDSViewsHashCode() throws InternalErrorException {
        GenericDSViewDAO dao = GenericDSViewFactory.getDAO();
        return DecisionSupportViews.generateHashCode(dao.searchAll());
    }

    @Override
    public void upsertDSView(DSViewDTO dsViewDTO) throws InternalErrorException, InstanceNotFoundException {
        logger.info("Updating view. ("+dsViewDTO.getDsViewId()+")");
        GenericDSViewDAO dao = GenericDSViewFactory.getDAO();
        try{
            dao.update(dsViewDTO);
        }catch(InstanceNotFoundException e){
            dao.insert(dsViewDTO);
        }
    }

    @Override
    public void removeDSView(String dsViewId) throws InternalErrorException, InstanceNotFoundException {
        logger.info("Removing view. ("+dsViewId+")");
        GenericDSViewDAO dao = GenericDSViewFactory.getDAO();
        dao.remove(dsViewId);
    }

    @Override
    public Collection<StudyDTO> searchAllStudies() throws InternalErrorException {
throw new UnsupportedOperationException();
    }

    @Override
    public StudyDTO searchStudy(String studyId) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public int getStudiesHashCode() throws InternalErrorException {
        throw new UnsupportedOperationException();

    }

    @Override
    public void upsertStudy(StudyDTO studyDTO) throws InternalErrorException {
        throw new UnsupportedOperationException();

    }

    @Override
    public void removeStudy(String studyId) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public Collection<CDSAppDTO> searchAllCDSApps() throws InternalErrorException {
        throw new UnsupportedOperationException();

    }

    @Override
    public Collection<CDSAppDTO> searchCDSApp(Collection<String> cdsAppIds) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public int getCDSAppHashCode() throws InternalErrorException {
        throw new UnsupportedOperationException();

    }

    @Override
    public void upsertCDSApp(CDSAppDTO cdsAppDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public void removeCDSApp(String cdsAppId) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public Collection<KBInstanceDTO> searchAllKBInstances() throws InternalErrorException {
        throw new UnsupportedOperationException();

    }

    @Override
    public Collection<KBInstanceDTO> searchKBInstances(Collection<String> kbInstances) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public int getKBInstanceHashCode() throws InternalErrorException {
        throw new UnsupportedOperationException();

    }

    @Override
    public void upsertKBInstance(KBInstanceDTO kbInstanceDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();

    }

    @Override
    public void removeKBInstance(String kbInstanceId) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<OrderSetDTO> searchAllOrderSets() throws InternalErrorException {
        return null; //Generated TODO
    }

    @Override
    public Collection<OrderSetDTO> searchOrderSets(Collection<String> kbInstances) throws InternalErrorException, InstanceNotFoundException {
        return null; //Generated  TODO
    }

    @Override
    public int getOrderSetHashCode() throws InternalErrorException {
        return 0; //Generated TODO
    }

    @Override
    public void upsertOrderSet(OrderSetDTO orderSetDTO) throws InternalErrorException, InstanceNotFoundException {
        //Generated TODO
    }

    @Override
    public void removeOrderSet(String orderSetId) throws InternalErrorException, InstanceNotFoundException {
        //Generated TODO
    }

    private void updateServerEvents() throws InternalErrorException{
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

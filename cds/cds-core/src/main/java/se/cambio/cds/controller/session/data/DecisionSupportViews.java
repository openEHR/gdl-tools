package se.cambio.cds.controller.session.data;
import org.apache.log4j.Logger;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.util.comparators.DSViewComparator;
import se.cambio.cds.model.view.dto.DSViewDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class DecisionSupportViews {
    private static DecisionSupportViews _instance = null;
    private Map<String, DSViewDTO> _dsViewsMap = null;

    private DecisionSupportViews(){
    }

    public void loadDSViews() throws InternalErrorException{
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        Collection<DSViewDTO> DSViewDTOs = adminFD.searchAllDSViews();
        loadDSViews(DSViewDTOs);
    }

    public void loadDSViewsById(Collection<String> dsViewIds) throws InternalErrorException, InstanceNotFoundException {
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        for (String dsViewId : dsViewIds){
            DSViewDTO DSViewDTO = adminFD.searchDSView(dsViewId);   //TODO Load all in one call
            registerDSView(DSViewDTO);
        }
    }

    public void loadDSViews(Collection<DSViewDTO> dsViewDTOs) throws InternalErrorException{
        init();
        for (DSViewDTO dsViewDTO : dsViewDTOs) {
            registerDSView(dsViewDTO);
        }
    }

    public void registerDSView(DSViewDTO DSViewDTO){
        getDSViewsMap().put(DSViewDTO.getDsViewId(), DSViewDTO);
        Logger.getLogger(DecisionSupportViews.class).info("Registering dsv: '" + DSViewDTO.getDsViewId() + "'.");
    }

    public DSViewDTO getDSViewDTO(String dsViewId){
        return getDSViewsMap().get(dsViewId);
    }

    public List<DSViewDTO> getAllDSViews(){
        return new ArrayList<DSViewDTO>(getDSViewsMap().values());
    }

    public Collection<String> getAllDSViewIds(){
        return new ArrayList<String>(getDSViewsMap().keySet());
    }

    public void removeDSView(String dsViewId) throws InternalErrorException{
        getDSViewsMap().remove(dsViewId);
    }

    public static DecisionSupportViews getInstance(){
        if (_instance == null){
            _instance = new DecisionSupportViews();
        }
        return _instance;
    }

    private void init(){
        getDSViewsMap().clear();
    }

    private Map<String, DSViewDTO> getDSViewsMap(){
        if (getInstance()._dsViewsMap ==null){
            getInstance()._dsViewsMap = new HashMap<String, DSViewDTO>();
        }
        return getInstance()._dsViewsMap;
    }

    public int generateHashCode() {
        return generateHashCode(getAllDSViews());
    }

    public static int generateHashCode(Collection<DSViewDTO> dsViewDTOs) {
        List<DSViewDTO> dsViewDTOList = new ArrayList<DSViewDTO>(dsViewDTOs);
        Collections.sort(dsViewDTOList, new DSViewComparator());
        List<String> defs = new ArrayList<String>();
        for(DSViewDTO dsViewDTO : dsViewDTOList){
            defs.add(dsViewDTO.getDSViewSrc());
        }
        return defs.hashCode();
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
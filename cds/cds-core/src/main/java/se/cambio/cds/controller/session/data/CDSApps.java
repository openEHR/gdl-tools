package se.cambio.cds.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.app.dto.CDSAppDTO;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.util.comparators.CDSAppComparator;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class CDSApps {
    private static CDSApps _instance = null;
    private Map<String, CDSAppDTO> _cdsAppMap = null;

    private CDSApps(){
    }

    public void loadApps() throws InternalErrorException{
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        Collection<CDSAppDTO> cdsAppDTOs = adminFD.searchAllCDSApps();
        loadStudies(cdsAppDTOs);
    }

    public void loadStudiesById(Collection<String> cdsAppIds) throws InternalErrorException, InstanceNotFoundException {
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        for (String cdsAppId : cdsAppIds){
            CDSAppDTO cdsAppDTO = adminFD.searchCDSApp(cdsAppId);
            registerApp(cdsAppDTO);
        }
    }

    public void loadStudies(Collection<CDSAppDTO> cdsAppDTOs) throws InternalErrorException{
        init();
        for (CDSAppDTO cdsAppDTO : cdsAppDTOs) {
            registerApp(cdsAppDTO);
        }
    }

    public void registerApp(CDSAppDTO cdsAppDTO){
        getAppsMap().put(cdsAppDTO.getCdaAppId(), cdsAppDTO);
        Logger.getLogger(CDSApps.class).info("Registering app: '"+cdsAppDTO.getCdaAppId()+"'.");
    }

    public CDSAppDTO getCDSAppDTO(String idStudy){
        return getAppsMap().get(idStudy);
    }

    public List<CDSAppDTO> getAllApps(){
        return new ArrayList<CDSAppDTO>(getAppsMap().values());
    }

    public Collection<String> getAllAppIds(){
        return new ArrayList<String>(getAppsMap().keySet());
    }

    public void removeApp(String cdsAppId) throws InternalErrorException{
        getAppsMap().remove(cdsAppId);
    }

    public static CDSApps getInstance(){
        if (_instance == null){
            _instance = new CDSApps();
        }
        return _instance;
    }

    private void init(){
        getAppsMap().clear();
    }

    private Map<String, CDSAppDTO> getAppsMap(){
        if (getInstance()._cdsAppMap ==null){
            getInstance()._cdsAppMap = new HashMap<String, CDSAppDTO>();
        }
        return getInstance()._cdsAppMap;
    }

    public int generateHashCode() {
        return generateHashCode(getAllApps());
    }

    public static int generateHashCode(Collection<CDSAppDTO> cdsAppDTOs){
        List<CDSAppDTO> cdsAppDTOsList = new ArrayList<CDSAppDTO>(cdsAppDTOs);
        Collections.sort(cdsAppDTOsList, new CDSAppComparator());
        List<String> defs = new ArrayList<String>();
        for(CDSAppDTO cdsAppDTO: cdsAppDTOsList){
            defs.add(cdsAppDTO.getAppSrc());
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
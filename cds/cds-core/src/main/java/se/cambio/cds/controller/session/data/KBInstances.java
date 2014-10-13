package se.cambio.cds.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.cds.model.util.comparators.KBInstanceComparator;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class KBInstances {
    private static KBInstances _instance = null;
    private Map<String, KBInstanceDTO> _kbInstancesMap = null;

    private KBInstances(){
    }

    public void loadKBInstances() throws InternalErrorException{
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        Collection<KBInstanceDTO> kbInstanceDTOs = adminFD.searchAllKBInstances();
        loadKBInstances(kbInstanceDTOs);
    }

    public void loadKBInstancesById(Collection<String> kbInstancesIds) throws InternalErrorException, InstanceNotFoundException {
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        Collection<KBInstanceDTO> kbInstanceDTOs = adminFD.searchKBInstances(kbInstancesIds);
        for(KBInstanceDTO kbInstanceDTO: kbInstanceDTOs) {
            registerKBInstance(kbInstanceDTO);
        }
    }

    public void loadKBInstances(Collection<KBInstanceDTO> kbInstanceDTOs) throws InternalErrorException{
        init();
        for (KBInstanceDTO kbInstanceDTO : kbInstanceDTOs) {
            registerKBInstance(kbInstanceDTO);
        }
    }

    public void registerKBInstance(KBInstanceDTO kbInstanceDTO){
        getKBInstancesMap().put(kbInstanceDTO.getKbInstanceId(), kbInstanceDTO);
        Logger.getLogger(KBInstances.class).info("Registering kb instance: '"+kbInstanceDTO.getKbInstanceId()+"'.");
    }

    public KBInstanceDTO getKBInstanceDTO(String kbInstanceId){
        return getKBInstancesMap().get(kbInstanceId);
    }

    public List<KBInstanceDTO> getAllKBInstances(){
        return new ArrayList<KBInstanceDTO>(getKBInstancesMap().values());
    }

    public Collection<String> getAllKBInstanceIds(){
        return new ArrayList<String>(getKBInstancesMap().keySet());
    }

    public void removeKBInstance(String kbInstanceId) throws InternalErrorException{
        getKBInstancesMap().remove(kbInstanceId);
    }

    public static KBInstances getInstance(){
        if (_instance == null){
            _instance = new KBInstances();
        }
        return _instance;
    }

    private void init(){
        getKBInstancesMap().clear();
    }

    private Map<String, KBInstanceDTO> getKBInstancesMap(){
        if (getInstance()._kbInstancesMap ==null){
            getInstance()._kbInstancesMap = new HashMap<String, KBInstanceDTO>();
        }
        return getInstance()._kbInstancesMap;
    }

    public int generateHashCode() {
        return generateHashCode(getAllKBInstances());
    }

    public static int generateHashCode(Collection<KBInstanceDTO> kbInstanceDTOs){
        List<KBInstanceDTO> kbInstanceDTOList = new ArrayList<KBInstanceDTO>(kbInstanceDTOs);
        Collections.sort(kbInstanceDTOList, new KBInstanceComparator());
        List<String> defs = new ArrayList<String>();
        for(KBInstanceDTO kbInstanceDTO: kbInstanceDTOList){
            defs.add(kbInstanceDTO.getKbInstanceSrc());
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
package se.cambio.cds.controller.session.data;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.overview.dto.OverviewDTO;
import se.cambio.cds.model.util.comparators.GuidesComparator;
import se.cambio.cds.model.util.comparators.OverviewComparator;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.util.comparators.ArchetypeComparator;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class Overviews {
    private static Overviews _instance = null;
    private Map<String, OverviewDTO> _overviewsMap = null;

    private Overviews(){
    }

    public static void loadOverviews() throws InternalErrorException{
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        Collection<OverviewDTO> overviewDTOs = adminFD.searchAllOverviews();
        loadOverviews(overviewDTOs);
    }

    public static void loadOverviewsById(Collection<String> overviewIds) throws InternalErrorException, InstanceNotFoundException {
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        for (String overviewId : overviewIds){
            OverviewDTO overviewDTO = adminFD.searchOverview(overviewId);   //TODO Load all in one call
            registerOverview(overviewDTO);
        }
    }



    public static void loadOverviews(Collection<OverviewDTO> overviewDTOs) throws InternalErrorException{
        init();
        for (OverviewDTO overviewDTO : overviewDTOs) {
            registerOverview(overviewDTO);
        }
    }

    public static void registerOverview(OverviewDTO overviewDTO){
        getOverviewsMap().put(overviewDTO.getIdOverview(), overviewDTO);
    }

    public static OverviewDTO getOverviewDTO(String idOverview){
        return getOverviewsMap().get(idOverview);
    }

    public static List<OverviewDTO> getAllOverviews(){
        return new ArrayList<OverviewDTO>(getOverviewsMap().values());
    }

    public static Collection<String> getAllOverviewIds(){
        return new ArrayList<String>(getOverviewsMap().keySet());
    }

    public static void removeOverview(String overviewId) throws InternalErrorException{
        getOverviewsMap().remove(overviewId);
    }

    public static Overviews getDelegate(){
        if (_instance == null){
            _instance = new Overviews();
        }
        return _instance;
    }

    private static void init(){
        getOverviewsMap().clear();
    }

    private static Map<String, OverviewDTO> getOverviewsMap(){
        if (getDelegate()._overviewsMap==null){
            getDelegate()._overviewsMap = new HashMap<String, OverviewDTO>();
        }
        return getDelegate()._overviewsMap;
    }

    public int hashCode(){
        List<OverviewDTO> overviewDTOs = getAllOverviews();
        Collections.sort(overviewDTOs, new OverviewComparator());
        List<String> defs = new ArrayList<String>();
        for(OverviewDTO overviewDTO: overviewDTOs){
            defs.add(overviewDTO.getOverviewSrc());
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
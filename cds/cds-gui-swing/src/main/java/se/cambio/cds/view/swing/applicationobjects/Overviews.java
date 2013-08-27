package se.cambio.cds.view.swing.applicationobjects;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.overview.dto.OverviewDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;


public class Overviews {
    private static Overviews _instance = null;
    private Map<String, OverviewDTO> _overviewsMap = null;

    private Overviews(){
	_overviewsMap = new HashMap<String, OverviewDTO>();
    }

    public static void loadOverviews() throws InternalErrorException{
	CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
	Collection<OverviewDTO> overviewDTOs = adminFD.searchAllOverviews();
	for (OverviewDTO overviewDTO : overviewDTOs) {
	    registerOverview(overviewDTO);
	}
    }

    public static void registerOverview(OverviewDTO overviewDTO){
	getDelegate()._overviewsMap.put(overviewDTO.getIdOverview(), overviewDTO);
    }

    public static OverviewDTO getOverviewDTO(String idOverview){
	return getDelegate()._overviewsMap.get(idOverview);
    }

    public static Collection<OverviewDTO> getAllOverviews(){
	return new ArrayList<OverviewDTO>(getDelegate()._overviewsMap.values());
    }

    public static Collection<String> getAllOverviewIds(){
	return new ArrayList<String>(getDelegate()._overviewsMap.keySet());
    }

    public static Overviews getDelegate(){
	if (_instance == null){
	    _instance = new Overviews();
	}
	return _instance;
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
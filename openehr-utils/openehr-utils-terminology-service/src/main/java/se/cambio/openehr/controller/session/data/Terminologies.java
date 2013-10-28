package se.cambio.openehr.controller.session.data;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegateFactory;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


public class Terminologies {
    private static Terminologies _instance = null;
    private Map<String, TerminologyDTO> _terminologiesById = null;
    public boolean _loaded = false;
    private Terminologies(){
    }

    public static void loadTerminologies() throws InternalErrorException{
        init();
        Collection<TerminologyDTO> terminologyDTOs =
                OpenEHRAdministrationFacadeDelegateFactory.getDelegate().searchAllTerminologies();
        for (TerminologyDTO terminologyDTO: terminologyDTOs){
            registerTerminology(terminologyDTO);
        }
        getDelegate()._loaded = true;
    }

    public static boolean isLoaded(){
        return getDelegate()._loaded;
    }

    private static void init(){
        getDelegate()._terminologiesById = new HashMap<String, TerminologyDTO>();
    }

    public static void registerTerminology(TerminologyDTO terminologyDTO){
        getDelegate()._terminologiesById.put(terminologyDTO.getTerminologyId(), terminologyDTO);
    }

    public static TerminologyDTO getTerminologyDTO(String terminologyId){
        return getDelegate()._terminologiesById.get(terminologyId);
    }

    public static Collection<TerminologyDTO> getAllTerminologies(){
        return new ArrayList<TerminologyDTO>(getDelegate()._terminologiesById.values());
    }


    public static Collection<String> getAllTerminologyIds(){
        return new ArrayList<String>(getDelegate()._terminologiesById.keySet());
    }

    public static Terminologies getDelegate(){
        if (_instance == null){
            _instance = new Terminologies();
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
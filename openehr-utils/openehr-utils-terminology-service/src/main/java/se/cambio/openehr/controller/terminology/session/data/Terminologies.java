package se.cambio.openehr.controller.terminology.session.data;
import org.apache.log4j.Logger;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegateFactory;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.model.util.comparators.TerminologyComparator;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class Terminologies {
    private static Terminologies _instance = null;
    private Map<String, TerminologyDTO> _terminologiesById = null;

    private Terminologies(){
    }

    public static void loadTerminologies() throws InternalErrorException{
        Collection<TerminologyDTO> terminologyDTOs =
                OpenEHRAdministrationFacadeDelegateFactory.getDelegate().searchAllTerminologies();
        loadTerminologies(terminologyDTOs);
    }

    public static void loadTerminologies(Collection<TerminologyDTO> terminologyDTOs) throws InternalErrorException{
        init();
        for (TerminologyDTO terminologyDTO: terminologyDTOs){
            Logger.getLogger(Terminologies.class).info("Registering terminology: '"+terminologyDTO.getTerminologyId()+"'.");
            registerTerminology(terminologyDTO);
        }
    }

    public static boolean isLoaded(){
        return !getTerminologiesMap().isEmpty();
    }

    private static void init(){
        getTerminologiesMap().clear();
    }

    public static void registerTerminology(TerminologyDTO terminologyDTO){
        getTerminologiesMap().put(terminologyDTO.getTerminologyId(), terminologyDTO);
    }

    public static TerminologyDTO getTerminologyDTO(String terminologyId){
        return getTerminologiesMap().get(terminologyId);
    }

    public static List<TerminologyDTO> getAllTerminologies(){
        return new ArrayList<TerminologyDTO>(getTerminologiesMap().values());
    }


    public static List<String> getAllTerminologyIds(){
        return new ArrayList<String>(getTerminologiesMap().keySet());
    }

    public static Map<String, TerminologyDTO> getTerminologiesMap(){
        if (getDelegate()._terminologiesById == null){
            getDelegate()._terminologiesById = new HashMap<String, TerminologyDTO>();
        }
        return getDelegate()._terminologiesById;
    }

    public static void removeTerminology(String terminologyId) throws InternalErrorException{
        getTerminologiesMap().remove(terminologyId);
    }


    public int hashCode(){
        List<TerminologyDTO> terminologyDTOs = getAllTerminologies();
        Collections.sort(terminologyDTOs, new TerminologyComparator());
        List<Integer> defs = new ArrayList<Integer>();
        for (TerminologyDTO terminologyDTO: terminologyDTOs){
            defs.add(terminologyDTO.getTerminologyId().hashCode()+ Arrays.hashCode(terminologyDTO.getSrc()));
        }
        return defs.hashCode();
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
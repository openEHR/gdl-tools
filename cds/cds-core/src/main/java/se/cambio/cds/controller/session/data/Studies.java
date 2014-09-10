package se.cambio.cds.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.study.dto.StudyDTO;
import se.cambio.cds.model.util.comparators.StudyComparator;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class Studies {
    private static Studies _instance = null;
    private Map<String, StudyDTO> _studiesMap = null;

    private Studies(){
    }

    public static void loadStudies() throws InternalErrorException{
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        Collection<StudyDTO> studyDTOs = adminFD.searchAllStudies();
        loadStudies(studyDTOs);
    }

    public static void loadStudiesById(Collection<String> studyIds) throws InternalErrorException, InstanceNotFoundException {
        CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
        for (String studyId : studyIds){
            StudyDTO studyDTO = adminFD.searchStudy(studyId);   //TODO Load all in one call
            registerStudy(studyDTO);
        }
    }

    public static void loadStudies(Collection<StudyDTO> studyDTOs) throws InternalErrorException{
        init();
        for (StudyDTO studyDTO : studyDTOs) {
            registerStudy(studyDTO);
        }
    }

    public static void registerStudy(StudyDTO studyDTO){
        getStudiesMap().put(studyDTO.getStudyId(), studyDTO);
        Logger.getLogger(Studies.class).info("Registering dsv: '"+studyDTO.getStudyId()+"'.");
    }

    public static StudyDTO getStudyDTO(String idStudy){
        return getStudiesMap().get(idStudy);
    }

    public static List<StudyDTO> getAllStudies(){
        return new ArrayList<StudyDTO>(getStudiesMap().values());
    }

    public static Collection<String> getAllStudyIds(){
        return new ArrayList<String>(getStudiesMap().keySet());
    }

    public static void removeStudy(String studyId) throws InternalErrorException{
        getStudiesMap().remove(studyId);
    }

    public static Studies getDelegate(){
        if (_instance == null){
            _instance = new Studies();
        }
        return _instance;
    }

    private static void init(){
        getStudiesMap().clear();
    }

    private static Map<String, StudyDTO> getStudiesMap(){
        if (getDelegate()._studiesMap ==null){
            getDelegate()._studiesMap = new HashMap<String, StudyDTO>();
        }
        return getDelegate()._studiesMap;
    }

    public int hashCode(){
        return generateHashCode(getStudiesMap().values());
    }

    public static int generateHashCode(Collection<StudyDTO> studyDTOs){
        List<StudyDTO> studyDTOsList = new ArrayList<StudyDTO>(studyDTOs);
        Collections.sort(studyDTOsList, new StudyComparator());
        List<String> defs = new ArrayList<String>();
        for(StudyDTO studyDTO: studyDTOsList){
            defs.add(studyDTO.getStudySrc());
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
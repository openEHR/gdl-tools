package se.cambio.cds.model.study.dao;

import se.cambio.cds.model.study.dto.StudyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

public class FileStudyDAO implements GenericStudyDAO{


    public StudyDTO searchByStudyId(String studyId) throws InternalErrorException, InstanceNotFoundException {
        try{
            String fileName = UserConfigurationManager.getStudiesFolder()+File.separator+studyId+".std";
            File file = new File(fileName);
            if (file.exists()){
                InputStream fis = new FileInputStream(file);
                UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                ubis.skipBOM();
                String studySrc = IOUtils.toString(ubis, "UTF-8");
                return new StudyDTO(studyId, studySrc, Calendar.getInstance().getTime());
            }
            throw new InstanceNotFoundException(studyId, StudyDTO.class.getName());
        }catch(IOException e){
            throw new InternalErrorException(e);
        }
    }

    public Collection<StudyDTO> searchAll() throws InternalErrorException {
        Collection<StudyDTO> studies = new ArrayList<StudyDTO>();
        File folder = UserConfigurationManager.getStudiesFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();
                if (fileName.endsWith(".std")){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                        ubis.skipBOM();
                        String studyId = fileName.substring(0,fileName.length()-4);
                        String studyScr = IOUtils.toString(ubis, "UTF-8");
                        studies.add(new StudyDTO(studyId, studyScr, Calendar.getInstance().getTime()));
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return studies;
    }

    @Override
    public void insert(StudyDTO studyDTO) throws InternalErrorException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public void update(StudyDTO studyDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public void remove(String studyId) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public Date getLastUpdateDate() throws InternalErrorException {
        return null;
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
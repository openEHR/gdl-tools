package se.cambio.cds.model.kb.instance.dao;

import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

public class FileKBInstanceDAO implements GenericKBInstanceDAO{


    public Collection<KBInstanceDTO> searchByIds(Collection<String> kbInstanceIds) throws InternalErrorException, InstanceNotFoundException {
        Collection<KBInstanceDTO> kbInstanceDTOs = searchAll();
        Collection<KBInstanceDTO> foundKBInstanceDTOs = new ArrayList<KBInstanceDTO>();
        for(KBInstanceDTO kbInstanceDTO: kbInstanceDTOs){
            if (kbInstanceIds.contains(kbInstanceDTO.getKbInstanceId())){
                foundKBInstanceDTOs.add(kbInstanceDTO);
            }
        }
        //TODO Instance not found exception if ids are not found
        return foundKBInstanceDTOs;
    }

    public Collection<KBInstanceDTO> searchAll() throws InternalErrorException {
        File folder = UserConfigurationManager.getKBInstancesFolder();
        return getKbInstanceDTOs(folder);
    }

    public static Collection<KBInstanceDTO> getKbInstanceDTOs(File folder) throws FolderNotFoundException {
        Collection<KBInstanceDTO> kbInstanceDTOs = new ArrayList<KBInstanceDTO>();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();
                if (fileName.endsWith(".kbi")){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                        ubis.skipBOM();
                        String id = fileName.substring(0,fileName.length()-4);
                        String src = IOUtils.toString(ubis, "UTF-8");
                        KBInstanceDTO kbInstance = new KBInstanceDTO(id);
                        kbInstance.setKbInstanceSrc(src);
                        kbInstance.setLastUpdate(Calendar.getInstance().getTime());
                        kbInstanceDTOs.add(kbInstance);
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return kbInstanceDTOs;
    }

    @Override
    public KBInstanceDTO upsert(KBInstanceDTO kbInstanceDTO) throws InternalErrorException {
        throw new NotImplementedException();
    }

    @Override
    public void remove(String studyId) throws InternalErrorException, InstanceNotFoundException {
        throw new NotImplementedException();
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
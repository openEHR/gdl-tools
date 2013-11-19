package se.cambio.cds.model.overview.dao;

import se.cambio.cds.model.overview.dto.OverviewDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

public class FileOverviewDAO implements GenericOverviewDAO{

    public OverviewDTO searchByOverviewId(String idOverview)
            throws InternalErrorException, InstanceNotFoundException {
        File folder = UserConfigurationManager.getOverviewsFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();
                if (fileName.equals(idOverview+".dsv")){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                        String overviewSrc = IOUtils.toString(fis, "UTF-8");
                        return new OverviewDTO(idOverview, idOverview, idOverview, overviewSrc);
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        throw new InstanceNotFoundException(idOverview, OverviewDTO.class.getName());
    }

    public Collection<OverviewDTO> searchAll() throws InternalErrorException {
        File folder = UserConfigurationManager.getOverviewsFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        Collection<OverviewDTO> overviewDTOs = new ArrayList<OverviewDTO>();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();
                if (fileName.endsWith(".dsv")){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                        String idOverview = fileName.substring(0,fileName.length()-4);
                        String overviewSrc = IOUtils.toString(fis, "UTF-8");
                        overviewDTOs.add(new OverviewDTO(idOverview, idOverview, idOverview, overviewSrc));
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return overviewDTOs;
    }

    @Override
    public void insert(OverviewDTO overviewDTO) throws InternalErrorException {
        //Generated
    }

    @Override
    public void update(OverviewDTO overviewDTO) throws InternalErrorException, InstanceNotFoundException {
        //Generated
    }

    @Override
    public void remove(String overviewId) throws InternalErrorException, InstanceNotFoundException {
        //Generated
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
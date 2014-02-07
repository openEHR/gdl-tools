package se.cambio.cds.model.guide.dao;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

public class FileGuideDAO implements GenericGuideDAO{


    public GuideDTO searchByGuideId(String idGuide) throws InternalErrorException, GuideNotFoundException {
        try{
            String fileName = UserConfigurationManager.getGuidesFolder()+File.separator+idGuide+".gdl";
            File file = new File(fileName);
            if (file.exists()){
                InputStream fis = new FileInputStream(file);
                UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                ubis.skipBOM();
                String guideSrc = IOUtils.toString(ubis, "UTF-8");
                return new GuideDTO(idGuide, guideSrc, null, null, true, Calendar.getInstance().getTime());
            }
            throw new GuideNotFoundException(idGuide);
        }catch(IOException e){
            throw new InternalErrorException(e);
        }
    }

    public Collection<GuideDTO> searchAll() throws InternalErrorException {
        Collection<GuideDTO> guides = new ArrayList<GuideDTO>();
        File folder = UserConfigurationManager.getGuidesFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();
                if (fileName.endsWith(".gdl")){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                        ubis.skipBOM();
                        String idGuide = fileName.substring(0,fileName.length()-4);
                        String guideSrc = IOUtils.toString(ubis, "UTF-8");
                        guides.add(new GuideDTO(idGuide, guideSrc, null,  null, true, Calendar.getInstance().getTime()));
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return guides;
    }

    @Override
    public Collection<GuideDTO> searchAllDefinitions() throws InternalErrorException {
        return searchAll();
    }

    public GuideDTO add(GuideDTO guideDTO) throws InternalErrorException {
        return null;
    }

    public void update(GuideDTO guideDTO) throws InternalErrorException,
            GuideNotFoundException {

    }

    public void remove(String idGuide) throws InternalErrorException,
            GuideNotFoundException {
    }

    public Date getLastUpdateDate()
            throws InternalErrorException{
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
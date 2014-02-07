package se.cambio.openehr.model.template.dao;

import org.apache.log4j.Logger;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;

public class FileTemplateDAO implements GenericTemplateDAO{

    public Collection<TemplateDTO> searchAll()
            throws InternalErrorException{
        File folder = UserConfigurationManager.getTemplateFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        Collection<TemplateDTO> templateVOs = new ArrayList<TemplateDTO>();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName().toLowerCase();
                try{
                    if (fileName.endsWith(".oet")){
                        InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                        String templateId = fileName.substring(0,fileName.length()-4);
                        String archetype = IOUtils.toString(fis);
                        TemplateDTO templateDTO = new TemplateDTO(templateId, templateId, templateId, null, null, archetype, null, null);
                        templateVOs.add(templateDTO);
                    }
                }catch(Throwable e){
                    Logger.getLogger(FileTemplateDAO.class).warn("Problem loading template '"+fileName+"'");
                    ExceptionHandler.handle(e);
                }
            }
        }
        return templateVOs;
    }

    @Override
    public Collection<TemplateDTO> searchAllDefinitions() throws InternalErrorException {
        return searchAll();
    }


    public Collection<TemplateDTO> searchByTemplateIds(Collection<String> templateIds)
            throws InternalErrorException {
        File folder = UserConfigurationManager.getTemplateFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        Collection<String> templateIdsIgnoreCase = new ArrayList<String>();
        for (String templateId : templateIds) {
            templateIdsIgnoreCase.add(templateId.toLowerCase());
        }
        Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String templateId = listOfFiles[i].getName().substring(0, listOfFiles[i].getName().length()-4);//remove '.oet' extension
                try {
                    InputStream fis = new FileInputStream(listOfFiles[i].getAbsolutePath());
                    String archetype = IOUtils.toString(fis);
                    templateDTOs.add(new TemplateDTO(templateId,templateId,templateId, null, null, archetype, null, null));
                } catch (Exception e) {
                    ExceptionHandler.handle(e);
                }

            }
        }
        return templateDTOs;
    }

    public void insert(TemplateDTO templateVO)
            throws InternalErrorException, ModelException{
        File folder = UserConfigurationManager.getTemplateFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        String fileName = templateVO.getIdTemplate()+".oet";
        File newFile = new File(folder.getAbsolutePath()+"/"+fileName);
        Writer output = null;
        try{
            try {
                output = new BufferedWriter(new FileWriter(newFile));
                output.write(templateVO.getArchetype());
            } catch (Throwable e) {
                ExceptionHandler.handle(e);
            }finally{
                if (output!=null){
                    output.close();
                }
            }
        }catch(IOException e){
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void update(TemplateDTO templateDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public void remove(String templateId) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("Not implemented!"));
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
package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;

public class FileArchetypeDAO implements GenericArchetypeDAO{

    public Collection<ArchetypeDTO> searchAll()
            throws InternalErrorException{
        File folder = UserConfigurationManager.getArchetypeFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getPath());
        }
        File[] listOfFiles = folder.listFiles();
        Collection<ArchetypeDTO> archetypeVOs = new ArrayList<ArchetypeDTO>();

        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();//.toLowerCase();
                if (fileName.toLowerCase().endsWith(".adl")){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getPath());
                        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                        ubis.skipBOM();
                        String idArchetype = fileName.substring(0,fileName.length()-4);
                        String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
                        archetypeVOs.add(new ArchetypeDTO(idArchetype, idArchetype, idArchetype, null, archetypeSrc, null, null));
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return archetypeVOs;
    }

    public Collection<ArchetypeDTO> searchByArchetypeIds(Collection<String> archetypeIds)
            throws InternalErrorException {
        File folder = UserConfigurationManager.getArchetypeFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getPath());
        }
        File[] listOfFiles = folder.listFiles();
        Collection<String> archetypeIdsIgnoreCase = new ArrayList<String>();
        for (String archetypeId : archetypeIds) {
            archetypeIdsIgnoreCase.add(archetypeId.toLowerCase());
        }
        Collection<ArchetypeDTO> archetypeDTOs = new ArrayList<ArchetypeDTO>();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String archetypeId = listOfFiles[i].getName().substring(0, listOfFiles[i].getName().length()-4); //remove '.adl' extension
                String archetypeIdIgnoreCase = archetypeId.toLowerCase();
                if (archetypeIdsIgnoreCase.contains(archetypeIdIgnoreCase)){
                    try{
                        InputStream fis = new FileInputStream(listOfFiles[i].getPath());
                        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                        ubis.skipBOM();
                        String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
                        archetypeDTOs.add(new ArchetypeDTO(archetypeId, archetypeId, archetypeId, null, archetypeSrc, null, null));
                    }catch(Exception e){
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return archetypeDTOs;
    }

    @Override
    public Collection<ArchetypeDTO> searchAllDefinitions() throws InternalErrorException {
        return searchAll();
    }

    public void insert(ArchetypeDTO archetypeVO)
            throws InternalErrorException, ModelException{
        File folder = UserConfigurationManager.getArchetypeFolder();
        if (!folder.isDirectory()){
            throw new FolderNotFoundException(folder.getPath());
        }
        String fileName = archetypeVO.getIdArchetype()+".adl";
        File newFile = new File(folder.getPath()+File.separator+fileName);
        Writer output = null;
        try{
            try {
                output = new BufferedWriter(new FileWriter(newFile));
                output.write(archetypeVO.getArchetype());
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
    public void update(ArchetypeDTO ArchetypeDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public void remove(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
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
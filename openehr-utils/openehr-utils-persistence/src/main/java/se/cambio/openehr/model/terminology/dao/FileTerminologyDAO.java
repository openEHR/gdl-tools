package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;

public class FileTerminologyDAO implements GenericTerminologyDAO{


    public Collection<TerminologyDTO> searchByTerminologyIds(Collection<String> terminologyIds)
            throws InternalErrorException{

        Collection<TerminologyDTO> terminologyDTOs = new ArrayList<TerminologyDTO>();
        File terminologyFolder = UserConfigurationManager.getTerminologiesFolder();
        if (terminologyFolder.exists() && terminologyFolder.isDirectory()){
            for (File file : terminologyFolder.listFiles()) {
                if (file.isFile()) {
                    String fileName = file.getName();
                    if (fileName.endsWith(".csv")){
                        String terminologyId = fileName.substring(0, fileName.length()-4);
                        if (terminologyIds.contains(terminologyId)){
                            try {
                                FileInputStream fis = new FileInputStream(file);
                                byte[] src = IOUtils.toByteArray(fis);
                                terminologyDTOs.add(new TerminologyDTO(terminologyId, src));
                            } catch (FileNotFoundException e) {
                                throw new InternalErrorException(e);
                            } catch (IOException e) {
                                throw new InternalErrorException(e);
                            }
                        }
                    }
                }
            }
        }else{
            throw new FolderNotFoundException(terminologyFolder.toString());
        }
        return terminologyDTOs;
    }

    public Collection<TerminologyDTO> searchAll() throws InternalErrorException {
        try{
            Collection<TerminologyDTO> terminologiesDTO = new ArrayList<TerminologyDTO>();
            File terminologyFolder = UserConfigurationManager.getTerminologiesFolder();
            if (terminologyFolder.exists() && terminologyFolder.isDirectory()){
                for (File file : terminologyFolder.listFiles()) {
                    if (file.isFile()) {
                        String fileName = file.getName();
                        if (fileName.endsWith(".csv")){
                            String terminologyId = fileName.substring(0, fileName.length()-4);
                            InputStream is = new FileInputStream(file);
                            byte[] terminologySrc = IOUtils.toByteArray(is);
                            terminologiesDTO.add(new TerminologyDTO(terminologyId, terminologySrc));
                        }
                    }
                }
            }else{
                throw new FolderNotFoundException(terminologyFolder.toString());
            }
            return terminologiesDTO;
        }catch(Exception e){
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void insert(TerminologyDTO TerminologyDTO) throws InternalErrorException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public void update(TerminologyDTO TerminologyDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("Not implemented!"));
    }

    @Override
    public void remove(String terminologyId) throws InternalErrorException, InstanceNotFoundException {
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
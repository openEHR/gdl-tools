package se.cambio.openehr.model.terminology.dao;

import org.apache.log4j.Logger;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

public class ResourceTerminologyDAO implements GenericTerminologyDAO{

    private static String TERMINOLOGY_FOLDER = "terminologies";

    public Collection<TerminologyDTO> searchByTerminologyIds(Collection<String> terminologyIds)
            throws InternalErrorException {

        Collection<TerminologyDTO> terminologyDTOs = new ArrayList<TerminologyDTO>();
        for (String terminologyId: terminologyIds){
            Logger.getLogger(ResourceTerminologyDAO.class).warn("Loading terminology '"+terminologyId+"'.");
            String fileName = TERMINOLOGY_FOLDER+"/"+terminologyId+".csv";
            InputStream is = ResourceTerminologyDAO.class.getClassLoader().getResourceAsStream(fileName);
            try {
                byte[] src = IOUtils.toByteArray(is);
                terminologyDTOs.add(new TerminologyDTO(terminologyId, src));
            } catch (IOException e) {
                throw new InternalErrorException(e);
            }
        }
        return terminologyDTOs;
    }

    public Collection<TerminologyDTO> searchAll()
            throws InternalErrorException {
        try{
            InputStream is = ResourceTerminologyDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
            if (is!=null) {
                Collection<String> terminologyIds = new ArrayList<String>();
                String resourceList = IOUtils.toString(is, "UTF-8");
                for (String string : resourceList.split("\n")) {
                    string = string.trim();
                    if (string.endsWith(".csv")){
                        int i = string.lastIndexOf("\\");
                        terminologyIds.add(string.substring(i+1, string.length()-4));
                    }
                }
                return searchByTerminologyIds(terminologyIds);
            }else{
                throw new Exception("Resource list not found!");
            }
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
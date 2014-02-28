package se.cambio.openehr.model.archetype.dao;

import org.apache.log4j.Logger;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

/**
 * User: Iago.Corbal
 * Date: 2014-02-27
 * Time: 16:20
 */
public class ResourceArchetypeDAO implements GenericArchetypeDAO {

    private static String ARCHETYPES_FOLDER = "archetypes";

    public ResourceArchetypeDAO(){
    }

    public Collection<ArchetypeDTO> searchAll()
            throws InternalErrorException {
        try{
            InputStream is = ResourceArchetypeDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
            if (is!=null) {
                Collection<String> archetypeIds = new ArrayList<String>();
                String resourceList = IOUtils.toString(is, "UTF-8");
                for (String string : resourceList.split("\n")) { //TODO try "\\r?\\n"?
                    string = string.trim();
                    if (string.endsWith(".adl")){
                        int i = string.lastIndexOf("\\");
                        archetypeIds.add(string.substring(i+1, string.length()-4));
                    }
                }
                return searchByArchetypeIds(archetypeIds);
            }else{
                throw new Exception("Resource list not found!");
            }
        }catch(Exception e){
            throw new InternalErrorException(e);
        }
    }

    public Collection<ArchetypeDTO> searchByArchetypeIds(Collection<String> archetypeIds)
            throws InternalErrorException {
        try{
            Collection<ArchetypeDTO> archetypeDTOs = new ArrayList<ArchetypeDTO>();
            for (String archetypeId : archetypeIds) {
                Logger.getLogger(ResourceArchetypeDAO.class).info("Loading archetype "+archetypeId);
                String fileName = ARCHETYPES_FOLDER+"/"+archetypeId+".adl";
                InputStream fis = ResourceArchetypeDAO.class.getClassLoader().getResourceAsStream(fileName);
                UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                ubis.skipBOM();
                String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
                archetypeDTOs.add(new ArchetypeDTO(archetypeId, archetypeId, archetypeId, null, archetypeSrc, null, null));
            }
            return archetypeDTOs;
        }catch(Exception e){
            throw new InternalErrorException(e);
        }
    }

    @Override
    public Collection<ArchetypeDTO> searchAllDefinitions() throws InternalErrorException {
        return searchAll();
    }

    public void insert(ArchetypeDTO archetypeVO)
            throws InternalErrorException, ModelException {
        throw new InternalErrorException(new Exception("It's Not possible to add archetypes into resources!"));
    }

    @Override
    public void update(ArchetypeDTO ArchetypeDTO) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("It's Not possible to update archetypes into resources!"));
    }

    @Override
    public void remove(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
        throw new InternalErrorException(new Exception("It's Not possible to remove archetypes from resources!"));
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

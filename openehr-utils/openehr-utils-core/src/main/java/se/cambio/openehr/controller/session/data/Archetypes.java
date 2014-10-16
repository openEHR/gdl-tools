package se.cambio.openehr.controller.session.data;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.ArchetypeObjectBundleManager;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;


public class Archetypes extends AbstractCMManager<ArchetypeDTO>{
    private static Archetypes instance = null;
    public ImageIcon ICON = OpenEHRImageUtil.ARCHETYPE;
    private ArchetypeObjectBundles archetypeObjectBundles = null;

    public Archetypes(){
    }

    @Override
    public void registerCMElementsInCache(Collection<ArchetypeDTO> cmElements){
        super.registerCMElementsInCache(cmElements);
        try {
            proccessArchetypes(cmElements);
            registerArchetypeDTOs(cmElements);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }

    public void proccessArchetypes(Collection<ArchetypeDTO> archetypeDTOs) throws InternalErrorException {
        for (ArchetypeDTO archetypeDTO: archetypeDTOs){
            new ArchetypeObjectBundleManager(archetypeDTO, getArchetypeMap()).buildArchetypeObjectBundleCustomVO();
        }
    }

    private void registerArchetypeDTOs(Collection<ArchetypeDTO> archetypeDTOs) {
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = getArchetypeObjectBundleCustomVO(archetypeDTO);
            Archetype archetype = getArchetypeAOM(archetypeDTO);
            getArchetypeObjectBundles().registerArchetypeObjectBundle(archetypeObjectBundleCustomVO, archetype);
        }
    }

    public ArchetypeObjectBundles getArchetypeObjectBundles() {
        if (archetypeObjectBundles == null) {
            archetypeObjectBundles = new ArchetypeObjectBundles(this);
        }
        return archetypeObjectBundles;
    }

    public ImageIcon getIcon(String idArchetype) throws InternalErrorException, InstanceNotFoundException {
        ArchetypeDTO archetypeDTO = getCMElementInCache(idArchetype);
        ImageIcon icon = null;
        if (archetypeDTO!=null){
            String entryType = archetypeDTO.getRMName();
            icon = OpenEHRConstUI.getIcon(entryType);
        }else{
            Logger.getLogger(Archetypes.class).warn("Archetype '"+idArchetype+"' was not found loading icon.");
        }
        if (icon!=null){
            return icon;
        }else{
            return ICON;
        }
    }

    public Map<String, Archetype> getArchetypeMap(){
        return new ArchetypeOnDemandMap(this);
    }

    public Collection<Archetype> getArchetypeAOMsById(Collection<String> archetypeIds) throws InternalErrorException, InstanceNotFoundException {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(archetypeIds);
        return getArchetypeAOMsInCache(archetypeDTOs);
    }

    public Collection<Archetype> getArchetypeAOMsInCacheById(Collection<String> archetypeIds) throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementsInCache(archetypeIds);
        return getArchetypeAOMsInCache(archetypeDTOs);
    }


    private Collection<Archetype> getArchetypeAOMsInCache(Collection<ArchetypeDTO> archetypeDTOs) throws InstanceNotFoundException, InternalErrorException {
        Collection<Archetype> archetypes = new ArrayList<Archetype>();
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            archetypes.add(getArchetypeAOM(archetypeDTO));
        }
        return archetypes;
    }

    private static Archetype getArchetypeAOM(ArchetypeDTO archetypeDTO){
        return (Archetype)IOUtils.getObject(archetypeDTO.getAom());
    }

    private static ArchetypeObjectBundleCustomVO getArchetypeObjectBundleCustomVO(ArchetypeDTO archetypeDTO){
        return (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
    }

    public static Archetypes getInstance(){
        if (instance == null){
            instance = new Archetypes();
        }
        return instance;
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
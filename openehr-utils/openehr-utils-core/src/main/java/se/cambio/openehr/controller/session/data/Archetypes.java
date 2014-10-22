package se.cambio.openehr.controller.session.data;
import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.ArchetypeObjectBundleManager;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;


public class Archetypes extends AbstractCMManager<ArchetypeDTO>{
    public static ImageIcon ICON = OpenEHRImageUtil.ARCHETYPE;
    private ArchetypeManager archetypeManager = null;

    public Archetypes(ArchetypeManager archetypeManager){
        this.archetypeManager = archetypeManager;
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
            archetypeManager.registerArchetypeObjectBundle(archetypeObjectBundleCustomVO, archetype);
        }
    }

    public static ImageIcon getIcon(String archetypeId) {
        ImageIcon icon = null;
        String entryType = getEntryType(archetypeId);
        if (entryType!=null) {
            icon = OpenEHRConstUI.getIcon(entryType);
        }
        if (icon!=null){
            return icon;
        }else{
            return ICON;
        }
    }

    public static String getEntryType(final String archetypeId){
        final int i = archetypeId.indexOf('.');
        final int j = archetypeId.substring(0,i).lastIndexOf('-');
        if (j+1<i){
            return archetypeId.substring(j+1,i);
        }
        return null;
    }

    public Map<String, Archetype> getArchetypeMap(){
        return new ArchetypeOnDemandMap(this);
    }

    public Archetype getArchetypeAOMById(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
        return getArchetypeAOMsByIds(Collections.singleton(archetypeId)).iterator().next();
    }

    public Collection<Archetype> getArchetypeAOMsByIds(Collection<String> archetypeIds) throws InternalErrorException, InstanceNotFoundException {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(archetypeIds);
        Collection<Archetype> archetypes = new ArrayList<Archetype>();
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            archetypes.add(getArchetypeAOM(archetypeDTO));
        }
        return archetypes;
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

    private Archetype getArchetypeAOM(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
        return (Archetype)IOUtils.getObject(getCMElement(archetypeId).getAom());
    }

    private static ArchetypeObjectBundleCustomVO getArchetypeObjectBundleCustomVO(ArchetypeDTO archetypeDTO){
        return (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
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
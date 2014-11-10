package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.Archetype;
import org.openehr.jaxb.am.FlatArchetype;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.util.CMTypeFormat;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.ArchetypeObjectBundleManager;
import se.cambio.openehr.util.ArchetypeOnDemandMap;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
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

    @Override
    public Class<ArchetypeDTO> getCMElementClass() {
        return ArchetypeDTO.class;
    }

    public void proccessArchetypes(Collection<ArchetypeDTO> archetypeDTOs) throws InternalErrorException {
        for (ArchetypeDTO archetypeDTO: archetypeDTOs){
            processArchetype(archetypeDTO);
        }
    }

    public void processArchetype(ArchetypeDTO archetypeDTO) throws InternalErrorException {
        new ArchetypeObjectBundleManager(archetypeDTO, archetypeManager).buildArchetypeObjectBundleCustomVO();
    }

    private void registerArchetypeDTOs(Collection<ArchetypeDTO> archetypeDTOs) throws InternalErrorException {
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = getArchetypeObjectBundleCustomVO(archetypeDTO);
            Archetype archetype = null;
            if (archetypeDTO.getFormat().equals(CMTypeFormat.ADL_FORMAT.getFormat())) {   //TODO Add support for ADLS format
                archetype = getArchetypeAOM(archetypeDTO);
            }
            getArchetypeManager().registerArchetypeObjectBundle(archetypeObjectBundleCustomVO, archetype);
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
        if (i<0){
            return null;
        }
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

    public Archetype getArchetypeAOM(ArchetypeDTO archetypeDTO) throws InternalErrorException {
        if (!archetypeDTO.getFormat().equals(CMTypeFormat.ADL_FORMAT.getFormat())){
            throw new InternalErrorException(new Exception("Invalid call for AOM for '" + archetypeDTO.getId() + "' with format '" + archetypeDTO.getFormat() + "'"));
        }
        if (archetypeDTO.getAom() == null){
            processArchetype(archetypeDTO);
        }
        return (Archetype)IOUtils.getObject(archetypeDTO.getAom());
    }

    public FlatArchetype getArchetypeAOM2ById(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(Collections.singleton(archetypeId));
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            return getArchetypeAOM2(archetypeDTO);
        }
        throw new InstanceNotFoundException(archetypeId, ArchetypeDTO.class.getName());
    }


    public FlatArchetype getArchetypeAOM2(ArchetypeDTO archetypeDTO) throws InternalErrorException {
        if (!archetypeDTO.getFormat().equals(CMTypeFormat.ADLS_FORMAT.getFormat())){
            throw new InternalErrorException(new Exception("Invalid call for AOM for '" + archetypeDTO.getId() + "' with format '" + archetypeDTO.getFormat() + "'"));
        }
        if (archetypeDTO.getAom() == null){
            processArchetype(archetypeDTO);
        }
        return (FlatArchetype)IOUtils.getObject(archetypeDTO.getAom());
    }

    public ArchetypeObjectBundleCustomVO getArchetypeAOBCVOById(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(Collections.singleton(archetypeId));
        for(ArchetypeDTO archetypeDTO: archetypeDTOs){
            return getArchetypeAOBCVO(archetypeDTO);
        }
        throw new InstanceNotFoundException(archetypeId, ArchetypeDTO.class.getName());
    }

    public ArchetypeObjectBundleCustomVO getArchetypeAOBCVO(ArchetypeDTO archetypeDTO) throws InternalErrorException {
        if (archetypeDTO.getAobcVO() == null){
            processArchetype(archetypeDTO);
        }
        return (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
    }

    private static ArchetypeObjectBundleCustomVO getArchetypeObjectBundleCustomVO(ArchetypeDTO archetypeDTO){
        return (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
    }

    public ArchetypeManager getArchetypeManager() {
        return archetypeManager;
    }

    public TemplateMap generateTemplateMap(String archetypeId) throws InternalErrorException, InstanceNotFoundException {
        Collection<ArchetypeElementVO> archetypeElementVOs =
                getArchetypeManager().getArchetypeElements().getArchetypeElementsVO(archetypeId, null);
        Map<String, TemplateElementMap> templateElementMaps = new HashMap<String, TemplateElementMap>();
        TemplateMap templateMap = new TemplateMap(archetypeId, null, templateElementMaps);
        Collection<String> elementMapIds = new ArrayList<String>();
        for(ArchetypeElementVO archetypeElementVO: archetypeElementVOs){
            TemplateElementMap templateElementMap = getArchetypeManager().getTemplateElementMap(archetypeElementVO, elementMapIds);
            templateElementMaps.put(templateElementMap.getElementMapId(), templateElementMap);
        }
        return templateMap;
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
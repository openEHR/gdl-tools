package se.cambio.openehr.controller.session.data;

import org.apache.commons.lang.SerializationUtils;
import org.openehr.am.archetype.Archetype;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.TemplateObjectBundleManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.*;


public class Templates extends AbstractCMManager<TemplateDTO>{
    public static ImageIcon ICON = OpenEHRImageUtil.TEMPLATE;
    private ArchetypeManager archetypeManager = null;


    public Templates(ArchetypeManager archetypeManager){
        this.archetypeManager = archetypeManager;
    }

    public Archetypes getArchetypes(){
        return archetypeManager.getArchetypes();
    }

    @Override
    public void registerCMElementsInCache(Collection<TemplateDTO> cmElements){
        super.registerCMElementsInCache(cmElements);
        try {
            proccessTemplates(cmElements);
            registerTemplateDTOs(cmElements);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }

    @Override
    public Class<TemplateDTO> getCMElementClass() {
        return TemplateDTO.class;
    }

    public void proccessTemplates(Collection<TemplateDTO> templateDTOs) throws InternalErrorException {
        for (TemplateDTO templateDTO: templateDTOs){
            processTemplate(templateDTO);
        }
    }

    public void processTemplate(TemplateDTO templateDTO) throws InternalErrorException {
        new TemplateObjectBundleManager(templateDTO, getArchetypes().getArchetypeMap()).buildArchetypeObjectBundleCustomVO();
    }

    private void registerTemplateDTOs(Collection<TemplateDTO> templateDTOs) throws InternalErrorException {
        for(TemplateDTO templateDTO: templateDTOs){
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = getArchetypeObjectBundleCustomVO(templateDTO);
            Archetype archetype = getTemplateAOM(templateDTO);
            getArchetypeManager().registerArchetypeObjectBundle(archetypeObjectBundleCustomVO, archetype);
        }
    }

    public ImageIcon getIcon(String idTemplate) throws InternalErrorException, InstanceNotFoundException {
        return ICON;
    }

    private static ArchetypeObjectBundleCustomVO getArchetypeObjectBundleCustomVO(TemplateDTO templateDTO){
        return (ArchetypeObjectBundleCustomVO)SerializationUtils.deserialize(templateDTO.getAobcVO());
    }

    public Archetype getTemplateAOMById(String templateId) throws InternalErrorException, InstanceNotFoundException {
        return getTemplatesAOMsByIds(Collections.singleton(templateId)).iterator().next();
    }

    public Collection<Archetype> getTemplatesAOMsByIds(Collection<String> templateIds) throws InternalErrorException, InstanceNotFoundException {
        Collection<TemplateDTO> templateDTOs = getCMElementByIds(templateIds);
        Collection<Archetype> archetypes = new ArrayList<>();
        for(TemplateDTO templateDTO: templateDTOs){
            archetypes.add(getTemplateAOM(templateDTO));
        }
        return archetypes;
    }

    public Archetype getTemplateAOM(TemplateDTO templateDTO) throws InternalErrorException {
        if (templateDTO.getAom() == null){
            processTemplate(templateDTO);
        }
        return (Archetype) SerializationUtils.deserialize(templateDTO.getAom());
    }

    public Archetype getTemplateAOM(String templateId) throws InternalErrorException, InstanceNotFoundException {
        return (Archetype)SerializationUtils.deserialize(getCMElement(templateId).getAom());
    }

    public ArchetypeManager getArchetypeManager() {
        return archetypeManager;
    }

    public TemplateMap generateTemplateMap(String templateId) throws InternalErrorException, InstanceNotFoundException {
        TemplateDTO templateDTO = getCMElement(templateId);
        String archetypeId = templateDTO.getArchetypeId();
        Collection<ArchetypeElementVO> archetypeElementVOs =
                getArchetypeManager().getArchetypeElements().getArchetypeElementsVO(archetypeId, templateId);
        Map<String, TemplateElementMap> templateElementMaps = new LinkedHashMap<>();
        TemplateMap templateMap = new TemplateMap(archetypeId, templateId, templateElementMaps);
        Collection<String> elementMapIds = new ArrayList<>();
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
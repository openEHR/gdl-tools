package se.cambio.openehr.controller.session.data;

import org.apache.commons.lang.SerializationUtils;
import org.openehr.am.archetype.Archetype;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.util.CMTypeFormat;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.cm.model.util.TemplateMap;
import se.cambio.openehr.controller.ArchetypeObjectBundleManager;
import se.cambio.openehr.util.ArchetypeOnDemandMap;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;


public class Archetypes extends AbstractCMManager<ArchetypeDTO> {
    public static final ImageIcon ICON = OpenEHRImageUtil.ARCHETYPE;
    private ArchetypeManager archetypeManager = null;
    private ExecutorService executorService;
    private Logger logger = LoggerFactory.getLogger(Archetypes.class);

    public Archetypes(
            ArchetypeManager archetypeManager,
            ExecutorService executorService) {
        super(archetypeManager.getClinicalModelsService());
        this.archetypeManager = archetypeManager;
        this.executorService = executorService;
    }

    @Override
    public void registerCMElementsInCache(Collection<ArchetypeDTO> cmElements) {
        super.registerCMElementsInCache(cmElements);
        processArchetypes(cmElements);
        registerArchetypeDTOs(cmElements);
    }

    @Override
    public Class<ArchetypeDTO> getCMElementClass() {
        return ArchetypeDTO.class;
    }

    public void processArchetypes(Collection<ArchetypeDTO> archetypeDTOs) {
        Collection<Future> futures = new ArrayList<>();
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            futures.add(executorService.submit(() -> processArchetype(archetypeDTO)));
        }
        try {
            for (Future future : futures) {
                future.get();
            }
        } catch (Exception ex) {
            logger.error("Error processing archetypes", ex);
        }
    }

    public void processArchetype(ArchetypeDTO archetypeDTO) {
        new ArchetypeObjectBundleManager(archetypeDTO, archetypeManager).buildArchetypeObjectBundleCustomVO();
    }

    private void registerArchetypeDTOs(Collection<ArchetypeDTO> archetypeDTOs) {
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = getArchetypeObjectBundleCustomVO(archetypeDTO);
            getArchetypeManager().registerArchetypeObjectBundle(
                    archetypeDTO.getId(),
                    null,
                    archetypeObjectBundleCustomVO);
        }
    }

    public static ImageIcon getIcon(String archetypeId) {
        ImageIcon icon = null;
        String entryType = getEntryType(archetypeId);
        if (entryType != null) {
            icon = OpenEHRConstUI.getIcon(entryType);
        }
        if (icon != null) {
            return icon;
        } else {
            return ICON;
        }
    }

    public static String getEntryType(final String archetypeId) {
        final int i = archetypeId.indexOf('.');
        if (i < 0) {
            return null;
        }
        final int j = archetypeId.substring(0, i).lastIndexOf('-');
        if (j + 1 < i) {
            return archetypeId.substring(j + 1, i);
        }
        return null;
    }

    public Map<String, Archetype> getArchetypeMap() {
        return new ArchetypeOnDemandMap(this);
    }

    public Archetype getArchetypeAOMById(String archetypeId) {
        return getArchetypeAOMsByIds(Collections.singleton(archetypeId)).iterator().next();
    }

    public Collection<Archetype> getArchetypeAOMsByIds(Collection<String> archetypeIds) {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(archetypeIds);
        Collection<Archetype> archetypes = new ArrayList<>();
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            archetypes.add(getArchetypeAOM(archetypeDTO));
        }
        return archetypes;
    }

    public Collection<Archetype> getArchetypeAOMsInCacheById(Collection<String> archetypeIds) throws InstanceNotFoundException, InternalErrorException {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementsInCache(archetypeIds);
        return getArchetypeAOMsInCache(archetypeDTOs);
    }


    private Collection<Archetype> getArchetypeAOMsInCache(Collection<ArchetypeDTO> archetypeDTOs) throws InstanceNotFoundException, InternalErrorException {
        Collection<Archetype> archetypes = new ArrayList<>();
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            Archetype archetype = getArchetypeAOM(archetypeDTO);
            if (archetype != null) {
                archetypes.add(archetype);
            }
        }
        return archetypes;
    }

    private Archetype getArchetypeAOM(ArchetypeDTO archetypeDTO) {
        if (!CMTypeFormat.ADL_FORMAT.getFormat().equals(archetypeDTO.getFormat())) {
            return null;
        }
        if (archetypeDTO.getAom() == null) {
            processArchetype(archetypeDTO);
        }
        return (Archetype) SerializationUtils.deserialize(archetypeDTO.getAom());
    }

    public org.openehr.jaxb.am.Archetype getArchetypeAOM2ById(String archetypeId) {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(Collections.singleton(archetypeId));
        if (archetypeDTOs.isEmpty()) {
            throw new InstanceNotFoundException(archetypeId, ArchetypeDTO.class.getName());
        }
        return getArchetypeAOM2(archetypeDTOs.iterator().next());
    }


    private org.openehr.jaxb.am.Archetype getArchetypeAOM2(ArchetypeDTO archetypeDTO) {
        if (!CMTypeFormat.ADLS_FORMAT.getFormat().equals(archetypeDTO.getFormat())) {
            throw new InternalErrorException(new Exception("Invalid call for AOM for '" + archetypeDTO.getId() + "' with format '" + archetypeDTO.getFormat() + "'"));
        }
        if (archetypeDTO.getAom() == null) {
            processArchetype(archetypeDTO);
        }
        return (org.openehr.jaxb.am.Archetype) SerializationUtils.deserialize(archetypeDTO.getAom());
    }

    public ArchetypeObjectBundleCustomVO getArchetypeAOBCVOById(String archetypeId) {
        Collection<ArchetypeDTO> archetypeDTOs = getCMElementByIds(Collections.singleton(archetypeId));
        if (archetypeDTOs.isEmpty()) {
            throw new InstanceNotFoundException(archetypeId, ArchetypeDTO.class.getName());
        }
        return getArchetypeAOBCVO(archetypeDTOs.iterator().next());
    }

    private ArchetypeObjectBundleCustomVO getArchetypeAOBCVO(ArchetypeDTO archetypeDTO) {
        if (archetypeDTO.getAobcVO() == null) {
            processArchetype(archetypeDTO);
        }
        return (ArchetypeObjectBundleCustomVO) SerializationUtils.deserialize(archetypeDTO.getAobcVO());
    }

    private static ArchetypeObjectBundleCustomVO getArchetypeObjectBundleCustomVO(ArchetypeDTO archetypeDTO) {
        return (ArchetypeObjectBundleCustomVO) SerializationUtils.deserialize(archetypeDTO.getAobcVO());
    }

    public ArchetypeManager getArchetypeManager() {
        return archetypeManager;
    }

    public TemplateMap generateTemplateMap(String archetypeId) {
        Collection<ArchetypeElementVO> archetypeElementVOs =
                getArchetypeManager().getArchetypeElements().getArchetypeElementsVO(archetypeId, null);
        Map<String, TemplateElementMap> templateElementMaps = new LinkedHashMap<>();
        TemplateMap templateMap = new TemplateMap(archetypeId, null, templateElementMaps);
        Collection<String> elementMapIds = new ArrayList<>();
        for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
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
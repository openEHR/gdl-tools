package se.cambio.cds.controller.session.data;

import org.apache.commons.lang.SerializationUtils;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.AbstractCMManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


public class Guides extends AbstractCMManager<GuideDTO> {
    private static Guides instance = null;

    public Guides() {
    }

    @Override
    public void registerCMElementsInCache(Collection<GuideDTO> guideDTOs) {
        super.registerCMElementsInCache(guideDTOs);
        processGuides(guideDTOs);
    }

    @Override
    public Class<GuideDTO> getCMElementClass() {
        return GuideDTO.class;
    }

    public void processGuides(Collection<GuideDTO> guideDTOs) {
        for (GuideDTO guideDTO : guideDTOs) {
            processGuide(guideDTO);
        }
    }

    public void processGuide(GuideDTO guideDTO) {
        if (!hasGuideObject(guideDTO)) {
            LoggerFactory.getLogger(Guides.class).info("Parsing guideline '{}'...", guideDTO.getId());
            long startTime = System.currentTimeMillis();
            parseGuide(guideDTO);
            long endTime = System.currentTimeMillis();
            LoggerFactory.getLogger(Guides.class).info("Done ({} ms)", (endTime - startTime));
        }
        if (!isCompiled(guideDTO)) {
            LoggerFactory.getLogger(Guides.class).info("Compiling guideline '{}'...", guideDTO.getId());
            long startTime = System.currentTimeMillis();
            compileGuide(guideDTO);
            long endTime = System.currentTimeMillis();
            LoggerFactory.getLogger(Guides.class).info("Done ({} ms)", (endTime - startTime));
        }
    }

    public static void parseGuide(GuideDTO guideDTO) {
        try {
            Guide guide = new GDLParser().parse(new ByteArrayInputStream(guideDTO.getSource().getBytes("UTF-8")));
            guideDTO.setGuideObject(SerializationUtils.serialize(guide));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    public static void compileGuide(GuideDTO guideDTO) {
        if (!hasGuideObject(guideDTO)) {
            parseGuide(guideDTO);
        }
        Guide guide = (Guide) SerializationUtils.deserialize(guideDTO.getGuideObject());
        byte[] compiledGuide = CDSSessionManager.getRuleEngineFacadeDelegate().compile(guide);
        guideDTO.setCompiledGuide(compiledGuide);
    }

    public Guide getGuide(String guideId) throws InternalErrorException, InstanceNotFoundException {
        GuideDTO guideDTO = getCMElement(guideId);
        return getGuide(guideDTO);
    }

    public Guide getGuide(GuideDTO guideDTO) {
        if (guideDTO != null) {
            if (!hasGuideObject(guideDTO)) {
                parseGuide(guideDTO);
            }
            return (Guide) SerializationUtils.deserialize(guideDTO.getGuideObject());
        } else {
            return null;
        }
    }

    public Map<String, Guide> getGuidesMap(Collection<String> guideIds) throws InstanceNotFoundException, InternalErrorException {
        Map<String, Guide> guideMap = new HashMap<String, Guide>();
        for (String guideId : guideIds) {
            guideMap.put(guideId, getGuide(guideId));
        }
        return guideMap;
    }

    public static boolean hasGuideObject(GuideDTO guideDTO) {
        return guideDTO.getGuideObject() != null;
    }

    public static boolean isCompiled(GuideDTO guideDTO) {
        return guideDTO.getCompiledGuide() != null;
    }

    public static Guides getInstance() {
        if (instance == null) {
            instance = new Guides();
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
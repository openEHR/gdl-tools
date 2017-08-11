package se.cambio.cds.controller.session.data;

import org.apache.commons.lang.SerializationUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.AbstractCMManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.Collection;

import static java.lang.String.format;


public class Guides extends AbstractCMManager<GuideDTO> {


    private final RuleEngineService ruleEngineService;
    private GDLParser gdlParser;
    private Logger logger = LoggerFactory.getLogger(Guides.class);

    public Guides(
            ClinicalModelsService clinicalModelsService,
            RuleEngineService ruleEngineService) {
        super(clinicalModelsService);
        this.ruleEngineService = ruleEngineService;
        this.gdlParser = new GDLParser();
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

    private void processGuides(Collection<GuideDTO> guideDTOs) {
        for (GuideDTO guideDTO : guideDTOs) {
            processGuide(guideDTO);
        }
    }

    private void processGuide(GuideDTO guideDTO) {
        long startTime = System.currentTimeMillis();
        boolean parsed = false;
        boolean compiled = false;
        if (!hasGuideObject(guideDTO)) {
            parseGuide(guideDTO);
            parsed = true;
        }
        if (!isCompiled(guideDTO)) {
            compileGuide(guideDTO);
            compiled = true;
        }
        long endTime = System.currentTimeMillis();
        String actions = parsed && compiled ? "parsed and compiled" : (parsed ? "parsed" : (compiled ? "compiled" : ""));
        if (parsed || compiled) {
            logger.info(format("Guideline '%s' %s successfully (%s ms)", guideDTO.getId(), actions, endTime - startTime));
        }
    }

    private void parseGuide(GuideDTO guideDTO) {
        try {
            Guide guide = gdlParser.parse(new ByteArrayInputStream(guideDTO.getSource().getBytes("UTF-8")));
            guideDTO.setGuideObject(SerializationUtils.serialize(guide));
        } catch (UnsupportedEncodingException ex) {
            throw new RuntimeException(ex);
        }
    }

    private void compileGuide(GuideDTO guideDTO) {
        if (!hasGuideObject(guideDTO)) {
            parseGuide(guideDTO);
        }
        Guide guide = (Guide) SerializationUtils.deserialize(guideDTO.getGuideObject());
        byte[] compiledGuide = ruleEngineService.compile(guide);
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

    public static boolean hasGuideObject(GuideDTO guideDTO) {
        return guideDTO.getGuideObject() != null;
    }

    private static boolean isCompiled(GuideDTO guideDTO) {
        return guideDTO.getCompiledGuide() != null;
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
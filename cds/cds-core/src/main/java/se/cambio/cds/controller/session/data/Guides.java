package se.cambio.cds.controller.session.data;

import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.GuideCompiler;
import se.cambio.cds.util.GuideCompilerFactory;
import se.cambio.openehr.controller.session.data.AbstractCMManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.util.Collection;


public class Guides extends AbstractCMManager<GuideDTO>{
    private static Guides _instance = null;
    private static GuideCompiler guideCompiler;

    private Guides(){
    }

    @Override
    public void registerCMElementsInCache(Collection<GuideDTO> guideDTOs){
        super.registerCMElementsInCache(guideDTOs);
        processGuides(guideDTOs);
    }

    @Override
    public Class<GuideDTO> getCMElementClass() {
        return GuideDTO.class;
    }

    public void processGuides(Collection<GuideDTO> guideDTOs) {
        for (GuideDTO guideDTO: guideDTOs){
            processGuide(guideDTO);
        }
    }

    private void processGuide(GuideDTO guideDTO) {
        try {
            if (guideDTO.getGuideObject() == null) {
                parseGuide(guideDTO);
            }
            if (guideDTO.getCompiledGuide() == null) {
                compileGuide(guideDTO);
            }
        } catch (InternalErrorException e){
            ExceptionHandler.handle(e);
        }
    }

    public static void parseGuide(GuideDTO guideDTO) throws InternalErrorException {
        try {
            Guide guide = new GDLParser().parse(new ByteArrayInputStream(guideDTO.getSource().getBytes()));
            guideDTO.setGuideObject(IOUtils.getBytes(guide));
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    public static void compileGuide(GuideDTO guideDTO) throws InternalErrorException {
        try {
            if (guideDTO.getGuideObject()==null){
                parseGuide(guideDTO);
            }
            Guide guide = (Guide) IOUtils.getObject(guideDTO.getGuideObject());
            byte[] compiledGuide = getGuideCompiler().compile(guide);
            guideDTO.setCompiledGuide(compiledGuide);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    private static GuideCompiler getGuideCompiler() throws InternalErrorException {
        if (guideCompiler == null) {
            guideCompiler = GuideCompilerFactory.getDelegate();
        }
        return guideCompiler;
    }

    public Guide getGuide(String guideId) throws InternalErrorException, InstanceNotFoundException {
        GuideDTO guideDTO = getCMElement(guideId);
        return getGuide(guideDTO);
    }

    public static Guide getGuide(GuideDTO guideDTO){
        if (guideDTO!=null){
            return (Guide)IOUtils.getObject(guideDTO.getGuideObject());
        }else{
            return null;
        }
    }

    public static Guides getInstance(){
        if (_instance ==null){
            _instance = new Guides();
        }
        return _instance;
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
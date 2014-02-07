package se.cambio.cds.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegateFactory;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.util.comparators.GuidesComparator;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


public class Guides {
    private static Guides _delegate = null;
    private Map<String, GuideDTO> _guidesMap = null;
    private Map<String, Collection<String>> _keywordsMap = null;
    private boolean _loaded = false;

    private Guides(){
    }

    private static void init(){
        getGuidesMap().clear();
        getKeywordsMap().clear();
    }
    public static void loadGuides() throws InternalErrorException{
        loadGuides(false);
    }

    public static void loadGuides(boolean force) throws InternalErrorException{
        if (!getDelegate()._loaded || force){
            init();
            CDSAdministrationFacadeDelegate adminFD = CDSAdministrationFacadeDelegateFactory.getDelegate();
            Collection<GuideDTO> guideDTOs = adminFD.searchAllGuides();
            loadGuides(guideDTOs);
            getDelegate()._loaded = true;
        }
    }

    public static void loadGuidesById(Collection<String> guideIds) throws InternalErrorException, GuideNotFoundException {
        CDSAdministrationFacadeDelegate adminFD = CDSAdministrationFacadeDelegateFactory.getDelegate();
        Collection<GuideDTO> guideDTOs = adminFD.searchByGuideIds(guideIds);
        for (GuideDTO guideDTO : guideDTOs) {
            registerGuide(guideDTO);
        }
    }

    public static void loadGuides(Collection<GuideDTO> guideDTOs) throws InternalErrorException{
        for (GuideDTO guideDTO : guideDTOs) {
            registerGuide(guideDTO);
        }
    }

    public static void registerGuide(GuideDTO guideDTO) throws InternalErrorException{
        try{
            getGuidesMap().put(guideDTO.getIdGuide(), guideDTO);
            if(guideDTO.getGuideObject()!=null){
                Guide guide = (Guide)IOUtils.getObject(guideDTO.getGuideObject());
                ResourceDescriptionItem rdi = guide.getDescription().getDetails().get(UserConfigurationManager.getLanguage());
                if (rdi==null){
                    rdi = guide.getDescription().getDetails().get(guide.getLanguage().getOriginalLanguage().getCodeString());
                }
                if (rdi!=null && rdi.getKeywords()!=null){
                    getKeywordsMap().put(guide.getId(), rdi.getKeywords());
                }
            }
            Logger.getLogger(Guides.class).info("Registering guideline: '"+guideDTO.getIdGuide()+"'.");

        }catch(Exception e){
            throw new InternalErrorException(e);
        }
    }

    public static Collection<String> getKeywords(String guideId){
        return getKeywordsMap().get(guideId);
    }

    public static Set<String> getAllKeywords(){
        Set<String> allKeywords = new HashSet<String>();
        for (Collection<String> keywords : getKeywordsMap().values()) {
            allKeywords.addAll(keywords);
        }
        return allKeywords;
    }

    public static GuideDTO getGuideDTO(String guideId){
        return getGuidesMap().get(guideId);
    }

    public static List<GuideDTO> getAllGuides(){
        return new ArrayList<GuideDTO>(getGuidesMap().values());
    }

    public static List<String> getAllGuideIdsSorted(){
        ArrayList<String> guideIds = new ArrayList<String>(getGuidesMap().keySet());
        Collections.sort(guideIds);
        return guideIds;
    }

    public static Collection<String> getAllInactiveGuideIds(){
        Collection<String> inactiveGuideIds = new ArrayList<String>();
        for (GuideDTO guideDTO: getAllGuides()){
            if (!guideDTO.isActive()){
                inactiveGuideIds.add(guideDTO.getIdGuide());
            }
        }
        return inactiveGuideIds;
    }

    public static Collection<Guide> getAllGuideObjects(){
        ArrayList<Guide> guides = new ArrayList<Guide>();
        for (GuideDTO guideDTO: getAllGuides()){
            Guide guide = getGuide(guideDTO);
            guides.add(guide);
        }
        return guides;
    }

    public static Guide getGuide(String guideId){
        GuideDTO guideDTO = getGuideDTO(guideId);
        return getGuide(guideDTO);
    }

    public static Guide getGuide(GuideDTO guideDTO){
        if (guideDTO!=null){
            return (Guide)IOUtils.getObject(guideDTO.getGuideObject());
        }else{
            return null;
        }
    }

    public static boolean isActive(String guideId){
        GuideDTO guideDTO = getGuideDTO(guideId);
        return guideDTO!=null && guideDTO.isActive();
    }

    private static Map<String, GuideDTO> getGuidesMap(){
        if (getDelegate()._guidesMap==null){
            getDelegate()._guidesMap = new HashMap<String, GuideDTO>();
        }
        return getDelegate()._guidesMap;
    }

    private static Map<String, Collection<String>> getKeywordsMap(){
        if (getDelegate()._keywordsMap==null){
            getDelegate()._keywordsMap = new HashMap<String, Collection<String>>();
        }
        return getDelegate()._keywordsMap;
    }

    public static void removeGuide(String guideId) throws InternalErrorException{
        getGuidesMap().remove(guideId);
    }

    public int hashCode(){
        List<GuideDTO> guideDTOs = getAllGuides();
        Collections.sort(guideDTOs, new GuidesComparator());
        List<String> defs = new ArrayList<String>();
        for (GuideDTO guideDTO: guideDTOs){
            defs.add(guideDTO.getGuideSrc()+guideDTO.isActive());
        }
        return defs.hashCode();
    }

    public static Guides getDelegate(){
        if (_delegate==null){
            _delegate = new Guides();
        }
        return _delegate;
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
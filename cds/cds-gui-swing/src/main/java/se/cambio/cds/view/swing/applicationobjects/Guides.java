package se.cambio.cds.view.swing.applicationobjects;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.view.swing.applicationobjects.Guides;
import se.cambio.openehr.controller.InitialLoadingObservable;
import se.cambio.openehr.controller.InitialLoadingObservable.LoadingStage;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;


public class Guides {
    private static Guides _delegate = null;
    private Map<String, GuideDTO> _guidesMap = null;
    private List<String> _allGuideIds = null;
    private Collection<String> _inactiveGuideIds = null;
    private Map<String, Collection<String>> _keywordsMap = null;

    private Guides(){
    }

    private static void init(){
	getDelegate()._guidesMap = new HashMap<String, GuideDTO>();
	getDelegate()._allGuideIds = new ArrayList<String>();
	getDelegate()._inactiveGuideIds = new ArrayList<String>();
	getDelegate()._keywordsMap = new HashMap<String, Collection<String>>();
    }

    public static void loadAllGuides() throws InternalErrorException{
	init();
	InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.GUIDES);
	CDSAdministrationFacadeDelegate adminFD = CDSSessionManager.getAdministrationFacadeDelegate();
	Collection<GuideDTO> guideDTOs = adminFD.searchAllGuides();
	int total = guideDTOs.size();
	int count = 1;
	for (GuideDTO guideDTO : guideDTOs) {
	    registerGuide(guideDTO);
	    InitialLoadingObservable.setCurrentProgress((double)count++/total);
	}
	Collections.sort(getDelegate()._allGuideIds);
	InitialLoadingObservable.setCurrentLoadingStageFinished();
	getDelegate()._inactiveGuideIds = adminFD.searchInactiveGuideIds();
    }

    public static void registerGuide(GuideDTO guideDTO) throws InternalErrorException{
	try{
	    getDelegate()._guidesMap.put(guideDTO.getIdGuide(), guideDTO);
	    getDelegate()._allGuideIds.add(guideDTO.getIdGuide());
	    Guide guide = (Guide)IOUtils.getObject(guideDTO.getGuideObject());
	    ResourceDescriptionItem rdi = guide.getDescription().getDetails().get(UserConfigurationManager.getLanguage());
	    if (rdi==null){
		rdi = guide.getDescription().getDetails().get(guide.getLanguage().getOriginalLanguage().getCodeString());
	    }
	    if (rdi!=null && rdi.getKeywords()!=null){
		getDelegate()._keywordsMap.put(guide.getId(), rdi.getKeywords());
	    }
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    public static Collection<String> getKeywords(String guideId){
	return getDelegate()._keywordsMap.get(guideId);
    }

    public static Set<String> getAllKeywords(){
	Set<String> allKeywords = new HashSet<String>();
	for (Collection<String> keywords : getDelegate()._keywordsMap.values()) {
	    allKeywords.addAll(keywords);
	}
	return allKeywords;
    }

    public static Collection<String> getAllGuideIds(){
	return getDelegate()._allGuideIds;
    }

    public static Collection<String> getAllInactiveGuideIds(){
	return getDelegate()._inactiveGuideIds;
    }

    public static boolean isActive(String guideId){
	return getAllGuideIds().contains(guideId) && !getAllInactiveGuideIds().contains(guideId);
    }

    public static GuideDTO getGuideDTO(String guideId){
	return getDelegate()._guidesMap.get(guideId);
    }

    public static List<GuideDTO> getAllGuides(){
	return new ArrayList<GuideDTO>(getDelegate()._guidesMap.values());
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
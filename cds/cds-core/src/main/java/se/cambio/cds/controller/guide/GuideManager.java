package se.cambio.cds.controller.guide;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;

import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.execution.vo.ExecutionMode;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class GuideManager {

    private Map<String, GuideDTO> _allGuidesDTOMap = null;
    private Map<String, Guide> _allGuidesMap = null;
    private Map<String, ElementInstanceCollection> _elementInstanceCollectionByIdGuideMap = null;
    private ElementInstanceCollection _completeElementInstanceCollection = null;

    public GuideManager(Collection<GuideDTO> guideDTOs){
	init();
	try{
	    loadGuides(guideDTOs);
	    _completeElementInstanceCollection = new GeneratedElementInstanceCollection();
	    _completeElementInstanceCollection.merge(_elementInstanceCollectionByIdGuideMap.values());
	    Logger.getLogger(GuideManager.class).debug(_completeElementInstanceCollection.toString());
	}catch(InternalErrorException e){
	    ExceptionHandler.handle(e);
	}
    }

    private void init(){
	_allGuidesDTOMap = new HashMap<String, GuideDTO>();
	_allGuidesMap = new HashMap<String, Guide>();
	_elementInstanceCollectionByIdGuideMap = new HashMap<String, ElementInstanceCollection>();
    }

    public void loadGuides(Collection<GuideDTO> guidesDTO) throws InternalErrorException{
	for (GuideDTO guideDTO : guidesDTO) {
	    _allGuidesDTOMap.put(guideDTO.getIdGuide(), guideDTO);
	    proccessGuide(guideDTO);
	}
    }

    private void proccessGuide(GuideDTO guideDTO) throws InternalErrorException{
	GeneratedElementInstanceCollection elementInstanceCollection = new GeneratedElementInstanceCollection();
	Guide guide = GuideUtil.parseGuide(
		guideDTO.getGuideSrc().getBytes(),
		this,
		elementInstanceCollection);
	_allGuidesMap.put(guide.getId(), guide);
	_elementInstanceCollectionByIdGuideMap.put(guide.getId(), elementInstanceCollection);
    }

    public Collection<GuideDTO> getGuides(ExecutionMode executionMode, ElementInstanceCollection elementInstancesCollection){
	if (executionMode.equals(ExecutionMode.STRICT)){
	    return getGuidesStrict(elementInstancesCollection);
	}else if (executionMode.equals(ExecutionMode.NORMAL)){
	    return getGuidesNormal(elementInstancesCollection);
	}else{
	    return getAllGuidesDTO();
	}
    }

    public Collection<ElementInstance> getElementInstances(String idGuide){
	ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
	if (eic==null){
	    Logger.getLogger(GuideManager.class).warn("Guide id '"+idGuide+"' not found!");
	    return new ArrayList<ElementInstance>();
	}else{
	    return eic.getAllElementInstances();
	}
    }


    public Collection<GuideDTO> getGuidesStrict(ElementInstanceCollection elementInstancesCollection){
	Set<String> guideIds = new HashSet<String>();
	//Get all idElements
	Set<String> idElementsEHR = elementInstancesCollection.getElementIdsByIdDomain(Domains.EHR_ID);

	for (String idGuide : _elementInstanceCollectionByIdGuideMap.keySet()) {
	    ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
	    Set<String> idElementsEHRAux = new HashSet<String>();
	    idElementsEHRAux.addAll(eic.getElementIdsByIdDomain(Domains.EHR_ID));
	    idElementsEHRAux.addAll(eic.getElementIdsByIdDomain(ElementInstanceCollection.EMPTY_CODE));
	    if (idElementsEHR.containsAll(idElementsEHRAux)){
		guideIds.add(idGuide);
	    }
	}
	return getGuideDTOs(guideIds);
    }

    public ElementInstanceCollection getCompleteElementInstanceCollection(){
	return _completeElementInstanceCollection;
    }

    public Collection<GuideDTO> getGuidesNormal(ElementInstanceCollection elementInstancesCollection){
	Set<String> guideIds = new HashSet<String>();
	//Get all idElements
	Set<String> idElementsEHR = elementInstancesCollection.getElementIdsByIdDomain(Domains.EHR_ID);

	for (String idGuide : _elementInstanceCollectionByIdGuideMap.keySet()) {
	    ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
	    Set<String> idElementsEHRAux = new HashSet<String>();
	    idElementsEHRAux.addAll(eic.getElementIdsByIdDomain(Domains.EHR_ID));
	    idElementsEHRAux.addAll(eic.getElementIdsByIdDomain(ElementInstanceCollection.EMPTY_CODE));
	    Iterator<String> i = idElementsEHRAux.iterator();
	    boolean contains = false;
	    while (i.hasNext() && !contains){
		if (idElementsEHR.contains(i.next())){
		    guideIds.add(idGuide);
		    contains = true;
		}
	    }
	}
	//TODO Look for linked guides
	return getGuideDTOs(guideIds);
    }

    private Collection<GuideDTO> getGuideDTOs(Collection<String> idGuides){
	Collection<GuideDTO> guidesDTO = new ArrayList<GuideDTO>();
	for (String idGuide : idGuides) {
	    guidesDTO.add(_allGuidesDTOMap.get(idGuide));
	}
	return guidesDTO;
    }

    private Map<String, GuideDTO> getAllGuidesDTOMap(){
	if (_allGuidesDTOMap==null){
	    _allGuidesDTOMap = new HashMap<String, GuideDTO>();
	}
	return _allGuidesDTOMap;
    }

    public ArrayList<GuideDTO> getAllGuidesDTO(){
	return new ArrayList<GuideDTO>(getAllGuidesDTOMap().values());
    }

    public Guide getGuide(String idGuide){
	return _allGuidesMap.get(idGuide);
    }

    public Collection<String> getGuidesKey(Collection<GuideDTO> guides){
	ArrayList<String> idGuides = new ArrayList<String>();
	for (GuideDTO guideDTO : guides) {
	    idGuides.add(guideDTO.getIdGuide());
	}
	Collections.sort(idGuides);
	return idGuides;
    }

    public Set<ElementInstance> getElementIdsCDSDomain(String idGuide){
	ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
	if (eic==null){
	    Logger.getLogger(GuideManager.class).warn("Guide id '"+idGuide+"' not found!");
	    return new HashSet<ElementInstance>();
	}else{
	    return eic.getAllElementInstancesByDomain(Domains.CDS_ID);
	}
    }
    
    public Set<ElementInstance> getElementIdsEHRDomain(String idGuide){
	ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
	if (eic==null){
	    Logger.getLogger(GuideManager.class).warn("Guide id '"+idGuide+"' not found!");
	    return new HashSet<ElementInstance>();
	}else{
	    return eic.getAllElementInstancesByDomain(Domains.EHR_ID);
	}
    }
    
    public Set<ElementInstance> getAllElementIdsCDSDomain(){
	Set<ElementInstance> elementInstances = new HashSet<ElementInstance>();
	for (String idGuide : _elementInstanceCollectionByIdGuideMap.keySet()) {
	    elementInstances.addAll(getElementIdsCDSDomain(idGuide));
	}
	return elementInstances;
    }
    
    

    public Set<String> getAllGuideIdsWithCDSDomain(ElementInstance elementInstance){
	Set<String> idGuides = new HashSet<String>();
	for (String idGuide : _elementInstanceCollectionByIdGuideMap.keySet()) {
	    Set<ArchetypeReference> archetypeReferences = 
		    _elementInstanceCollectionByIdGuideMap.get(idGuide).getArchetypeReferences(elementInstance.getArchetypeReference());
	    Iterator<ArchetypeReference> i = archetypeReferences.iterator();
	    boolean inCDS = false;
	    while(i.hasNext() && !inCDS){
		ArchetypeReference ar = i.next();
		if (Domains.CDS_ID.equals(ar.getIdDomain()) && ar.getElementInstancesMap().containsKey(elementInstance.getId())){
		    inCDS = true;
		}
	    }
	    if (inCDS){
		idGuides.add(idGuide);
	    }
	}
	return idGuides;
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
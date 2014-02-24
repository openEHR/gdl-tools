package se.cambio.cds.controller.guide;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.ExecutionMode;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class SimpleGuideManager {

    private Map<String, Guide> _allGuidesMap = null;
    private Map<String, ElementInstanceCollection> _elementInstanceCollectionByIdGuideMap = null;
    private GeneratedElementInstanceCollection _completeElementInstanceCollection = null;


    public SimpleGuideManager(Collection<Guide> guides){
        init();
        try{
            loadGuides(guides);
        }catch(InternalErrorException e){
            ExceptionHandler.handle(e);
        }
    }

    private void init(){
        _allGuidesMap = new HashMap<String, Guide>();
        _elementInstanceCollectionByIdGuideMap = new HashMap<String, ElementInstanceCollection>();
        _completeElementInstanceCollection = new GeneratedElementInstanceCollection();
    }

    public void loadGuides(Collection<Guide> guides) throws InternalErrorException{
        for (Guide guide : guides) {
            GeneratedElementInstanceCollection gei = proccessGuide(guide);
            _completeElementInstanceCollection.merge(gei);
        }
    }

    private GeneratedElementInstanceCollection proccessGuide(Guide guide) throws InternalErrorException{
        GeneratedElementInstanceCollection elementInstanceCollection = new GeneratedElementInstanceCollection();
        GuideUtil.fillElementInstanceCollection(guide, elementInstanceCollection);
        _allGuidesMap.put(guide.getId(), guide);
        _elementInstanceCollectionByIdGuideMap.put(guide.getId(), elementInstanceCollection);
        return elementInstanceCollection;
    }

    public Collection<String> getGuideIds(ExecutionMode executionMode, ElementInstanceCollection elementInstancesCollection){
        Collection<String> guideIds = null;
        if (executionMode.equals(ExecutionMode.STRICT_BY_CONTEXT)){
            guideIds = getGuideIdsStrict(elementInstancesCollection);
        }else if (executionMode.equals(ExecutionMode.CHAINED_BY_CONTEXT)){
            guideIds = getGuideIdsNormal(elementInstancesCollection);
        }else{
            guideIds = getAllGuideIds();
        }
        return guideIds;
    }

    public Collection<ElementInstance> getElementInstances(String idGuide){
        ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
        if (eic==null){
            Logger.getLogger(SimpleGuideManager.class).warn("Guide id '"+idGuide+"' not found!");
            return new ArrayList<ElementInstance>();
        }else{
            return eic.getAllElementInstances();
        }
    }


    public Collection<String> getGuideIdsStrict(ElementInstanceCollection elementInstancesCollection){
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
        return guideIds;
    }

    public GeneratedElementInstanceCollection getCompleteElementInstanceCollection(){
        return _completeElementInstanceCollection;
    }

    public GeneratedElementInstanceCollection getElementInstanceCollection(Collection<String> guideIds){
        if (guideIds==null){
            return getCompleteElementInstanceCollection();
        }else{
            GeneratedElementInstanceCollection guideEIC = new GeneratedElementInstanceCollection();
            for (String guideId : guideIds) {
                ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(guideId);
                if (eic!=null){
                    guideEIC.merge(eic);
                }
            }
            return guideEIC;
        }
    }

    public Collection<String> getGuideIdsNormal(ElementInstanceCollection elementInstancesCollection){
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
        return guideIds;
    }

    public ArrayList<String> getAllGuideIds(){
        return new ArrayList<String>(_allGuidesMap.keySet());
    }

    public Guide getGuide(String idGuide){
        return _allGuidesMap.get(idGuide);
    }

    public Collection<Guide> getAllGuides(){
        return new ArrayList<Guide>(_allGuidesMap.values());
    }

    public final Map<String,Guide> getAllGuidesMap(){
        return _allGuidesMap;
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
            Logger.getLogger(SimpleGuideManager.class).warn("Guide id '"+idGuide+"' not found!");
            return new HashSet<ElementInstance>();
        }else{
            return eic.getAllElementInstancesByDomain(Domains.CDS_ID);
        }
    }

    public Set<ElementInstance> getElementIdsEHRDomain(String idGuide){
        ElementInstanceCollection eic = _elementInstanceCollectionByIdGuideMap.get(idGuide);
        if (eic==null){
            Logger.getLogger(SimpleGuideManager.class).warn("Guide id '"+idGuide+"' not found!");
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
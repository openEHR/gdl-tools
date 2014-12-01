package se.cambio.cds.controller.guide;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.ExecutionMode;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class SimpleGuideManager {

    private Map<String, Guide> _allGuidesMap = null;
    private Map<String, ElementInstanceCollection> _elementInstanceCollectionByIdGuideMap = null;
    private GeneratedElementInstanceCollection _completeElementInstanceCollection = null;
    private Map<String, Set<String>> _guideIdsByElementIdsMap = null;

    public SimpleGuideManager(Collection<Guide> guides){
        init();
        try{
            loadGuides(guides);
        }catch(InternalErrorException e){
            ExceptionHandler.handle(e);
        }
    }

    public String getId(){
        return generateId(_allGuidesMap.keySet());
    }

    public static String generateId(Collection<String> guideIds) {
        List<String> guideIdsAux = new ArrayList<String>(guideIds);
        Collections.sort(guideIdsAux);
        return StringUtils.join(guideIdsAux, ",");
    }

    private void init(){
        _allGuidesMap = new HashMap<String, Guide>();
        _elementInstanceCollectionByIdGuideMap = new HashMap<String, ElementInstanceCollection>();
        _completeElementInstanceCollection = new GeneratedElementInstanceCollection();
        _guideIdsByElementIdsMap = new HashMap<String, Set<String>>();
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

    public Collection<String> getGuideIds(ExecutionMode executionMode, ElementInstanceCollection elementInstancesCollection)
            throws InternalErrorException {
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

    public Collection<String> getGuideIdsNormal(ElementInstanceCollection elementInstancesCollection) throws InternalErrorException {
        //Get all idElements
        Set<String> elementIds = elementInstancesCollection.getElementIdsByIdDomain(Domains.EHR_ID);
        return getGuideIdsNormal(elementIds);
    }


    public Set<String> getGuideIdsNormal(Collection<String> elementIds)
            throws InternalErrorException {
        String elementIdsKey = getELementIdsKey(elementIds);
        Set<String> guideIds = _guideIdsByElementIdsMap.get(elementIdsKey);
        if (guideIds==null){
            guideIds = generateGuideIdsNormal(elementIds);
        }
        return guideIds;
    }

    private Set<String> generateGuideIdsNormal(Collection<String> elementIds) throws InternalErrorException {
        return getGuideIdsNormal(null, elementIds, Domains.EHR_ID);
    }

    private String getELementIdsKey(Collection<String> elementIds){
        StringBuffer sb = new StringBuffer();
        ArrayList<String> elementIdsList = new ArrayList<String>(elementIds);
        Collections.sort(elementIdsList);
        for(String elementId: elementIdsList){
            sb.append(elementId);
        }
        return sb.toString();
    }

    private Set<String> getGuideIdsNormal(Set<String> skipElementIds, Collection<String> elementIds, String domainId) throws InternalErrorException {
        Set<String> guideIds = new HashSet<String>();
        for (String guideId : getAllGuideIds()) {
            Set<String> idElementsRead = getElementIdsByReads(Collections.singleton(guideId), domainId);
            Iterator<String> i = idElementsRead.iterator();
            boolean contains = false;
            while (i.hasNext() && !contains){
                if (elementIds.contains(i.next())){
                    guideIds.add(guideId);
                    contains = true;
                }
            }
        }
        //Look for linked guides
        Set<String> elementIdsByCDSWrites = getElementIdsByCDSWrites(guideIds);
        if (skipElementIds==null){
            skipElementIds = new HashSet<String>();
        }
        //First call are EHR element ids, we dont want to include those
        if (Domains.CDS_ID.equals(domainId)){
            skipElementIds.addAll(elementIds);
        }
        elementIdsByCDSWrites.removeAll(skipElementIds);
        if (!elementIdsByCDSWrites.isEmpty()){
            guideIds.addAll(getGuideIdsNormal(skipElementIds, elementIdsByCDSWrites, Domains.CDS_ID));
        }
        return guideIds;
    }

    private Set<String> getElementIdsByReads(Collection<String> guideIds, String domainId) throws InternalErrorException {
        Set<String> elementIds = new HashSet<String>();
        for (String guideId: guideIds){
            Guide guide = getGuide(guideId);
            if (guide==null){
                throw new InternalErrorException(new Exception("Guide '"+guideId+"' not found on GuideManager."));
            }
            Set<String> gtCodes = GuideUtil.getGTCodesInReads(guide);
            Map<String, String> elementIdsByGtCodesMap = GuideUtil.getGtCodeElementIdMap(guide, domainId);
            for (String gtCode: gtCodes){
                String elementId = elementIdsByGtCodesMap.get(gtCode);
                if (elementId!=null){
                    elementIds.add(elementId);
                }
            }
        }
        return elementIds;
    }

    private Set<String> getElementIdsByCDSWrites(Collection<String> guideIds) throws InternalErrorException {
        Set<String> elementIds = new HashSet<String>();
        for (String guideId: guideIds){
            Guide guide = getGuide(guideId);
            if (guide==null){
                throw new InternalErrorException(new Exception("Guide '"+guideId+"' not found on GuideManager."));
            }
            Set<String> gtCodes = GuideUtil.getGTCodesInWrites(guide);
            Map<String, String> elementIdsByGtCodesMap = GuideUtil.getGtCodeElementIdMap(guide);
            for (String gtCode: gtCodes){
                String elementId = elementIdsByGtCodesMap.get(gtCode);
                if (elementId==null){
                    throw new InternalErrorException(new Exception("GT code '"+gtCode+"' not found for guide '"+guideId+"' on GuideManager."));
                }
                elementIds.add(elementId);
            }
        }
        return elementIds;
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
            idGuides.add(guideDTO.getId());
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
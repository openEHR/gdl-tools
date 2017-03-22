package se.cambio.cds.controller.guide;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.ExecutionMode;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.cm.model.guide.dto.GuideDTO;

import java.util.*;

import static java.lang.String.format;

public class SimpleGuideManager {

    private LinkedHashMap<String, Guide> allGuidesMap = null;
    private Map<String, ElementInstanceCollection> elementInstanceCollectionByIdGuideMap = null;
    private GeneratedElementInstanceCollection completeElementInstanceCollection = null;
    private Map<String, Set<String>> guideIdsByElementIdsMap = null;
    private Collection<String> lastGuideIdsUsed = null;
    private GeneratedElementInstanceCollection lastGeneratedElementInstanceCollection = null;

    public SimpleGuideManager(Collection<Guide> guides) {
        init();
        loadGuides(guides);
    }

    public String getId() {
        return generateId(allGuidesMap.keySet());
    }

    public static String generateId(Collection<String> guideIds) {
        List<String> guideIdsAux = new ArrayList<>(guideIds);
        Collections.sort(guideIdsAux);
        return StringUtils.join(guideIdsAux, ",");
    }

    private void init() {
        allGuidesMap = new LinkedHashMap<>();
        elementInstanceCollectionByIdGuideMap = new HashMap<>();
        completeElementInstanceCollection = new GeneratedElementInstanceCollection();
        guideIdsByElementIdsMap = new HashMap<>();
    }

    public void loadGuides(Collection<Guide> guides) {
        for (Guide guide : guides) {
            GeneratedElementInstanceCollection gei = processGuide(guide);
            completeElementInstanceCollection.merge(gei);
        }
    }

    private GeneratedElementInstanceCollection processGuide(Guide guide) {
        GeneratedElementInstanceCollection elementInstanceCollection = new GeneratedElementInstanceCollection();
        GuideUtil.fillElementInstanceCollection(guide, elementInstanceCollection);
        allGuidesMap.put(guide.getId(), guide);
        elementInstanceCollectionByIdGuideMap.put(guide.getId(), elementInstanceCollection);
        return elementInstanceCollection;
    }

    public Collection<String> getGuideIds(ExecutionMode executionMode, ElementInstanceCollection elementInstancesCollection) {
        Collection<String> guideIds;
        if (executionMode.equals(ExecutionMode.STRICT_BY_CONTEXT)) {
            guideIds = getGuideIdsStrict(elementInstancesCollection);
        } else if (executionMode.equals(ExecutionMode.CHAINED_BY_CONTEXT)) {
            guideIds = getGuideIdsNormal(elementInstancesCollection);
        } else {
            guideIds = getAllGuideIds();
        }
        return guideIds;
    }

    public Collection<ElementInstance> getElementInstances(String idGuide) {
        ElementInstanceCollection eic = elementInstanceCollectionByIdGuideMap.get(idGuide);
        if (eic == null) {
            LoggerFactory.getLogger(SimpleGuideManager.class).warn("Guide id '{}' not found!", idGuide);
            return new ArrayList<>();
        } else {
            return eic.getAllElementInstances();
        }
    }


    public Collection<String> getGuideIdsStrict(ElementInstanceCollection elementInstancesCollection) {
        Set<String> guideIds = new HashSet<>();
        Set<String> idElementsEHR = elementInstancesCollection.getElementIdsByIdDomain(Domains.EHR_ID);
        for (Map.Entry<String, ElementInstanceCollection> idGuide : elementInstanceCollectionByIdGuideMap.entrySet()) {
            ElementInstanceCollection eic = idGuide.getValue();
            Set<String> idElementsEHRAux = new HashSet<>();
            idElementsEHRAux.addAll(eic.getElementIdsByIdDomain(Domains.EHR_ID));
            idElementsEHRAux.addAll(eic.getElementIdsByIdDomain(ElementInstanceCollection.EMPTY_CODE));
            if (idElementsEHR.containsAll(idElementsEHRAux)) {
                guideIds.add(idGuide.getKey());
            }
        }
        return guideIds;
    }

    public GeneratedElementInstanceCollection getCompleteElementInstanceCollection() {
        return completeElementInstanceCollection;
    }

    public GeneratedElementInstanceCollection getElementInstanceCollection(Collection<String> guideIds) {
        GeneratedElementInstanceCollection geic;
        if (guideIds == null) {
            geic = getCompleteElementInstanceCollection();
        } else {
            geic = getGeneratedElementInstanceCollection(guideIds);
        }
        return geic;
    }

    private synchronized GeneratedElementInstanceCollection getGeneratedElementInstanceCollection(Collection<String> guideIds) {
        if (lastGuideIdsUsed == null || !lastGuideIdsUsed.equals(guideIds) || lastGeneratedElementInstanceCollection == null) {
            lastGuideIdsUsed = guideIds;
            lastGeneratedElementInstanceCollection = generateElementInstanceCollection(guideIds);
        }
        return lastGeneratedElementInstanceCollection;
    }

    private GeneratedElementInstanceCollection generateElementInstanceCollection(Collection<String> guideIds) {
        GeneratedElementInstanceCollection guideEIC = new GeneratedElementInstanceCollection();
        for (String guideId : guideIds) {
            ElementInstanceCollection eic = elementInstanceCollectionByIdGuideMap.get(guideId);
            if (eic != null) {
                guideEIC.merge(eic);
            }
        }
        return guideEIC;
    }

    public Collection<String> getGuideIdsNormal(ElementInstanceCollection elementInstancesCollection) {
        Set<String> elementIds = elementInstancesCollection.getElementIdsByIdDomain(Domains.EHR_ID);
        return getGuideIdsNormal(elementIds);
    }


    public Set<String> getGuideIdsNormal(Collection<String> elementIds) {
        String elementIdsKey = getElementIdsKey(elementIds);
        Set<String> guideIds = guideIdsByElementIdsMap.get(elementIdsKey);
        if (guideIds == null) {
            guideIds = generateGuideIdsNormal(elementIds);
        }
        return guideIds;
    }

    private Set<String> generateGuideIdsNormal(Collection<String> elementIds) {
        return getGuideIdsNormal(null, elementIds, Domains.EHR_ID);
    }

    private String getElementIdsKey(Collection<String> elementIds) {
        StringBuffer sb = new StringBuffer();
        ArrayList<String> elementIdsList = new ArrayList<>(elementIds);
        Collections.sort(elementIdsList);
        for (String elementId : elementIdsList) {
            sb.append(elementId);
        }
        return sb.toString();
    }

    private Set<String> getGuideIdsNormal(Set<String> skipElementIds, Collection<String> elementIds, String domainId) {
        Set<String> guideIds = new HashSet<>();
        for (String guideId : getAllGuideIds()) {
            Set<String> idElementsRead = getElementIdsByReads(Collections.singleton(guideId), domainId);
            Iterator<String> i = idElementsRead.iterator();
            boolean contains = false;
            while (i.hasNext() && !contains) {
                if (elementIds.contains(i.next())) {
                    guideIds.add(guideId);
                    contains = true;
                }
            }
        }
        Set<String> elementIdsByCDSWrites = getElementIdsByCDSWrites(guideIds);
        if (skipElementIds == null) {
            skipElementIds = new HashSet<>();
        }
        if (Domains.CDS_ID.equals(domainId)) {
            skipElementIds.addAll(elementIds);
        }
        elementIdsByCDSWrites.removeAll(skipElementIds);
        if (!elementIdsByCDSWrites.isEmpty()) {
            guideIds.addAll(getGuideIdsNormal(skipElementIds, elementIdsByCDSWrites, Domains.CDS_ID));
        }
        return guideIds;
    }

    private Set<String> getElementIdsByReads(Collection<String> guideIds, String domainId) {
        Set<String> elementIds = new HashSet<>();
        for (String guideId : guideIds) {
            Guide guide = getGuide(guideId);
            if (guide == null) {
                throw new RuntimeException(format("Guide '%s' not found on GuideManager.", guideId));
            }
            Set<String> gtCodes = GuideUtil.getGTCodesInReads(guide);
            Map<String, String> elementIdsByGtCodesMap = GuideUtil.getGtCodeElementIdMap(guide, domainId);
            for (String gtCode : gtCodes) {
                String elementId = elementIdsByGtCodesMap.get(gtCode);
                if (elementId != null) {
                    elementIds.add(elementId);
                }
            }
        }
        return elementIds;
    }

    private Set<String> getElementIdsByCDSWrites(Collection<String> guideIds) {
        Set<String> elementIds = new HashSet<>();
        for (String guideId : guideIds) {
            Guide guide = getGuide(guideId);
            if (guide == null) {
                throw new RuntimeException(format("Guide '%s' not found on GuideManager.", guideId));
            }
            Set<String> gtCodes = GuideUtil.getGTCodesInWrites(guide);
            Map<String, String> elementIdsByGtCodesMap = GuideUtil.getGtCodeElementIdMap(guide);
            for (String gtCode : gtCodes) {
                String elementId = elementIdsByGtCodesMap.get(gtCode);
                if (elementId == null) {
                    throw new RuntimeException(format("GT code '%s' not found on for guide '%s'.", gtCode, guideId));
                }
                elementIds.add(elementId);
            }
        }
        return elementIds;
    }

    public ArrayList<String> getAllGuideIds() {
        return new ArrayList<>(allGuidesMap.keySet());
    }

    public Guide getGuide(String idGuide) {
        return allGuidesMap.get(idGuide);
    }

    public Collection<Guide> getAllGuides() {
        return new ArrayList<>(allGuidesMap.values());
    }

    public final Map<String, Guide> getAllGuidesMap() {
        return allGuidesMap;
    }

    public Collection<String> getGuidesKey(Collection<GuideDTO> guides) {
        ArrayList<String> idGuides = new ArrayList<>();
        for (GuideDTO guideDTO : guides) {
            idGuides.add(guideDTO.getId());
        }
        Collections.sort(idGuides);
        return idGuides;
    }

    public Set<ElementInstance> getElementIdsCDSDomain(String idGuide) {
        ElementInstanceCollection eic = elementInstanceCollectionByIdGuideMap.get(idGuide);
        if (eic == null) {
            LoggerFactory.getLogger(SimpleGuideManager.class).warn("Guide id '{}' not found!", idGuide);
            return new HashSet<>();
        } else {
            return eic.getAllElementInstancesByDomain(Domains.CDS_ID);
        }
    }

    public Set<ElementInstance> getElementIdsEHRDomain(String idGuide) {
        ElementInstanceCollection eic = elementInstanceCollectionByIdGuideMap.get(idGuide);
        if (eic == null) {
            LoggerFactory.getLogger(SimpleGuideManager.class).warn("Guide id '{}' not found!", idGuide);
            return new HashSet<>();
        } else {
            return eic.getAllElementInstancesByDomain(Domains.EHR_ID);
        }
    }

    public Set<ElementInstance> getAllElementIdsCDSDomain() {
        Set<ElementInstance> elementInstances = new HashSet<>();
        for (String idGuide : elementInstanceCollectionByIdGuideMap.keySet()) {
            elementInstances.addAll(getElementIdsCDSDomain(idGuide));
        }
        return elementInstances;
    }


    public Set<String> getAllGuideIdsWithCDSDomain(ElementInstance elementInstance) {
        Set<String> idGuides = new HashSet<>();
        for (Map.Entry<String, ElementInstanceCollection> idGuide : elementInstanceCollectionByIdGuideMap.entrySet()) {
            Set<ArchetypeReference> archetypeReferences =
                    idGuide.getValue().getArchetypeReferences(elementInstance.getArchetypeReference());
            Iterator<ArchetypeReference> i = archetypeReferences.iterator();
            boolean inCDS = false;
            while (i.hasNext() && !inCDS) {
                ArchetypeReference ar = i.next();
                if (Domains.CDS_ID.equals(ar.getIdDomain()) && ar.getElementInstancesMap().containsKey(elementInstance.getId())) {
                    inCDS = true;
                }
            }
            if (inCDS) {
                idGuides.add(idGuide.getKey());
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
package se.cambio.cds.controller.guide;

import org.apache.commons.lang.StringUtils;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GeneratedElementInstanceCollection;

import java.util.*;

public class SimpleGuideManager {

    private LinkedHashMap<String, Guide> allGuidesMap = null;
    private Map<String, ElementInstanceCollection> elementInstanceCollectionByIdGuideMap = null;
    private GeneratedElementInstanceCollection completeElementInstanceCollection = null;
    private Collection<String> lastGuideIdsUsed = null;
    private GeneratedElementInstanceCollection lastGeneratedElementInstanceCollection = null;
    private ElementInstanceCollectionManager elementInstanceCollectionManager;

    public SimpleGuideManager(Collection<Guide> guides, ElementInstanceCollectionManager elementInstanceCollectionManager) {
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
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
        completeElementInstanceCollection = new GeneratedElementInstanceCollection(elementInstanceCollectionManager);
    }

    private void loadGuides(Collection<Guide> guides) {
        for (Guide guide : guides) {
            GeneratedElementInstanceCollection gei = processGuide(guide);
            completeElementInstanceCollection.merge(gei);
        }
    }

    private GeneratedElementInstanceCollection processGuide(Guide guide) {
        GeneratedElementInstanceCollection elementInstanceCollection = new GeneratedElementInstanceCollection(elementInstanceCollectionManager);
        GuideUtil.fillElementInstanceCollection(guide, elementInstanceCollection);
        allGuidesMap.put(guide.getId(), guide);
        elementInstanceCollectionByIdGuideMap.put(guide.getId(), elementInstanceCollection);
        return elementInstanceCollection;
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
        GeneratedElementInstanceCollection guideEIC = new GeneratedElementInstanceCollection(elementInstanceCollectionManager);
        for (String guideId : guideIds) {
            ElementInstanceCollection eic = elementInstanceCollectionByIdGuideMap.get(guideId);
            if (eic != null) {
                guideEIC.merge(eic);
            }
        }
        return guideEIC;
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
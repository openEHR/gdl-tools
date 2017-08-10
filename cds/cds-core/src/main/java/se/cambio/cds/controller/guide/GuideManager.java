package se.cambio.cds.controller.guide;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.SerializationUtils;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cm.model.guide.dto.GuideDTO;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;

@Slf4j
public class GuideManager extends SimpleGuideManager {

    private LinkedHashMap<String, GuideDTO> allGuidesDTOMap = null;

    public GuideManager(Collection<GuideDTO> guideDTOs, ElementInstanceCollectionManager elementInstanceCollectionManager) {
        super(loadGuideDTOs(guideDTOs), elementInstanceCollectionManager);
        allGuidesDTOMap = new LinkedHashMap<>();
        for (GuideDTO guideDTO : guideDTOs) {
            allGuidesDTOMap.put(guideDTO.getId(), guideDTO);
        }
    }

    public static Collection<Guide> loadGuideDTOs(Collection<GuideDTO> guidesDTO) {
        Collection<Guide> guides = new ArrayList<>();
        for (GuideDTO guideDTO : guidesDTO) {
            try {
                Guide guide;
                if (Guides.hasGuideObject(guideDTO)) {
                    guide = (Guide) SerializationUtils.deserialize(guideDTO.getGuideObject());
                } else {
                    guide = GuideUtil.parseGuide(new ByteArrayInputStream(guideDTO.getSource().getBytes("UTF-8")));
                }
                guides.add(guide);
            } catch (Exception ex) {
                log.error("Error loading guideline: " + guideDTO.getId(), ex);
            }
        }
        return guides;
    }

    public List<GuideDTO> getAllGuidesDTO() {
        return new ArrayList<>(allGuidesDTOMap.values());
    }

    public GuideDTO getGuideDTO(String idGuide) {
        return allGuidesDTOMap.get(idGuide);
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
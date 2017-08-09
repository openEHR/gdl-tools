package se.cambio.openehr.controller.session.data;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import se.cambio.cm.model.archetype.vo.ArchetypeTermVO;
import se.cambio.cm.model.archetype.vo.OrdinalVO;

import java.util.*;

import static java.lang.String.format;

public class Ordinals {
    private final ArchetypeManager archetypeManager;
    private ListMultimap<String, OrdinalVO> ordinalByElementId = null;
    private Map<String, ListMultimap<String, OrdinalVO>> ordinalsByTemplateIdAndElementId = null;

    Ordinals(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        init();
    }

    public void init() {
        ordinalByElementId = ArrayListMultimap.create();
        ordinalsByTemplateIdAndElementId = new HashMap<>();
    }

    void loadOrdinals(
            String archetypeId,
            String templateId,
            Collection<OrdinalVO> ordinalVOs) {
        cleanPreviousElements(archetypeId, templateId);
        for (OrdinalVO ordinalVO : ordinalVOs) {
            registerOrdinal(ordinalVO);
        }
    }

    private void cleanPreviousElements(String archetypeId, String templateId) {
        if (templateId != null) {
            ordinalsByTemplateIdAndElementId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(ordinalByElementId.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    ordinalByElementId.removeAll(id);
                }
            }
        }
    }

    private void registerOrdinal(OrdinalVO ordinalVO) {
        if (ordinalVO.getIdTemplate() == null) {
            ordinalByElementId.put(ordinalVO.getId(), ordinalVO);
        } else {
            getTemplateOrdinals(ordinalVO.getIdTemplate()).put(ordinalVO.getId(), ordinalVO);
        }
    }

    public OrdinalVO getOrdinalVO(String templateId, String elementId, String code) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, elementId);
        if (templateId == null) {
            if (!ordinalByElementId.containsKey(elementId)) {
                throw new RuntimeException(format("Could not find element '%s'", elementId));
            }
            return getOrdinals(elementId)
                    .stream()
                    .filter(o -> o.getCode().equals(code))
                    .findFirst()
                    .orElseThrow(() -> new RuntimeException(format("Could not find code '%s' in element '%s'", code, elementId)));
        } else {
            if (!getTemplateOrdinals(templateId).containsKey(elementId)) {
                throw new RuntimeException(format("Could not find element '%s' with template '%s'", elementId, templateId));
            }
            return getTemplateOrdinals(templateId).get(elementId)
                    .stream()
                    .filter(o -> o.getCode().equals(code))
                    .findFirst()
                    .orElseThrow(() -> new RuntimeException(format("Could not find code '%s' in element '%s' with template '%s'", code, elementId, templateId)));
        }
    }

    public List<OrdinalVO> getOrdinalVOs(String idTemplate, String elementId) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, elementId);
        if (idTemplate == null) {
            return new ArrayList<>(getOrdinals(elementId));
        } else {
            return new ArrayList<>(getTemplateOrdinals(idTemplate).get(elementId));
        }
    }

    private ListMultimap<String, OrdinalVO> getTemplateOrdinals(String idTemplate) {
        return ordinalsByTemplateIdAndElementId.computeIfAbsent(idTemplate, k -> ArrayListMultimap.create());
    }

    private List<OrdinalVO> getOrdinals(String elementId) {
        return ordinalByElementId.get(elementId);
    }

    public String getText(OrdinalVO ordinalVO, String lang) {
        return getText(ordinalVO.getIdTemplate(), ordinalVO.getId(), ordinalVO.getCode(), lang);
    }

    public String getText(String idTemplate, String idElement, String ordinalKey, String lang) {
        OrdinalVO ordinalVO = getOrdinalVO(idTemplate, idElement, ordinalKey);
        if (ordinalVO != null) {
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(archetypeId, idTemplate, idElement, ordinalVO.getCode(), lang);
            if (archetypeTem != null) {
                return archetypeTem.getText();
            } else {
                return ordinalVO.getName();
            }
        } else {
            return "*UNKNOWN*";
        }
    }

    public String getDescription(OrdinalVO ordinalVO, String lang) {
        return getDescription(ordinalVO.getIdTemplate(), ordinalVO.getId(), ordinalVO.getCode(), lang);
    }

    public String getDescription(String idTemplate, String idElement, String ordinalKey, String lang) {
        OrdinalVO ordinalVO = getOrdinalVO(idTemplate, idElement, ordinalKey);
        if (ordinalVO != null) {
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(archetypeId, idTemplate, idElement, ordinalVO.getCode(), lang);
            if (archetypeTem != null) {
                return archetypeTem.getDescription();
            } else {
                return ordinalVO.getDescription();
            }
        } else {
            return "*UNKNOWN*";
        }
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
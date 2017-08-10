package se.cambio.openehr.controller.session.data;

import se.cambio.cm.model.archetype.vo.UnitVO;

import java.util.*;

import static java.lang.String.format;

public class Units {
    private Map<String, Set<String>> registeredUnitsByElement = null;
    private Map<String, Map<String, Set<String>>> registeredUnitsByTemplate = null;

    Units() {
        init();
    }

    public void init() {
        registeredUnitsByElement = new HashMap<>();
        registeredUnitsByTemplate = new HashMap<>();
    }

    void loadUnits(
            String archetypeId,
            String templateId,
            Collection<UnitVO> unitVOs) {
        cleanPreviousElements(archetypeId, templateId);
        for (UnitVO unitVO : unitVOs) {
            registerUnit(unitVO);
        }
    }

    private void cleanPreviousElements(String archetypeId, String templateId) {
        if (templateId != null) {
            registeredUnitsByTemplate.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(registeredUnitsByElement.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    registeredUnitsByElement.remove(id);
                }
            }
        }
    }

    private void registerUnit(UnitVO unitVO) {
        if (unitVO.getIdTemplate() == null) {
            registerUnitWithoutTemplate(unitVO);
        } else {
            registerUnitWithTemplate(unitVO);
        }
    }

    private void registerUnitWithoutTemplate(UnitVO unitVO) {
        Set<String> units = registeredUnitsByElement.computeIfAbsent(unitVO.getIdElement(), k -> new HashSet<>());
        units.add(unitVO.getUnit());
    }

    private void registerUnitWithTemplate(UnitVO unitVO) {
        Map<String, Set<String>> unitsByTemplate = registeredUnitsByTemplate.computeIfAbsent(unitVO.getIdTemplate(), k -> new HashMap<>());
        Set<String> units = unitsByTemplate.computeIfAbsent(unitVO.getIdElement(), k -> new HashSet<>());
        units.add(unitVO.getUnit());
    }

    public Collection<String> getUnits(String idTemplate, String idElement) {
        Set<String> units;
        if (idTemplate == null) {
            units = getUnitsByArchetype(idElement);
        } else {
            units = getUnitsByTemplateAndElement(idTemplate, idElement);
        }
        return Collections.unmodifiableCollection(units);
    }

    private Set<String> getUnitsByArchetype(String idElement) {
        if (!registeredUnitsByElement.containsKey(idElement)) {
            throw new RuntimeException(format("Could not find element '%s'", idElement));
        }
        return registeredUnitsByElement.get(idElement);
    }

    private Set<String> getUnitsByTemplateAndElement(String templateId, String elementId) {
        if (!registeredUnitsByTemplate.containsKey(templateId)) {
            throw new RuntimeException(format("Could not find element '%s' in template '%s'", elementId, templateId));
        }
        Map<String, Set<String>> unitsByTemplate = registeredUnitsByTemplate.get(templateId);
        if (!unitsByTemplate.containsKey(elementId)) {
            throw new RuntimeException(format("Could not find element '%s' in template '%s'", elementId, templateId));
        }
        return unitsByTemplate.get(elementId);
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
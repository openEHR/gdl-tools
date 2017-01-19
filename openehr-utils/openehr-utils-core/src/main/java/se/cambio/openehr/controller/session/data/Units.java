package se.cambio.openehr.controller.session.data;

import se.cambio.cm.model.archetype.vo.UnitVO;

import java.util.*;

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

    void loadUnits(Collection<UnitVO> unitVOs) {
        for (UnitVO unitVO : unitVOs) {
            registerUnit(unitVO);
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
            units = getUnitsByTemplate(idElement);
        } else {
            units = getUnitsByElement(idTemplate, idElement);
        }
        return Collections.unmodifiableCollection(units);
    }

    private Set<String> getUnitsByTemplate(String idElement) {
        Set<String> units;
        if (registeredUnitsByElement.containsKey(idElement)) {
            units = registeredUnitsByElement.get(idElement);
        } else {
            units = new HashSet<>();
        }
        return units;
    }

    private Set<String> getUnitsByElement(String templateId, String elementId) {
        Set<String> units;
        if (registeredUnitsByTemplate.containsKey(templateId)) {
            Map<String, Set<String>> unitsByTemplate = registeredUnitsByTemplate.get(templateId);
            if (unitsByTemplate.containsKey(elementId)) {
                units = unitsByTemplate.get(elementId);
            } else {
                units = new HashSet<>();
            }
        } else {
            units = new HashSet<>();
        }
        return units;
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
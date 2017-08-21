package se.cambio.openehr.controller.session.data;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import org.openehr.rm.datatypes.quantity.ProportionKind;
import se.cambio.cm.model.archetype.vo.ProportionTypeVO;

import java.util.*;

public class ProportionTypes {
    private ListMultimap<String, ProportionKind> proportionTypesByIdElement;
    private Map<String, ListMultimap<String, ProportionKind>> proportionTypesByTemplateIdAndElementId = null;


    ProportionTypes() {
        init();
    }

    public void init() {
        proportionTypesByIdElement = ArrayListMultimap.create();
        proportionTypesByTemplateIdAndElementId = new HashMap<>();
    }

    void loadProportionTypes(
            String archetypeId,
            String templateId,
            Collection<ProportionTypeVO> proportionTypeVOs) {
        cleanPreviousElements(archetypeId, templateId);
        for (ProportionTypeVO proportionTypeVO : proportionTypeVOs) {
            registerProportionType(proportionTypeVO);
        }
    }

    private void cleanPreviousElements(String archetypeId, String templateId) {
        if (templateId != null) {
            proportionTypesByTemplateIdAndElementId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(proportionTypesByIdElement.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    proportionTypesByIdElement.removeAll(id);
                }
            }
        }
    }

    private void registerProportionType(ProportionTypeVO proportionTypeVO) {
        if (proportionTypeVO.getIdTemplate() == null) {
            getProportionTypes(proportionTypeVO.getIdElement()).add(ProportionKind.fromValue(proportionTypeVO.getType()));
        } else {
            getProportionTypes(proportionTypeVO.getIdTemplate(), proportionTypeVO.getIdElement()).add(ProportionKind.fromValue(proportionTypeVO.getType()));
        }
    }

    private ListMultimap<String, ProportionKind> getProportionTypesInTemplate(String templateId) {
        return proportionTypesByTemplateIdAndElementId.computeIfAbsent(templateId, k -> ArrayListMultimap.create());
    }

    public Collection<ProportionKind> getProportionTypes(String idTemplate, String elementId) {
        if (idTemplate == null) {
            if (proportionTypesByIdElement.containsKey(elementId)) {
                return getProportionTypes(elementId);
            } else {
                return Collections.emptySet();
            }
        } else {
            if (getProportionTypesInTemplate(idTemplate).containsKey(elementId)) {
                return getProportionTypesInTemplate(idTemplate).get(elementId);
            } else {
                return Collections.emptySet();
            }
        }
    }

    private Collection<ProportionKind> getProportionTypes(String elementId) {
        return proportionTypesByIdElement.get(elementId);
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
package se.cambio.openehr.controller.session.data;

import org.openehr.rm.datatypes.quantity.ProportionKind;
import se.cambio.cm.model.archetype.vo.ProportionTypeVO;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static java.lang.String.format;

public class ProportionTypes {
    private Map<String, Collection<ProportionKind>> proportionTypesByIdElement = null;
    private Map<String, Map<String, Collection<ProportionKind>>> templateProportionTypesByTemplateIdAndId = null;


    public ProportionTypes() {
        init();
    }

    public void init() {
        proportionTypesByIdElement = new HashMap<>();
        templateProportionTypesByTemplateIdAndId = new HashMap<>();
    }

    public void loadProportionTypes(
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
            templateProportionTypesByTemplateIdAndId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(proportionTypesByIdElement.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    proportionTypesByIdElement.remove(id);
                }
            }
        }
    }

    public void registerProportionType(ProportionTypeVO proportionTypeVO) {
        if (proportionTypeVO.getIdTemplate() == null) {
            getProportionTypes(proportionTypeVO.getIdElement()).add(ProportionKind.fromValue(proportionTypeVO.getType()));
        } else {
            getProportionTypes(proportionTypeVO.getIdTemplate(), proportionTypeVO.getIdElement()).add(ProportionKind.fromValue(proportionTypeVO.getType()));
        }
    }

    private Map<String, Collection<ProportionKind>> getProportionTypesInTemplate(String idTemplate) {
        return templateProportionTypesByTemplateIdAndId.computeIfAbsent(idTemplate, k -> new HashMap<>());
    }

    public Collection<ProportionKind> getProportionTypes(String idTemplate, String idElement) {
        if (idTemplate == null) {
            if (!proportionTypesByIdElement.containsKey(idElement)) {
                throw new RuntimeException(format("Could not find element '%s'", idElement));
            }
            return getProportionTypes(idElement);
        } else {
            if (!getProportionTypesInTemplate(idTemplate).containsKey(idElement)) {
                throw new RuntimeException(format("Could not find element '%s' with template '%s'", idElement, idTemplate));
            }
            return getProportionTypesInTemplate(idTemplate).get(idElement);
        }
    }

    private Collection<ProportionKind> getProportionTypes(String idElement) {
        return proportionTypesByIdElement.computeIfAbsent(idElement, k -> new ArrayList<>());
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
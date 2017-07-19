package se.cambio.openehr.controller.session.data;

import se.cambio.cm.model.archetype.vo.ArchetypeTermVO;
import se.cambio.cm.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

import static java.lang.String.format;

public class CodedTexts {
    private final ArchetypeManager archetypeManager;
    private Map<String, Map<String, CodedTextVO>> codedTextsByParentId = null;
    private Map<String, Map<String, Map<String, CodedTextVO>>> templateCodedTextsByTemplateIdAndId = null;


    public CodedTexts(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        init();
    }

    public void init() {
        codedTextsByParentId = new HashMap<>();
        templateCodedTextsByTemplateIdAndId = new HashMap<>();
    }

    public void loadCodedTexts(
            String archetypeId,
            String templateId,
            Collection<CodedTextVO> codedTextVOs) {
        cleanPreviousElements(archetypeId, templateId);
        for (CodedTextVO codedTextVO : codedTextVOs) {
            registerCodedText(codedTextVO);
        }
    }

    private void cleanPreviousElements(String archetypeId, String templateId) {
        if (templateId != null) {
            templateCodedTextsByTemplateIdAndId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(codedTextsByParentId.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    codedTextsByParentId.remove(id);
                }
            }
        }
    }

    private void registerCodedText(CodedTextVO codedTextVO) {
        if (codedTextVO.getIdTemplate() == null) {
            getCodedTextMap(codedTextVO.getId()).put(codedTextVO.getCode(), codedTextVO);
        } else {
            getCodedTextTemplateMap(codedTextVO.getIdTemplate(), codedTextVO.getId()).put(codedTextVO.getCode(), codedTextVO);
        }
    }


    private Map<String, Map<String, CodedTextVO>> getCodedTextTemplateMap(String idTemplate) {
        return templateCodedTextsByTemplateIdAndId.computeIfAbsent(idTemplate, k -> new HashMap<>());
    }

    public Map<String, CodedTextVO> getCodedTextTemplateMap(String idTemplate, String idElement) {
        return getCodedTextTemplateMap(idTemplate).computeIfAbsent(idElement, k -> new HashMap<>());
    }

    public CodedTextVO getCodedTextVO(String idTemplate, String idElement, String code) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        if (idTemplate == null) {
            if (!codedTextsByParentId.containsKey(idElement)) {
                throw new RuntimeException(format("Could not find element '%s'", idElement));
            }
            return getCodedTextMap(idElement).get(code);
        } else {
            if (!getCodedTextTemplateMap(idTemplate).containsKey(idElement)) {
                throw new RuntimeException(format("Could not find element '%s' in template '%s'", idElement, idTemplate));
            }
            return getCodedTextTemplateMap(idTemplate, idElement).get(code);
        }
    }

    public List<CodedTextVO> getCodedTextVOs(String idTemplate, String idElement) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        ArrayList<CodedTextVO> codedTexts;
        if (idTemplate == null) {
            codedTexts = new ArrayList<>(getCodedTextMap(idElement).values());
        } else {
            codedTexts = new ArrayList<>(getCodedTextTemplateMap(idTemplate, idElement).values());
        }
        return codedTexts;
    }

    private Map<String, CodedTextVO> getCodedTextMap(String idElement) {
        return codedTextsByParentId.computeIfAbsent(idElement, k -> new HashMap<>());
    }

    public String getText(CodedTextVO codedTextVO, String lang) {
        return getText(codedTextVO.getIdTemplate(), codedTextVO.getId(), codedTextVO.getCode(), lang);
    }

    public String getText(String idTemplate, String idElement, String code, String lang) {
        CodedTextVO codedTextVO = getCodedTextVO(idTemplate, idElement, code);
        if (codedTextVO != null) {
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(archetypeId, idTemplate, idElement, code, lang);
            if (archetypeTem != null) {
                return archetypeTem.getText();
            } else {
                return codedTextVO.getName();
            }
        } else {
            return null;
        }
    }

    public String getDescription(CodedTextVO codedTextVO, String lang) throws InternalErrorException {
        return getDescription(codedTextVO.getIdTemplate(), codedTextVO.getId(), codedTextVO.getCode(), lang);
    }

    public String getDescription(String idTemplate, String idElement, String code, String lang) {
        CodedTextVO codedTextVO = getCodedTextVO(idTemplate, idElement, code);
        if (codedTextVO != null) {
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTermVO archetypeTem = archetypeManager.getArchetypeTerm(archetypeId, idTemplate, idElement, code, lang);
            if (archetypeTem != null) {
                return archetypeTem.getDescription();
            } else {
                return codedTextVO.getDescription();
            }
        } else {
            return null;
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
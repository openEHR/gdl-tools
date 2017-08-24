package se.cambio.openehr.controller.session.data;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import se.cambio.cm.model.archetype.vo.ArchetypeTermVO;
import se.cambio.cm.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

import static java.lang.String.format;

public class CodedTexts {
    private final ArchetypeManager archetypeManager;
    private ListMultimap<String, CodedTextVO> codedTextByElementId;
    private Map<String, ListMultimap<String, CodedTextVO>> codedTextsByTemplateIdAndElementId;


    CodedTexts(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
        init();
    }

    public void init() {
        codedTextByElementId = ArrayListMultimap.create();
        codedTextsByTemplateIdAndElementId = new HashMap<>();
    }

    void loadCodedTexts(
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
            codedTextsByTemplateIdAndElementId.remove(templateId);
        } else {
            Collection<String> ids = new ArrayList<>(codedTextByElementId.keySet());
            for (String id : ids) {
                if (id.startsWith(archetypeId)) {
                    codedTextByElementId.removeAll(id);
                }
            }
        }
    }

    private void registerCodedText(CodedTextVO codedTextVO) {
        if (codedTextVO.getIdTemplate() == null) {
            codedTextByElementId.put(codedTextVO.getId(), codedTextVO);
        } else {
            getCodedTextTemplateMap(codedTextVO.getIdTemplate()).put(codedTextVO.getId(), codedTextVO);
        }
    }


    private ListMultimap<String, CodedTextVO> getCodedTextTemplateMap(String idTemplate) {
        return codedTextsByTemplateIdAndElementId.computeIfAbsent(idTemplate, k -> ArrayListMultimap.create());
    }

    public CodedTextVO getCodedTextVO(String templateId, String elementId, String code) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(templateId, elementId);
        if (templateId == null) {
            if (!codedTextByElementId.containsKey(elementId)) {
                throw new RuntimeException(format("Could not find element '%s'", elementId));
            }
            return getCodedTexts(elementId)
                    .stream()
                    .filter(c -> c.getCode().equals(code))
                    .findFirst()
                    .orElseThrow(() -> new RuntimeException(format("Could not find code '%s' in element '%s'", code, elementId)));
        } else {
            if (!getCodedTextTemplateMap(templateId).containsKey(elementId)) {
                throw new RuntimeException(format("Could not find element '%s' in template '%s'", elementId, templateId));
            }
            List<CodedTextVO> codedTextVOS = getCodedTextTemplateMap(templateId).get(elementId);
            if (codedTextVOS.isEmpty()) {
                throw new RuntimeException(format("Could not find coded texts for element '%s' in template '%s'", elementId, templateId));
            }
            CodedTextVO firstCodedText = codedTextVOS.iterator().next();
            if (isLocalTerminology(firstCodedText)) {
                return codedTextVOS
                        .stream()
                        .filter(c -> c.getCode().equals(code))
                        .findFirst()
                        .orElseThrow(() -> new RuntimeException(format("Could not find code '%s' in element '%s' with template '%s'", code, elementId, templateId)));
            } else {
                return null;
            }
        }
    }

    private boolean isLocalTerminology(CodedTextVO firstCodedText) {
        return "local".equals(firstCodedText.getTerminology());
    }

    public List<CodedTextVO> getCodedTextVOs(String idTemplate, String elementId) {
        archetypeManager.loadArchetypesAndTemplatesIfNeeded(idTemplate, elementId);
        ArrayList<CodedTextVO> codedTexts;
        if (idTemplate == null) {
            codedTexts = new ArrayList<>(getCodedTexts(elementId));
        } else {
            codedTexts = new ArrayList<>(getCodedTextTemplateMap(idTemplate).get(elementId));
        }
        return codedTexts;
    }

    private List<CodedTextVO> getCodedTexts(String idElement) {
        return codedTextByElementId.get(idElement);
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
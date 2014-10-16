package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.openehr.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class CodedTexts {
    private final ArchetypeObjectBundles archetypeObjectBundles;
    private Map<String, Map<String, CodedTextVO>> _codedTextsByParentId = null;
    private Map<String, Map<String, Map<String, CodedTextVO>>> _templateCodedTextsByTemplateIdAndId = null;


    public CodedTexts(ArchetypeObjectBundles archetypeObjectBundles){
        this.archetypeObjectBundles = archetypeObjectBundles;
        init();
    }

    public void init(){
        _codedTextsByParentId = new HashMap<String, Map<String, CodedTextVO>>();
        _templateCodedTextsByTemplateIdAndId = new HashMap<String, Map<String,Map<String,CodedTextVO>>>();
    }
    public void loadCodedTexts(Collection<CodedTextVO> codedTextVOs){
        for (CodedTextVO codedTextVO : codedTextVOs) {
            registerCodedText(codedTextVO);
        }
    }

    public void registerCodedText(CodedTextVO codedTextVO){
        if (codedTextVO.getIdTemplate()==null){
            getCodedTextMap(codedTextVO.getIdParent()).put(codedTextVO.getCode(), codedTextVO);
        }else{
            getCodedTextTemplateMap(codedTextVO.getIdTemplate(),codedTextVO.getIdParent()).put(codedTextVO.getCode(), codedTextVO);
        }
    }


    private Map<String, Map<String, CodedTextVO>> getCodedTextTemplateMap(String idTemplate){
        Map<String, Map<String,CodedTextVO>> map = _templateCodedTextsByTemplateIdAndId.get(idTemplate);
        if (map==null){
            map = new HashMap<String, Map<String,CodedTextVO>>();
            _templateCodedTextsByTemplateIdAndId.put(idTemplate, map);
        }
        return map;
    }

    public Map<String, CodedTextVO> getCodedTextTemplateMap(String idTemplate, String idElement){
        Map<String,CodedTextVO> map = getCodedTextTemplateMap(idTemplate).get(idElement);
        if (map==null){
            map = new HashMap<String,CodedTextVO>();
            getCodedTextTemplateMap(idTemplate).put(idElement, map);
        }
        return map;
    }

    public CodedTextVO getCodedTextVO(String idTemplate, String idElement, String code){
        if (idTemplate==null){
            return getCodedTextMap(idElement).get(code);
        }else{
            return getCodedTextTemplateMap(idTemplate, idElement).get(code);
        }
    }

    public ArrayList<CodedTextVO> getCodedTextVOs(String idTemplate, String idElement){
        ArrayList<CodedTextVO> codedTexts = null;
        if (idTemplate==null){
            codedTexts = new ArrayList<CodedTextVO>(getCodedTextMap(idElement).values());
        }else{
            codedTexts = new ArrayList<CodedTextVO>(getCodedTextTemplateMap(idTemplate, idElement).values());
        }
        return codedTexts;
    }

    private Map<String, CodedTextVO> getCodedTextMap(String idElement){
        Map<String, CodedTextVO> map = _codedTextsByParentId.get(idElement);
        if (map==null){
            map = new HashMap<String, CodedTextVO>();
            _codedTextsByParentId.put(idElement, map);
        }
        return map;
    }

    public String getText(CodedTextVO codedTextVO, String lang) {
        return getText(codedTextVO.getIdTemplate(), codedTextVO.getIdParent(), codedTextVO.getCode(), lang);
    }

    public String getText(String idTemplate, String idElement, String code, String lang) {
        CodedTextVO codedTextVO = getCodedTextVO(idTemplate, idElement, code);
        if (codedTextVO!=null){
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTerm archetypeTem = archetypeObjectBundles.getArchetypeTerm(archetypeId, idTemplate, idElement, code, lang);
            if (archetypeTem!=null){
                return archetypeTem.getText();
            }else{
                return codedTextVO.getName();
            }
        }else{
            return "*UNKNOWN*";
        }
    }

    public String getDescription(CodedTextVO codedTextVO, String lang) throws InternalErrorException {
        return getDescription(codedTextVO.getIdTemplate(), codedTextVO.getIdParent(), codedTextVO.getCode(), lang);
    }

    public String getDescription(String idTemplate, String idElement, String code, String lang) throws InternalErrorException {
        CodedTextVO codedTextVO = getCodedTextVO(idTemplate, idElement, code);
        if (codedTextVO!=null){
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTerm archetypeTem = archetypeObjectBundles.getArchetypeTerm(archetypeId, idTemplate, idElement, code, lang);
            if (archetypeTem!=null){
                return archetypeTem.getDescription();
            }else{
                return codedTextVO.getDescription();
            }
        }else{
            return "*UNKNOWN*";
        }
    }

    private ArchetypeTerms getArchetypeTerms() {
        return this.archetypeObjectBundles.getArchetypeTerms();
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
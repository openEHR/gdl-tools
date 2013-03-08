package se.cambio.cds.openehr.view.applicationobjects;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import se.cambio.cds.openehr.model.codedtext.vo.CodedTextVO;

public class CodedTexts {
    private static CodedTexts _instance = null;
    private Map<String, Map<String, CodedTextVO>> _codedTextsByParentId = null;
    private Map<String, Map<String, Map<String, CodedTextVO>>> _templateCodedTextsByTemplateIdAndId = null;
    
    private CodedTexts(){
	_codedTextsByParentId = new HashMap<String, Map<String, CodedTextVO>>();
	_templateCodedTextsByTemplateIdAndId = new HashMap<String, Map<String,Map<String,CodedTextVO>>>();
    }

    public static void loadCodedTexts(Collection<CodedTextVO> codedTextVOs){
	for (CodedTextVO codedTextVO : codedTextVOs) {
	    registerCodedText(codedTextVO);
	}
    }

    public static void registerCodedText(CodedTextVO codedTextVO){
	if (codedTextVO.getIdTemplate()==null){
	    getCodedTextMap(codedTextVO.getIdParent()).put(codedTextVO.getCode(), codedTextVO);
	}else{
	    getCodedTextTemplateMap(codedTextVO.getIdTemplate(),codedTextVO.getIdParent()).put(codedTextVO.getCode(), codedTextVO);
	}
    }


    private static Map<String, Map<String, CodedTextVO>> getCodedTextTemplateMap(String idTemplate){
	Map<String, Map<String,CodedTextVO>> map = getDelegate()._templateCodedTextsByTemplateIdAndId.get(idTemplate);
	if (map==null){
	    map = new HashMap<String, Map<String,CodedTextVO>>();
	    getDelegate()._templateCodedTextsByTemplateIdAndId.put(idTemplate, map);
	}
	return map;
    }

    public static Map<String, CodedTextVO> getCodedTextTemplateMap(String idTemplate, String idParentArchetypeNode){
	Map<String,CodedTextVO> map = getCodedTextTemplateMap(idTemplate).get(idParentArchetypeNode);
	if (map==null){
	    map = new HashMap<String,CodedTextVO>();
	    getCodedTextTemplateMap(idTemplate).put(idParentArchetypeNode, map);
	}
	return map;
    }

    public static CodedTextVO getCodedTextVO(String idTemplate, String idParentArchetypeNode, String code){
	if (idTemplate==null){
	    return getCodedTextMap(idParentArchetypeNode).get(code);
	}else{
	    return getCodedTextTemplateMap(idTemplate, idParentArchetypeNode).get(code);
	}
    }

    public static ArrayList<CodedTextVO> getCodedTextVOs(String idTemplate, String idParentArchetypeNode){
	ArrayList<CodedTextVO> codedTexts = null;
	if (idTemplate==null){
	    codedTexts = new ArrayList<CodedTextVO>(getCodedTextMap(idParentArchetypeNode).values());
	}else{
	    codedTexts = new ArrayList<CodedTextVO>(getCodedTextTemplateMap(idTemplate, idParentArchetypeNode).values());
	}
	return codedTexts;
    }

    private static Map<String, CodedTextVO> getCodedTextMap(String idParentArchetypeNode){
	Map<String, CodedTextVO> map = getDelegate()._codedTextsByParentId.get(idParentArchetypeNode);
	if (map==null){
	    map = new HashMap<String, CodedTextVO>();
	    getDelegate()._codedTextsByParentId.put(idParentArchetypeNode, map);
	}
	return map;
    }

    public static CodedTexts getDelegate(){
	if (_instance == null){
	    _instance = new CodedTexts();
	}
	return _instance;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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
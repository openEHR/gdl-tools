package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.openehr.model.archetype.vo.OrdinalVO;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class Ordinals {
    private final ArchetypeObjectBundles archetypeObjectBundles;
    private Map<String, Map<Integer, OrdinalVO>> _ordinalByParentId = null;
    private Map<String, Map<String, Map<Integer, OrdinalVO>>> _templateOrdinalsByTemplateIdAndId = null;

    public Ordinals(ArchetypeObjectBundles archetypeObjectBundles){
        this.archetypeObjectBundles = archetypeObjectBundles;
        init();
    }

    public void init(){
        _ordinalByParentId = new HashMap<String, Map<Integer, OrdinalVO>>();
        _templateOrdinalsByTemplateIdAndId = new HashMap<String, Map<String,Map<Integer,OrdinalVO>>>();
    }

    public void loadOrdinals(Collection<OrdinalVO> ordinalVOs){
        for (OrdinalVO ordinalVO : ordinalVOs) {
            registerOrdinal(ordinalVO);
        }
    }

    public void registerOrdinal(OrdinalVO ordinalVO){
        if (ordinalVO.getIdTemplate()==null){
            getOrdinalMap(ordinalVO.getIdParent()).put(ordinalVO.getValue(), ordinalVO);
        }else{
            getOrdinalTemplateMap(ordinalVO.getIdTemplate(),ordinalVO.getIdParent()).put(ordinalVO.getValue(), ordinalVO);
        }
    }

    public OrdinalVO getOrdinalVO(String idTemplate, String idParentArchetypeNode, Integer value){
        if (idTemplate==null){
            return getOrdinalMap(idParentArchetypeNode).get(value);
        }else{
            return getOrdinalTemplateMap(idTemplate, idParentArchetypeNode).get(value);
        }
    }

    public Collection<OrdinalVO> getOrdinalVOs(String idTemplate, String idParentArchetypeNode){
        if (idTemplate==null){
            return new ArrayList<OrdinalVO>(getOrdinalMap(idParentArchetypeNode).values());
        }else{
            return new ArrayList<OrdinalVO>(getOrdinalTemplateMap(idTemplate, idParentArchetypeNode).values());
        }
    }

    private Map<String, Map<Integer, OrdinalVO>> getOrdinalTemplateMap(String idTemplate){
        Map<String, Map<Integer,OrdinalVO>> map = _templateOrdinalsByTemplateIdAndId.get(idTemplate);
        if (map==null){
            map = new HashMap<String, Map<Integer,OrdinalVO>>();
            _templateOrdinalsByTemplateIdAndId.put(idTemplate, map);
        }
        return map;
    }

    public Map<Integer, OrdinalVO> getOrdinalTemplateMap(String idTemplate, String idParentArchetypeNode){
        Map<Integer,OrdinalVO> map = getOrdinalTemplateMap(idTemplate).get(idParentArchetypeNode);
        if (map==null){
            map = new HashMap<Integer,OrdinalVO>();
            getOrdinalTemplateMap(idTemplate).put(idParentArchetypeNode, map);
        }
        return map;
    }

    private Map<Integer, OrdinalVO> getOrdinalMap(String idParentArchetypeNode){
        Map<Integer, OrdinalVO> map = _ordinalByParentId.get(idParentArchetypeNode);
        if (map==null){
            map = new HashMap<Integer, OrdinalVO>();
            _ordinalByParentId.put(idParentArchetypeNode, map);
        }
        return map;
    }

    public String getText(OrdinalVO ordinalVO, String lang) {
        return getText(ordinalVO.getIdTemplate(), ordinalVO.getIdParent(), ordinalVO.getValue(), lang);
    }

    public String getText(String idTemplate, String idElement, Integer value, String lang) {
        OrdinalVO ordinalVO = getOrdinalVO(idTemplate, idElement, value);
        if (ordinalVO!=null){
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTerm archetypeTem = archetypeObjectBundles.getArchetypeTerm(archetypeId, idTemplate, idElement, ordinalVO.getCode(), lang);
            if (archetypeTem!=null){
                return archetypeTem.getText();
            }else{
                return ordinalVO.getName();
            }
        }else{
            return "*UNKNOWN*";
        }
    }

    public String getDescription(OrdinalVO ordinalVO, String lang)  {
        return getDescription(ordinalVO.getIdTemplate(), ordinalVO.getIdParent(), ordinalVO.getValue(), lang);
    }

    public String getDescription(String idTemplate, String idElement, Integer value, String lang) {
        OrdinalVO ordinalVO = getOrdinalVO(idTemplate, idElement, value);
        if (ordinalVO!=null){
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            ArchetypeTerm archetypeTem = archetypeObjectBundles.getArchetypeTerm(archetypeId, idTemplate, idElement, ordinalVO.getCode(), lang);
            if (archetypeTem!=null){
                return archetypeTem.getDescription();
            }else{
                return ordinalVO.getDescription();
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
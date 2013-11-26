package se.cambio.openehr.controller.session.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.openehr.rm.datatypes.quantity.DvOrdinal;

import se.cambio.openehr.model.archetype.vo.OrdinalVO;

public class Ordinals {
    private static Ordinals _instance = null;
    private Map<String, Map<Integer, OrdinalVO>> _ordinalByParentId = null;
    private Map<String, Map<String, Map<Integer, OrdinalVO>>> _templateOrdinalsByTemplateIdAndId = null;

    private Ordinals(){
        _ordinalByParentId = new HashMap<String, Map<Integer, OrdinalVO>>();
        _templateOrdinalsByTemplateIdAndId = new HashMap<String, Map<String,Map<Integer,OrdinalVO>>>();
    }

    public static void loadOrdinals(Collection<OrdinalVO> ordinalVOs){
        for (OrdinalVO ordinalVO : ordinalVOs) {
            registerOrdinal(ordinalVO);
        }
    }

    public static void registerOrdinal(OrdinalVO ordinalVO){
        if (ordinalVO.getIdTemplate()==null){
            getOrdinalMap(ordinalVO.getIdParent()).put(ordinalVO.getValue(), ordinalVO);
        }else{
            getOrdinalTemplateMap(ordinalVO.getIdTemplate(),ordinalVO.getIdParent()).put(ordinalVO.getValue(), ordinalVO);
        }
    }

    public static OrdinalVO getOrdinalVO(String idTemplate, String idParentArchetypeNode, Integer value){
        if (idTemplate==null){
            return getOrdinalMap(idParentArchetypeNode).get(value);
        }else{
            return getOrdinalTemplateMap(idTemplate, idParentArchetypeNode).get(value);
        }
    }

    public static Collection<OrdinalVO> getOrdinalVOs(String idTemplate, String idParentArchetypeNode){
        if (idTemplate==null){
            return new ArrayList<OrdinalVO>(getOrdinalMap(idParentArchetypeNode).values());
        }else{
            return new ArrayList<OrdinalVO>(getOrdinalTemplateMap(idTemplate, idParentArchetypeNode).values());
        }
    }

    private static Map<String, Map<Integer, OrdinalVO>> getOrdinalTemplateMap(String idTemplate){
        Map<String, Map<Integer,OrdinalVO>> map = getDelegate()._templateOrdinalsByTemplateIdAndId.get(idTemplate);
        if (map==null){
            map = new HashMap<String, Map<Integer,OrdinalVO>>();
            getDelegate()._templateOrdinalsByTemplateIdAndId.put(idTemplate, map);
        }
        return map;
    }

    public static Map<Integer, OrdinalVO> getOrdinalTemplateMap(String idTemplate, String idParentArchetypeNode){
        Map<Integer,OrdinalVO> map = getOrdinalTemplateMap(idTemplate).get(idParentArchetypeNode);
        if (map==null){
            map = new HashMap<Integer,OrdinalVO>();
            getOrdinalTemplateMap(idTemplate).put(idParentArchetypeNode, map);
        }
        return map;
    }

    private static Map<Integer, OrdinalVO> getOrdinalMap(String idParentArchetypeNode){
        Map<Integer, OrdinalVO> map = getDelegate()._ordinalByParentId.get(idParentArchetypeNode);
        if (map==null){
            map = new HashMap<Integer, OrdinalVO>();
            getDelegate()._ordinalByParentId.put(idParentArchetypeNode, map);
        }
        return map;
    }

    public static String getName(String idTemplate, String idElement, DvOrdinal dvOrdinal){
        OrdinalVO ordinalVO = getOrdinalVO(idTemplate, idElement, dvOrdinal.getValue());
        if (ordinalVO!=null){
            return ordinalVO.getName();
        }else{
            return dvOrdinal.getSymbolValue();
        }
    }

    public static String getDescription(String idTemplate, String idElement, DvOrdinal dvOrdinal){
        OrdinalVO ordinalVO = getOrdinalVO(idTemplate, idElement, dvOrdinal.getValue());
        if (ordinalVO!=null){
            return ordinalVO.getDescription();
        }else{
            return dvOrdinal.getSymbolValue();
        }
    }

    public static Ordinals getDelegate(){
        if (_instance == null){
            _instance = new Ordinals();
        }
        return _instance;
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
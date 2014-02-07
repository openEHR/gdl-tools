package se.cambio.openehr.controller.session.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import se.cambio.openehr.model.archetype.vo.UnitVO;

public class Units {
    private static Units _instance = null;
    private Map<String, Collection<String>> _unitsByIdElement = null;
    private Map<String, Map<String, Collection<String>>> _templateUnitsByTemplateIdAndId = null;

    private Units(){
	_unitsByIdElement = new HashMap<String, Collection<String>>();
	_templateUnitsByTemplateIdAndId = new HashMap<String, Map<String,Collection<String>>>();
    }

    public static void loadUnits(Collection<UnitVO> unitVOs){
	for (UnitVO unitVO : unitVOs) {
	    registerUnit(unitVO);
	}
    }

    public static void registerUnit(UnitVO unitVO){
	if (unitVO.getIdTemplate()==null){
	    getUnits(unitVO.getIdElement()).add(unitVO.getUnit());
	}else{
	    getUnits(unitVO.getIdTemplate(), unitVO.getIdElement()).add(unitVO.getUnit());
	}
    }

    private static Map<String, Collection<String>> getUnitsInTemplate(String idTemplate){
	Map<String, Collection<String>> map = getDelegate()._templateUnitsByTemplateIdAndId.get(idTemplate);
	if (map==null){
	    map = new HashMap<String, Collection<String>>();
	    getDelegate()._templateUnitsByTemplateIdAndId.put(idTemplate, map);
	}
	return map;
    }

    public static Collection<String> getUnits(String idTemplate, String idElement){
	if (idTemplate==null){
	    return getUnits(idElement);
	}else{
	    Collection<String> units = getUnitsInTemplate(idTemplate).get(idElement);
	    if (units==null){
		units = new ArrayList<String>();
		getUnitsInTemplate(idTemplate).put(idElement, units);
	    }
	    return units;
	}
    }

    private static Collection<String> getUnits(String idElement){
	Collection<String> units = getDelegate()._unitsByIdElement.get(idElement);
	if (units==null){
	    units = new ArrayList<String>();
	    getDelegate()._unitsByIdElement.put(idElement, units);
	}
	return units;
    }

    public static Units getDelegate(){
	if (_instance == null){
	    _instance = new Units();
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
package se.cambio.openehr.controller.session.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import se.cambio.openehr.model.archetype.vo.ArchetypeSlotVO;

public class ArchetypeSlots {
    private static ArchetypeSlots _instance = null;
    private Map<String, ArchetypeSlotVO> _archetypeSlotsById = null;

    private ArchetypeSlots(){
	_archetypeSlotsById = new HashMap<String, ArchetypeSlotVO>();
    }

    public static void loadArchetypeNodes(Collection<ArchetypeSlotVO> archetypeSlotVOs){
	for (ArchetypeSlotVO archetypeSlotVO : archetypeSlotVOs) {
	    registerArchetypeSlot(archetypeSlotVO);
	}
    }

    public static void registerArchetypeSlot(ArchetypeSlotVO archetypeSlotVO){
	getDelegate()._archetypeSlotsById.put(archetypeSlotVO.getId(), archetypeSlotVO);
    }

    public static ArchetypeSlotVO getArchetypeSlot(String idArchetypeNode){
	return getDelegate()._archetypeSlotsById.get(idArchetypeNode);
    }

    public static Collection<ArchetypeSlotVO> getArchetypeSlots(String idArchetype){
	Collection<ArchetypeSlotVO> list = new ArrayList<ArchetypeSlotVO>();
	for (ArchetypeSlotVO archetypeSlotVO : getDelegate()._archetypeSlotsById.values()) {
	    if(idArchetype.equals(archetypeSlotVO.getIdArchetype())){
		list.add(archetypeSlotVO);
	    }
	}
	return list;
    }

    public static boolean passesFilter(String idArchetype, ArchetypeSlotVO archetypeSlotVO){
	boolean included = isIncluded(idArchetype, archetypeSlotVO.getIncludes());
	boolean excluded = isExcluded(idArchetype, archetypeSlotVO.getExludes());
	return (included && !excluded);
    }

    private static boolean isIncluded(String idArchetype, Collection<String> includes){
	for (String includesExp : includes) {
	    if (Pattern.matches(includesExp, idArchetype)){
		return true; 
	    }
	}
	return false;
    }

    private static boolean isExcluded(String idArchetype, Collection<String> excludes){
	for (String excludesExp : excludes) {
	    if (!excludesExp.equals(".*")){
		if (Pattern.matches(excludesExp, idArchetype)){
		    return true;
		}
	    }
	}
	return false;
    }

    public static ArchetypeSlots getDelegate(){
	if (_instance == null){
	    _instance = new ArchetypeSlots();
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
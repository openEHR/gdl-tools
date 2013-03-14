package se.cambio.cds.openehr.view.applicationobjects;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ImageIcon;

import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;

import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.openehr.model.facade.archetype.delegate.ArchetypeFacadeDelegateFactory;
import se.cambio.cds.openehr.model.facade.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRConst;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.exceptions.InternalErrorException;


public class Archetypes {
    private static Archetypes _instance = null;
    private Map<String, ArchetypeDTO> _archetypesById = null;
    private Map<String, Archetype> _archetypeAOM = null;
    public static ImageIcon ICON = ImageUtil.ARCHETYPE;

    private Archetypes(){
	_archetypesById = new HashMap<String, ArchetypeDTO>();
	_archetypeAOM = new HashMap<String, Archetype>();
    }

    public static void loadArchetypes() throws InternalErrorException{
	loadArchetypesObjectBundle(
		ArchetypeFacadeDelegateFactory.getDelegate().getAllArchetypesObjectBundles());
    }

    public static void registerArchertype(ArchetypeDTO archetypeVO){
	getDelegate()._archetypesById.put(archetypeVO.getIdArchetype(), archetypeVO);
    }

    public static ArchetypeDTO getArchetypeVO(String idArchetype){
	return getDelegate()._archetypesById.get(idArchetype);
    }

    public static Archetype getArchetypeAOM(String idArchetype){
	Archetype aom = getDelegate()._archetypeAOM.get(idArchetype);
	if(aom==null){
	    ArchetypeDTO archetypeDTO = getArchetypeVO(idArchetype);
	    if (archetypeDTO!=null){
		aom = (Archetype)IOUtils.getObject(archetypeDTO.getAom());
		getDelegate()._archetypeAOM.put(idArchetype, aom);
	    }else {
		Logger.getLogger(Archetype.class).debug("Archetype '"+idArchetype+"' not found.");
	    }
	}
	return aom;
    }

    public static String getArchetypeSource(String idArchetype){
	return getArchetypeVO(idArchetype).getArchetype();
    }

    public static void loadArchetypesObjectBundle(Collection<ArchetypeObjectBundleCustomVO> archetypeObjectBundleCustomVOs){
	getDelegate()._archetypesById.clear();
	for (ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO : archetypeObjectBundleCustomVOs) {
	    loadArchetypeObjectBundle(archetypeObjectBundleCustomVO);
	}
    }

    public static void loadArchetypeObjectBundle(ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO){
	registerArchertype(archetypeObjectBundleCustomVO.getArchetypeVO());
	ArchetypeElements.loadArchetypeElements(archetypeObjectBundleCustomVO.getElementVOs());
	Clusters.loadClusters(archetypeObjectBundleCustomVO.getClusterVOs());
	CodedTexts.loadCodedTexts(archetypeObjectBundleCustomVO.getCodedTextVOs());
	Ordinals.loadOrdinals(archetypeObjectBundleCustomVO.getOrdinalVOs());
	ArchetypeSlots.loadArchetypeNodes(archetypeObjectBundleCustomVO.getSlotVOs());
	Units.loadUnits(archetypeObjectBundleCustomVO.getUnitVOs());
	ProportionTypesUI.loadProportionTypes(archetypeObjectBundleCustomVO.getProportionTypes());
    }

    public static ArrayList<ArchetypeDTO> getArchetypes(String entryType){
	ArrayList<ArchetypeDTO> list = new ArrayList<ArchetypeDTO>();
	for (ArchetypeDTO archetypeVO : getDelegate()._archetypesById.values()) {
	    if (entryType.equals(archetypeVO.getRMName())){
		list.add(archetypeVO);
	    }
	}
	return list;
    }

    public static ImageIcon getIcon(String idArchetype){
	String entryType = getArchetypeVO(idArchetype).getRMName();
	ImageIcon icon = OpenEHRConst.getIcon(entryType);
	if (icon!=null){
	    return icon;
	}else{
	    return ICON;
	}
    }

    public static Collection<ArchetypeDTO> getAllArchetypes(){
	return new ArrayList<ArchetypeDTO>(getDelegate()._archetypesById.values());
    }

    public static Map<String, Archetype> getArchetypeMap(){
	Map<String, Archetype> archetypeMap = new HashMap<String, Archetype>();
	for (ArchetypeDTO archetypeVO : getAllArchetypes()) {
	    archetypeMap.put(archetypeVO.getIdArchetype(), getArchetypeAOM(archetypeVO.getIdArchetype()));   
	}
	return archetypeMap;
    }

    public static Collection<String> getAllArchetypeIds(){
	return new ArrayList<String>(getDelegate()._archetypesById.keySet());
    }

    public static Archetypes getDelegate(){
	if (_instance == null){
	    _instance = new Archetypes();
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
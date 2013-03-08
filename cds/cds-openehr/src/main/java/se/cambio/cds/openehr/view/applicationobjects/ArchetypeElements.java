package se.cambio.cds.openehr.view.applicationobjects;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.model.cluster.vo.ClusterVO;
import se.cambio.cds.openehr.util.ImageUtil;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.openehr.util.OpenEHRConst;
import se.cambio.cds.openehr.util.OpenEHRDataValuesUI;
import se.cambio.cds.util.OpenEHRDataValues;

public class ArchetypeElements {
    private static ArchetypeElements _instance = null;
    private Map<String, ArchetypeElementVO> _archetypeElementsById = null;
    private Map<String, Map<String, ArchetypeElementVO>> _templateElementsByTemplateIdAndId = null;

    public static String ID_CURRENT_DATE_TIME = "currentDateTime";

    //Add current time
    public static ArchetypeElementVO CURRENT_DATE_TIME =
	    new ArchetypeElementVO(
		    OpenEHRLanguageManager.getMessage("CurrentDateTime"), 
		    OpenEHRLanguageManager.getMessage("CurrentDateTime"), 
		    OpenEHRDataValues.DV_DATE_TIME, null, null, null, null);

    private ArchetypeElements(){
	_archetypeElementsById = new LinkedHashMap<String, ArchetypeElementVO>();
	_templateElementsByTemplateIdAndId = new HashMap<String, Map<String, ArchetypeElementVO>>();

    }

    public static void loadArchetypeElements(Collection<ArchetypeElementVO> archetypeElementVOs){
	for (ArchetypeElementVO archetypeElementVO : archetypeElementVOs) {
	    registerArchetypeElement(archetypeElementVO);
	}
    }

    public static void registerArchetypeElement(ArchetypeElementVO archetypeElementVO){
	if (archetypeElementVO.getIdTemplate()==null){
	    getDelegate()._archetypeElementsById.put(archetypeElementVO.getId(), archetypeElementVO);    
	}else{
	    getArchetypeElementsInTemplate(archetypeElementVO.getIdTemplate()).put(archetypeElementVO.getId(), archetypeElementVO);
	}

    }

    public static ArchetypeElementVO getArchetypeElement(String idTemplate, String idElement){
	if (idTemplate==null){
	    return getDelegate()._archetypeElementsById.get(idElement);
	}else{
	    return getArchetypeElementsInTemplate(idTemplate).get(idElement);
	}
    }

    private static Map<String, ArchetypeElementVO> getArchetypeElementsInTemplate(String idTemplate){
	Map<String, ArchetypeElementVO> elementsInTemplate = 
		getDelegate()._templateElementsByTemplateIdAndId.get(idTemplate);
	if (elementsInTemplate==null){
	    elementsInTemplate = new LinkedHashMap<String, ArchetypeElementVO>();
	    getDelegate()._templateElementsByTemplateIdAndId.put(idTemplate, elementsInTemplate);
	}
	return elementsInTemplate;
    }

    public static Collection<ArchetypeElementVO> getArchetypeElementsVO(String idArchetype, String idTemplate){
	Collection<ArchetypeElementVO> list = new ArrayList<ArchetypeElementVO>();
	if (idTemplate!=null){
	    list.addAll(getArchetypeElementsInTemplate(idTemplate).values());
	}else{
	    for (ArchetypeElementVO archetypeElementVO : getDelegate()._archetypeElementsById.values()) {
		if(idArchetype.equals(archetypeElementVO.getIdArchetype())){
		    list.add(archetypeElementVO);
		}
	    }
	}
	return list;
    }

    public static String getHTMLTooltip(ArchetypeElementVO archetypeElementVO, ArchetypeReference ar){
	return getHTMLTooltip(archetypeElementVO, ar, null);
    }
    
    public static String getHTMLTooltip(ArchetypeElementVO archetypeElementVO, ArchetypeReference ar, String extraLines){
	ArchetypeDTO archetypeVO = Archetypes.getArchetypeVO(archetypeElementVO.getIdArchetype());

	String aggregationFunction = null;
	String idDomain = null;
	if (ar!=null){
	    aggregationFunction = ar.getAggregationFunction();
	    idDomain = ar.getIdDomain();
	}

	String archetypeImageName = OpenEHRConst.getIconName(archetypeVO.getRMName());
	String dataValueImageName = OpenEHRDataValuesUI.getDVIconName(archetypeElementVO.getRMType());

	String elementName = archetypeElementVO.getName()+(aggregationFunction!=null?" ("+AggregationFunctionsUI.getAggregationFunctionName(aggregationFunction)+")":"");

	String archetypeName = ArchetypeReferences.getName(ar)+(idDomain!=null?" ("+idDomain+")":"");
	String cardinalityStr = archetypeElementVO.getLowerCardinality()+"..."+(archetypeElementVO.getUpperCardinality()==null?"*":archetypeElementVO.getUpperCardinality());
	String units = null;
	if (OpenEHRDataValues.DV_QUANTITY.equals(archetypeElementVO.getRMType())){
	    StringBuffer unitsSB = new StringBuffer();
	    for (String unit : Units.getUnits(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId())) {
		unitsSB.append(unit+", ");
	    }
	    if (unitsSB.length()>1){
		units = unitsSB.toString();
		units = units.substring(0, units.length()-2);
	    }
	}

	String path = null;
	String[] pathArray = archetypeElementVO.getPath().split("\\/");
	StringBuffer pathSB = new StringBuffer();
	StringBuffer clusterPathSB = new StringBuffer();
	clusterPathSB.append(archetypeVO.getIdArchetype());
	for (String pathNode : pathArray) {
	    if (!pathNode.isEmpty()){
		clusterPathSB.append("/"+pathNode);
		ClusterVO clusterVO = Clusters.getClusterVO(archetypeElementVO.getIdTemplate(), clusterPathSB.toString());
		if (clusterVO!=null){
		    pathSB.append(ImageUtil.getImgHTMLTag(OpenEHRConst.getIconName(clusterVO.getRMType()))+"&nbsp;"+clusterVO.getName()+" / ");
		}
	    }
	}
	if (pathSB.length()>0){
	    path = pathSB.toString();
	    path = path.substring(0, path.length()-3);
	}
	return "<html><table width=500>"+
	"<tr><td><b>"+OpenEHRLanguageManager.getMessage("Name")+": </b>"+elementName+"</td><td><b>"+OpenEHRLanguageManager.getMessage("Archetype")+": </b>"+ImageUtil.getImgHTMLTag(archetypeImageName)+"&nbsp;"+archetypeName+"</td></tr>"+
	"<tr><td><b>"+OpenEHRLanguageManager.getMessage("DataValue")+": </b>"+ImageUtil.getImgHTMLTag(dataValueImageName)+"&nbsp;"+OpenEHRDataValuesUI.getName(archetypeElementVO.getRMType())+"</td><td><b>"+OpenEHRLanguageManager.getMessage("Occurrences")+": </b>"+cardinalityStr+"</td></tr>"+
	(units!=null?"<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Units")+": </b>"+units+"</td></tr>":"")+
	"<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Path")+": </b>"+path+"</td></tr>"+
	"<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Description")+": </b>"+archetypeElementVO.getDescription()+"</td></tr>"+
	(extraLines!=null?extraLines:"")+
	"</table></html>";
    }

    public static ArrayList<ClusterVO> getClusters(ArchetypeElementVO archetypeElementVO){
	ArrayList<ClusterVO> clusters = new ArrayList<ClusterVO>();
	String[] pathArray = archetypeElementVO.getPath().split("\\/");
	StringBuffer clusterPathSB = new StringBuffer();
	clusterPathSB.append(archetypeElementVO.getIdArchetype());
	for (String pathNode : pathArray) {
	    if (!pathNode.isEmpty()){
		clusterPathSB.append("/"+pathNode);
		ClusterVO clusterVO = Clusters.getClusterVO(archetypeElementVO.getIdTemplate(), clusterPathSB.toString());
		if (clusterVO!=null){
		    clusters.add(clusterVO);
		}
	    }
	}
	return clusters;
    }

    public static ArchetypeElements getDelegate(){
	if (_instance == null){
	    _instance = new ArchetypeElements();
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
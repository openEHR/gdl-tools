package se.cambio.cds.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.export.DVDefSerializer;
import se.cambio.openehr.controller.session.data.*;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class ArchetypeReferences {

    private static short MAX_CHAR_PREDICATE_DESC_SIZE = 50;

    public static String getName(ArchetypeReference ar){
        return getName(ar, true);
    }

    public static String getName(ArchetypeReference ar, boolean withPredicate){
        if (ar!=null) {
            return ar.getIdArchetype();
        } else {
            return "*UNKNOWN*";
        }
    }

    private static String getShortPredicateDescription(ArchetypeReference ar){
        String predicateDesc = getPredicateDescription(ar);
        if (predicateDesc.length()>MAX_CHAR_PREDICATE_DESC_SIZE){
            predicateDesc = predicateDesc.substring(0, MAX_CHAR_PREDICATE_DESC_SIZE)+"...";
        }
        return predicateDesc;
    }

    private static String getPredicateDescription(ArchetypeReference ar){
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (ElementInstance elementInstance : ar.getElementInstancesMap().values()) {
            if (elementInstance instanceof PredicateGeneratedElementInstance){
                if (!first){
                    sb.append(", ");
                }
                String name = ArchetypeManager.getInstance().getArchetypeElements().getText(ar.getIdTemplate(), elementInstance.getId(), UserConfigurationManager.getLanguage());
                if (name != null){
                    sb.append(name+"="+DVDefSerializer.getReadableValue(elementInstance.getDataValue(), null));
                    first = false;
                }else{
                    Logger.getLogger(ArchetypeReference.class).warn("Unknown predicate for AR '"+ar.toString()+"'");
                    sb.append("*UNKNOWN PREDICATE*");
                }
            }
        }
        return sb.toString();
    }

    public static String getDescription(ArchetypeReference ar){
        if (ar!=null) {
            return ar.getIdArchetype();
        } else {
            return "*UNKNOWN*";
        }
    }

    public static String getHTMLTooltip(ArchetypeReference ar){
        String archetypeImageName = OpenEHRConstUI.getIconName(Archetypes.getEntryType(ar.getIdArchetype()));
        String archetypeName = getName(ar, false);
        String predicateDesc = getPredicateDescription(ar);

        return "<html><table width=500>"+
                "<tr><td><b>"+OpenEHRLanguageManager.getMessage("Archetype")+": </b>"+OpenEHRImageUtil.getImgHTMLTag(archetypeImageName)+"&nbsp;"+archetypeName+"</td></tr>"+
                "<tr><td><b>"+OpenEHRLanguageManager.getMessage("Description")+": </b>"+getDescription(ar)+"</td></tr>"+
                (predicateDesc.isEmpty()?"":"<tr><td><b>"+OpenEHRLanguageManager.getMessage("Predicate")+": </b>"+predicateDesc+"</td></tr>")+
                "</table></html>";
    }


    //TODO Change location (Should be in ArchetypeElements class or similar)
    public static String getHTMLTooltip(ArchetypeElementVO archetypeElementVO, ArchetypeReference ar){
        return getHTMLTooltip(archetypeElementVO, ar, null);
    }

    public static String getHTMLTooltip(ArchetypeElementVO archetypeElementVO, ArchetypeReference ar, String extraLines){
        ArchetypeDTO archetypeVO = null;
        try {
            archetypeVO = ArchetypeManager.getInstance().getArchetypes().getCMElement(archetypeElementVO.getIdArchetype());
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }

        String idDomain = null;
        if (ar != null){
            idDomain = ar.getIdDomain();
        }

        String archetypeImageName = null;
        if (ar != null) {
            archetypeImageName = OpenEHRConstUI.getIconName(Archetypes.getEntryType(ar.getIdArchetype()));
        }
        String dataValueImageName = OpenEHRDataValuesUI.getDVIconName(archetypeElementVO.getRMType());

        String elementName = ArchetypeManager.getInstance().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
        String elementDesc = ArchetypeManager.getInstance().getArchetypeElements().getDescription(archetypeElementVO, UserConfigurationManager.getLanguage());

        String archetypeName = ArchetypeReferences.getName(ar)+(idDomain!=null?" ("+idDomain+")":"");
        String cardinalityStr = archetypeElementVO.getLowerCardinality()+"..."+(archetypeElementVO.getUpperCardinality()==null?"*":archetypeElementVO.getUpperCardinality());
        String units = null;
        if (OpenEHRDataValues.DV_QUANTITY.equals(archetypeElementVO.getRMType())){
            StringBuffer unitsSB = new StringBuffer();
            for (String unit : ArchetypeManager.getInstance().getUnits().getUnits(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId())) {
                unitsSB.append(unit+", ");
            }
            if (unitsSB.length() > 1){
                units = unitsSB.toString();
                units = units.substring(0, units.length()-2);
            }
        }

        String path = null;
        String[] pathArray = archetypeElementVO.getPath().split("\\/");
        StringBuffer pathSB = new StringBuffer();
        StringBuffer clusterPathSB = new StringBuffer();
        if (archetypeVO != null) {
            clusterPathSB.append(archetypeVO.getId());
        }
        for (String pathNode : pathArray) {
            if (!pathNode.isEmpty()){
                clusterPathSB.append("/"+pathNode);
                ClusterVO clusterVO = ArchetypeManager.getInstance().getClusters().getClusterVO(archetypeElementVO.getIdTemplate(), clusterPathSB.toString());
                if (clusterVO!=null){
                    String name = null;
                    try {
                        name = ArchetypeManager.getInstance().getClusters().getText(clusterVO, UserConfigurationManager.getLanguage());
                    } catch (InternalErrorException e) {
                        ExceptionHandler.handle(e);
                    }
                    pathSB.append(OpenEHRImageUtil.getImgHTMLTag(OpenEHRConstUI.getIconName(clusterVO.getRMType()))+"&nbsp;"+name+" / ");
                }
            }
        }
        if (pathSB.length()>0){
            path = pathSB.toString();
            path = path.substring(0, path.length()-3);
        }
        return "<html><table width=500>"+
                "<tr><td><b>"+OpenEHRLanguageManager.getMessage("Name")+": </b>"+elementName+"</td><td><b>"+OpenEHRLanguageManager.getMessage("Archetype")+": </b>"+OpenEHRImageUtil.getImgHTMLTag(archetypeImageName)+"&nbsp;"+archetypeName+"</td></tr>"+
                "<tr><td><b>"+OpenEHRLanguageManager.getMessage("DataValue")+": </b>"+OpenEHRImageUtil.getImgHTMLTag(dataValueImageName)+"&nbsp;"+OpenEHRDataValuesUI.getName(archetypeElementVO.getRMType())+"</td><td><b>"+OpenEHRLanguageManager.getMessage("Occurrences")+": </b>"+cardinalityStr+"</td></tr>"+
                (units!=null?"<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Units")+": </b>"+units+"</td></tr>":"")+
                "<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Path")+": </b>"+path+"</td></tr>"+
                "<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Description")+": </b>"+elementDesc+"</td></tr>"+
                (extraLines!=null?extraLines:"")+
                "</table></html>";
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
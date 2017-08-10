package se.cambio.cds.controller.session.data;

import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class ArchetypeReferencesManager {

    private ArchetypeManager archetypeManager;

    public ArchetypeReferencesManager(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
    }

    public static String getName(ArchetypeReference ar) {
        if (ar != null) {
            return ar.getIdArchetype();
        } else {
            return "*UNKNOWN*";
        }
    }

    public static String getDescription(ArchetypeReference ar) {
        if (ar != null) {
            return ar.getIdArchetype();
        } else {
            return "*UNKNOWN*";
        }
    }

    public String getHTMLTooltip(ArchetypeElementVO archetypeElementVO, ArchetypeReference ar) {
        return getHTMLTooltip(archetypeElementVO, ar, null);
    }

    public String getHTMLTooltip(ArchetypeElementVO archetypeElementVO, ArchetypeReference ar, String extraLines) {
        ArchetypeDTO archetypeDTO = getArchetypeDTO(archetypeElementVO);

        String idDomain = getDomainId(ar);

        String archetypeImageName = getArchetypeImageName(ar);
        String dataValueImageName = OpenEHRDataValuesUI.getDVIconName(archetypeElementVO.getRMType());

        String elementName = getElementName(archetypeElementVO);
        String elementDesc = getElementDescription(archetypeElementVO);

        String archetypeName = getArchetypeName(ar, idDomain);
        String cardinalityStr = getCardinalityDescription(archetypeElementVO);
        String units = getUnitsDescription(archetypeElementVO);

        String path = getTotalPath(archetypeElementVO, archetypeDTO);

        return "<html><table width=500>" +
                "<tr><td><b>" + OpenEHRLanguageManager.getMessage("Name") + ": </b>" + elementName + "</td><td><b>" + OpenEHRLanguageManager.getMessage("Archetype") + ": </b>" + OpenEHRImageUtil.getImgHTMLTag(archetypeImageName) + "&nbsp;" + archetypeName + "</td></tr>" +
                "<tr><td><b>" + OpenEHRLanguageManager.getMessage("DataValue") + ": </b>" + OpenEHRImageUtil.getImgHTMLTag(dataValueImageName) + "&nbsp;" + OpenEHRDataValuesUI.getName(archetypeElementVO.getRMType()) + "</td><td><b>" + OpenEHRLanguageManager.getMessage("Occurrences") + ": </b>" + cardinalityStr + "</td></tr>" +
                (units != null ? "<tr><td colspan=2><b>" + OpenEHRLanguageManager.getMessage("Units") + ": </b>" + units + "</td></tr>" : "") +
                "<tr><td colspan=2><b>" + OpenEHRLanguageManager.getMessage("Path") + ": </b>" + path + "</td></tr>" +
                "<tr><td colspan=2><b>" + OpenEHRLanguageManager.getMessage("Description") + ": </b>" + elementDesc + "</td></tr>" +
                (extraLines != null ? extraLines : "") +
                "</table></html>";
    }

    private String getTotalPath(ArchetypeElementVO archetypeElementVO, ArchetypeDTO archetypeDTO) {
        String path = null;
        String[] pathArray = archetypeElementVO.getPath().split("/");
        StringBuilder pathSB = new StringBuilder();
        StringBuilder clusterPathSB = new StringBuilder();
        if (archetypeDTO != null) {
            clusterPathSB.append(archetypeDTO.getId());
        }
        for (String pathNode : pathArray) {
            if (!pathNode.isEmpty()) {
                clusterPathSB.append("/").append(pathNode);
                if (archetypeManager.getClusters().isCluster(archetypeElementVO.getIdTemplate(), clusterPathSB.toString())) {
                    ClusterVO clusterVO = archetypeManager.getClusters().getClusterVO(archetypeElementVO.getIdTemplate(), clusterPathSB.toString());
                    String name = null;
                    try {
                        name = archetypeManager.getClusters().getText(clusterVO, archetypeManager.getUserConfigurationManager().getLanguage());
                    } catch (InternalErrorException e) {
                        ExceptionHandler.handle(e);
                    }
                    pathSB.append(OpenEHRImageUtil.getImgHTMLTag(OpenEHRConstUI.getIconName(clusterVO.getRMType()))).append("&nbsp;").append(name).append(" / ");
                }
            }
        }
        if (pathSB.length() > 0) {
            path = pathSB.toString();
            path = path.substring(0, path.length() - 3);
        }
        return path;
    }

    private ArchetypeDTO getArchetypeDTO(ArchetypeElementVO archetypeElementVO) {
        ArchetypeDTO archetypeVO = null;
        try {
            archetypeVO = archetypeManager.getArchetypes().getCMElement(archetypeElementVO.getIdArchetype());
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        return archetypeVO;
    }

    private static String getDomainId(ArchetypeReference ar) {
        String idDomain = null;
        if (ar != null) {
            idDomain = ar.getIdDomain();
        }
        return idDomain;
    }

    private static String getArchetypeImageName(ArchetypeReference ar) {
        String archetypeImageName = null;
        if (ar != null) {
            archetypeImageName = OpenEHRConstUI.getIconName(Archetypes.getEntryType(ar.getIdArchetype()));
        }
        return archetypeImageName;
    }

    private String getElementDescription(ArchetypeElementVO archetypeElementVO) {
        return archetypeManager.getArchetypeElements().getDescription(archetypeElementVO, archetypeManager.getUserConfigurationManager().getLanguage());
    }

    private String getElementName(ArchetypeElementVO archetypeElementVO) {
        return archetypeManager.getArchetypeElements().getText(archetypeElementVO, archetypeManager.getUserConfigurationManager().getLanguage());
    }

    private static String getArchetypeName(ArchetypeReference ar, String idDomain) {
        return ArchetypeReferencesManager.getName(ar) + (idDomain != null ? " (" + idDomain + ")" : "");
    }

    private static String getCardinalityDescription(ArchetypeElementVO archetypeElementVO) {
        return archetypeElementVO.getLowerCardinality() + "..." + (archetypeElementVO.getUpperCardinality() == null ? "*" : archetypeElementVO.getUpperCardinality());
    }

    private String getUnitsDescription(ArchetypeElementVO archetypeElementVO) {
        String units = null;
        if (OpenEHRDataValues.DV_QUANTITY.equals(archetypeElementVO.getRMType())) {
            StringBuilder unitsSB = new StringBuilder();
            for (String unit : archetypeManager.getUnits().getUnits(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId())) {
                unitsSB.append(unit).append(", ");
            }
            if (unitsSB.length() > 1) {
                units = unitsSB.toString();
                units = units.substring(0, units.length() - 2);
            }
        }
        return units;
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
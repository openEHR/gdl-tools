package se.cambio.openehr.controller.session.data;

import org.openehr.rm.datatypes.quantity.ProportionKind;
import se.cambio.openehr.model.archetype.vo.ProportionTypeVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class ProportionTypesUI {
    private Map<String, Collection<ProportionKind>> _proportionTypesByIdElement = null;
    private Map<String, Map<String, Collection<ProportionKind>>> _templateProportionTypesByTemplateIdAndId = null;

    private Map<ProportionKind, String> _proportionTypeNames = null;
    private Map<ProportionKind, String> _proportionTypeDescriptions = null;
    private Map<ProportionKind, String> _proportionTypeIDs = null;

    private String RATIO_ID = "RATIO";
    private String UNITARY_ID = "UNITARY";
    private String PERCENT_ID = "PERCENT";
    private String FRACTION_ID = "FRACTION";
    private String INTEGER_FRACTION_ID = "INTEGER_FRACTION";

    public ProportionTypesUI(){
        init();
    }

    public void init(){
        _proportionTypesByIdElement = new HashMap<String, Collection<ProportionKind>>();
        _templateProportionTypesByTemplateIdAndId = new HashMap<String, Map<String,Collection<ProportionKind>>>();
        _proportionTypeIDs = new HashMap<ProportionKind, String>();

        _proportionTypeNames = new HashMap<ProportionKind, String>();
        _proportionTypeDescriptions = new HashMap<ProportionKind, String>();
        _proportionTypeNames.put(ProportionKind.RATIO, OpenEHRLanguageManager.getMessage("Ratio"));
        _proportionTypeNames.put(ProportionKind.UNITARY, OpenEHRLanguageManager.getMessage("Unitary"));
        _proportionTypeNames.put(ProportionKind.PERCENT, OpenEHRLanguageManager.getMessage("Percent"));
        _proportionTypeNames.put(ProportionKind.FRACTION, OpenEHRLanguageManager.getMessage("Fraction"));
        _proportionTypeNames.put(ProportionKind.INTEGER_FRACTION, OpenEHRLanguageManager.getMessage("IntegerFraction"));

        _proportionTypeDescriptions.put(ProportionKind.RATIO, OpenEHRLanguageManager.getMessage("RatioDesc"));
        _proportionTypeDescriptions.put(ProportionKind.UNITARY, OpenEHRLanguageManager.getMessage("UnitaryDesc"));
        _proportionTypeDescriptions.put(ProportionKind.PERCENT, OpenEHRLanguageManager.getMessage("PercentDesc"));
        _proportionTypeDescriptions.put(ProportionKind.FRACTION, OpenEHRLanguageManager.getMessage("FractionDesc"));
        _proportionTypeDescriptions.put(ProportionKind.INTEGER_FRACTION, OpenEHRLanguageManager.getMessage("IntegerFractionDesc"));

        _proportionTypeIDs.put(ProportionKind.RATIO, ProportionKind.class.getSimpleName()+"."+RATIO_ID);
        _proportionTypeIDs.put(ProportionKind.UNITARY, ProportionKind.class.getSimpleName()+"."+UNITARY_ID);
        _proportionTypeIDs.put(ProportionKind.PERCENT, ProportionKind.class.getSimpleName()+"."+PERCENT_ID);
        _proportionTypeIDs.put(ProportionKind.FRACTION, ProportionKind.class.getSimpleName()+"."+FRACTION_ID);
        _proportionTypeIDs.put(ProportionKind.INTEGER_FRACTION, ProportionKind.class.getSimpleName()+"."+INTEGER_FRACTION_ID);
    }

    public void loadProportionTypes(Collection<ProportionTypeVO> proportionTypeVOs){
        for (ProportionTypeVO proportionTypeVO : proportionTypeVOs) {
            registerProportionType(proportionTypeVO);
        }
    }

    public void registerProportionType(ProportionTypeVO proportionTypeVO){
        if (proportionTypeVO.getIdTemplate()==null){
            getProportionTypes(proportionTypeVO.getIdElement()).add(ProportionKind.fromValue(proportionTypeVO.getType()));
        }else{
            getProportionTypes(proportionTypeVO.getIdTemplate(), proportionTypeVO.getIdElement()).add(ProportionKind.fromValue(proportionTypeVO.getType()));
        }
    }

    private Map<String, Collection<ProportionKind>> getProportionTypesInTemplate(String idTemplate){
        Map<String, Collection<ProportionKind>> map = _templateProportionTypesByTemplateIdAndId.get(idTemplate);
        if (map==null){
            map = new HashMap<String, Collection<ProportionKind>>();
            _templateProportionTypesByTemplateIdAndId.put(idTemplate, map);
        }
        return map;
    }

    public Collection<ProportionKind> getProportionTypes(String idTemplate, String idElement){
        if (idTemplate==null){
            return getProportionTypes(idElement);
        }else{
            Collection<ProportionKind> proportionTypes = getProportionTypesInTemplate(idTemplate).get(idElement);
            if (proportionTypes==null){
                proportionTypes = new ArrayList<ProportionKind>();
                getProportionTypesInTemplate(idTemplate).put(idElement, proportionTypes);
            }
            return proportionTypes;
        }
    }

    private Collection<ProportionKind> getProportionTypes(String idElement){
        Collection<ProportionKind> proportionTypes = _proportionTypesByIdElement.get(idElement);
        if (proportionTypes==null){
            proportionTypes = new ArrayList<ProportionKind>();
            _proportionTypesByIdElement.put(idElement, proportionTypes);
        }
        return proportionTypes;
    }

    public String getName(ProportionKind proportionType){
        return _proportionTypeNames.get(proportionType);
    }

    public String getDescription(ProportionKind proportionType){
        return _proportionTypeDescriptions.get(proportionType);
    }

    public String getInstanceID(ProportionKind proportionKind){
        return _proportionTypeIDs.get(proportionKind);
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
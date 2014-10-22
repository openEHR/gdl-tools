package se.cambio.cds.gdl.model.readable.util;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineDefinitionElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ExpressionRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.PredicateArchetypeElementAttributeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.PredicateAttributeComparisonOperatorRuleLineElement;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.export.json.DVDefSerializer;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class ReadableArchetypeReferencesUtil {

    private static short MAX_CHAR_PREDICATE_DESC_SIZE = 50;

    public static String getName(ArchetypeInstantiationRuleLine airl) throws InstanceNotFoundException, InternalErrorException {
        return getName(airl, true);
    }
    public static String getName(ArchetypeInstantiationRuleLine airl, boolean withPredicate) {
        if (airl!=null){
            ArchetypeReference ar = airl.getArchetypeReference();
            if (ar!=null){
                String name = ar.getIdArchetype();
                if (withPredicate){
                    String predicateDesc = getShortPredicateDescription(airl);
                    if (!predicateDesc.isEmpty()){
                        name = name+" ("+predicateDesc+")";
                    }
                }
                return name;
            }
        }
        return "*UNKNOWN*";
    }

    private static String getShortPredicateDescription(ArchetypeInstantiationRuleLine airl){
        String predicateDesc = getPredicateDescription(airl);
        if (predicateDesc.length()>MAX_CHAR_PREDICATE_DESC_SIZE){
            predicateDesc = predicateDesc.substring(0, MAX_CHAR_PREDICATE_DESC_SIZE)+"...";
        }
        return predicateDesc;
    }

    private static String getPredicateDescription(ArchetypeInstantiationRuleLine airl){
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (RuleLine ruleLine : airl.getChildrenRuleLines()) {
            if (ruleLine instanceof WithElementPredicateAttributeDefinitionRuleLine){
                WithElementPredicateAttributeDefinitionRuleLine wpadrl = (WithElementPredicateAttributeDefinitionRuleLine)ruleLine;
                if (first){
                    first = false;
                }else{
                    sb.append(", ");
                }
                ArchetypeElementRuleLineDefinitionElement aerlde = wpadrl.getArchetypeElementRuleLineDefinitionElement();
                if (aerlde!=null){
                    ArchetypeElementVO archetypeElementVO = aerlde.getValue();
                    if (archetypeElementVO!=null){
                        String name = aerlde.getArchetypeManager().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
                        sb.append(name+"="+DVDefSerializer.getReadableValue(wpadrl.getDataValueRuleLineElement().getValue(), null));
                    }else{
                        Logger.getLogger(ArchetypeReference.class).warn("Unknown predicate for AR '"+aerlde.toString()+"'");
                        sb.append("*UNKNOWN PREDICATE*");
                    }
                }
            } else if (ruleLine instanceof WithElementPredicateFunctionDefinitionRuleLine){
                WithElementPredicateFunctionDefinitionRuleLine wpfdrl = (WithElementPredicateFunctionDefinitionRuleLine)ruleLine;
                if (first){
                    first = false;
                }else{
                    sb.append(", ");
                }
                ArchetypeElementRuleLineDefinitionElement aerlde = wpfdrl.getArchetypeElementRuleLineDefinitionElement();
                if (aerlde!=null){
                    ArchetypeElementVO archetypeElementVO = aerlde.getValue();
                    if (archetypeElementVO!=null){
                        String name = aerlde.getArchetypeManager().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
                        sb.append(wpfdrl.getFunctionRuleLineElement().getValue()+"("+name+")");
                    }else{
                        Logger.getLogger(ArchetypeReference.class).warn("Unknown predicate for AR '"+aerlde.toString()+"'");
                        sb.append("*UNKNOWN PREDICATE*");
                    }
                }
            } else if (ruleLine instanceof WithElementPredicateExistsDefinitionRuleLine){
                WithElementPredicateExistsDefinitionRuleLine wpedrl = (WithElementPredicateExistsDefinitionRuleLine)ruleLine;
                if (first){
                    first = false;
                }else{
                    sb.append(", ");
                }
                ArchetypeElementRuleLineDefinitionElement aerlde = wpedrl.getArchetypeElementRuleLineDefinitionElement();
                if (aerlde!=null){
                    ArchetypeElementVO archetypeElementVO = aerlde.getValue();
                    if (archetypeElementVO!=null){
                        OperatorKind operator = wpedrl.getExistenceOperatorRuleLineElement().getOperator();
                        String opStr = "??";
                        if (operator!=null){
                            opStr = operator.getSymbol();
                        }
                        String name = aerlde.getArchetypeManager().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
                        sb.append(name+opStr+"null");
                    }else{
                        Logger.getLogger(ArchetypeReference.class).warn("Unknown predicate for AR '"+aerlde.toString()+"'");
                        sb.append("*UNKNOWN PREDICATE*");
                    }
                }
            } else if (ruleLine instanceof WithElementPredicateExpressionDefinitionRuleLine){
                WithElementPredicateExpressionDefinitionRuleLine wepedrl = (WithElementPredicateExpressionDefinitionRuleLine)ruleLine;
                if (first){
                    first = false;
                }else{
                    sb.append(", ");
                }
                PredicateArchetypeElementAttributeRuleLineElement paearle = wepedrl.getArchetypeElementAttributeRuleLineDefinitionElement();
                PredicateAttributeComparisonOperatorRuleLineElement pacorl = wepedrl.getComparisonOperatorRuleLineElement();
                ExpressionRuleLineElement ere = wepedrl.getExpressionRuleLineElement();
                if (paearle!=null){
                    ArchetypeElementVO archetypeElementVO = paearle.getValue();
                    String attribute = paearle.getAttribute();
                    if (archetypeElementVO!=null && pacorl.getValue()!=null){
                        String name = airl.getArchetypeManager().getArchetypeElements().getText(archetypeElementVO, UserConfigurationManager.getLanguage());
                        sb.append(name+"."+attribute+" "+pacorl.getValue().getSymbol()+" "+ere.toString());
                    }else{
                        Logger.getLogger(ArchetypeReference.class).warn("Unknown predicate for AR '"+paearle.toString()+"'");
                        sb.append("*UNKNOWN PREDICATE*");
                    }
                }
            }
        }
        return sb.toString();
    }

    public static String getDescription(ArchetypeInstantiationRuleLine airl) {
        if (airl!=null){
            ArchetypeReference ar = airl.getArchetypeReference();
            if (ar!=null){
                return ar.getIdArchetype();
            }
        }
        return "*UNKNOWN*";
    }

    public static String getHTMLPredicate(ArchetypeInstantiationRuleLine airl){
        String predicateDesc = getPredicateDescription(airl);
        return (predicateDesc.isEmpty()?"":"<tr><td colspan=2><b>"+OpenEHRLanguageManager.getMessage("Predicate")+": </b>"+predicateDesc+"</td></tr>");
    }

    public static String getHTMLTooltip(ArchetypeInstantiationRuleLine airl) {
        ArchetypeReference ar = airl.getArchetypeReference();
        if (ar!=null){
            String archetypeImageName = OpenEHRConstUI.getIconName(Archetypes.getEntryType(ar.getIdArchetype()));
            String archetypeName = getName(airl, false);
            return "<html><table width=500>"+
                    "<tr><td><b>"+OpenEHRLanguageManager.getMessage("Archetype")+": </b>"+OpenEHRImageUtil.getImgHTMLTag(archetypeImageName)+"&nbsp;"+archetypeName+"</td></tr>"+
                    "<tr><td><b>"+OpenEHRLanguageManager.getMessage("Description")+": </b>"+getDescription(airl)+"</td></tr>"+
                    getHTMLPredicate(airl)+
                    "</table></html>";
        }else{
            return "*UNKNOWN*";
        }
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
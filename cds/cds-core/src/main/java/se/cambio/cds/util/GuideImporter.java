package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.DataValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.util.RulePriorityComparator;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.CodedTexts;
import se.cambio.openehr.controller.session.data.Ordinals;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class GuideImporter {


    private GuideImporter(){

    }

    public static ReadableGuide importGuide(Guide guide, String language) throws InternalErrorException {
        Logger.getLogger(GuideImporter.class).debug("Importing guide: "+guide.getId()+", lang="+language);
        Map<String, ArchetypeElementInstantiationRuleLine> gtCodeElementMap =
                new HashMap<String, ArchetypeElementInstantiationRuleLine>();
        ArchetypeElementInstantiationRuleLine dummyAEIRL = new ArchetypeElementInstantiationRuleLine(new ArchetypeInstantiationRuleLine());
        dummyAEIRL.setGTCode("currentDateTime");
        gtCodeElementMap.put("currentDateTime", dummyAEIRL);
        GuideDefinition guideDefinition = guide.getDefinition();
        TermDefinition termDefinition = getTermDefinition(guide, language);
        ReadableGuide readableGuide = new ReadableGuide(termDefinition);
        if (guideDefinition!=null){
            List<ArchetypeBinding> ab = guideDefinition.getArchetypeBindings();
            if (ab!=null){
                for (ArchetypeBinding archetypeBinding: ab) {
                    ArchetypeInstantiationRuleLine airl =
                            new ArchetypeInstantiationRuleLine();
                    ArchetypeReference ar =
                            new ArchetypeReference(
                                    archetypeBinding.getDomain(),
                                    archetypeBinding.getArchetypeId(),
                                    archetypeBinding.getTemplateId());
                    airl.setArchetypeReference(ar);
                    readableGuide.getDefinitionRuleLines().add(airl);
                    if (archetypeBinding.getElements()!=null){
                        for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                            ArchetypeElementInstantiationRuleLine aeirl =
                                    new ArchetypeElementInstantiationRuleLine(airl);
                            aeirl.setGTCode(elementBinding.getId());
                            String elementId =
                                    archetypeBinding.getArchetypeId()+elementBinding.getPath();
                            ArchetypeElementVO archetypeElementVO =
                                    ArchetypeElements.getArchetypeElement(
                                            archetypeBinding.getTemplateId(),
                                            elementId);

                            log.debug("elementId: " + elementId + ", archetypeElementVO: " + archetypeElementVO);

                            aeirl.setArchetypeElementVO(archetypeElementVO);
                            gtCodeElementMap.put(elementBinding.getId(), aeirl);
                        }
                    }
                    if (archetypeBinding.getPredicateStatements()!=null){
                        for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
                            if (expressionItem instanceof BinaryExpression){
                                BinaryExpression binaryExpression = (BinaryExpression)expressionItem;
                                if (binaryExpression.getLeft() instanceof Variable &&
                                        binaryExpression.getRight() instanceof ConstantExpression){
                                    Variable variable = (Variable)binaryExpression.getLeft();
                                    ConstantExpression constantExpression2 = (ConstantExpression)binaryExpression.getRight();
                                    String path = variable.getPath();
                                    String dvStr = constantExpression2.getValue();
                                    ArchetypeElementVO archetypeElementVO =
                                            ArchetypeElements.getArchetypeElement(
                                                    archetypeBinding.getTemplateId(),
                                                    archetypeBinding.getArchetypeId()+path);
                                    if (!dvStr.equals("null")){
                                        WithElementPredicateAttributeDefinitionRuleLine wepdrl = new WithElementPredicateAttributeDefinitionRuleLine();
                                        airl.addChildRuleLine(wepdrl);
                                        wepdrl.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
                                        String rmType = archetypeElementVO.getRMType();
                                        if (OpenEHRDataValues.DV_TEXT.equals(rmType) &&
                                                (OperatorKind.IS_A.equals(binaryExpression.getOperator()) || OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()))){
                                            rmType = OpenEHRDataValues.DV_CODED_TEXT;
                                        }
                                        DataValue dv = parseDataValue(rmType, dvStr, archetypeElementVO);
                                        wepdrl.getDataValueRuleLineElement().setValue(dv);
                                        wepdrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                                    }else{
                                       WithElementPredicateExistsDefinitionRuleLine wepedrl = new WithElementPredicateExistsDefinitionRuleLine();
                                        airl.addChildRuleLine(wepedrl);
                                        wepedrl.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
                                        wepedrl.getExistenceOperatorRuleLineElement().setValue(binaryExpression.getOperator().getSymbol()+"null");
                                    }


                                }else if (binaryExpression.getLeft() instanceof Variable &&
                                        binaryExpression.getRight() instanceof ExpressionItem){
                                    Variable variable = (Variable)binaryExpression.getLeft();
                                    ExpressionItem expressionItemAux = binaryExpression.getRight();
                                    WithElementPredicateExpressionDefinitionRuleLine wepdrl = new WithElementPredicateExpressionDefinitionRuleLine(airl);
                                    String path = variable.getPath();
                                    String attribute = path.substring(path.lastIndexOf("/value/") + 7, path.length());
                                    path = path.substring(0, path.length()-attribute.length()-7);
                                    ArchetypeElementVO archetypeElementVO =
                                            ArchetypeElements.getArchetypeElement(
                                                    archetypeBinding.getTemplateId(),
                                                    archetypeBinding.getArchetypeId()+path);
                                    wepdrl.getArchetypeElementAttributeRuleLineDefinitionElement().setValue(archetypeElementVO);
                                    wepdrl.getArchetypeElementAttributeRuleLineDefinitionElement().setAttribute(attribute);
                                    wepdrl.getExpressionRuleLineElement().setValue(expressionItemAux);
                                    wepdrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                                }
                            }else if (expressionItem instanceof UnaryExpression){
                                UnaryExpression unaryExpression = (UnaryExpression)expressionItem;
                                WithElementPredicateFunctionDefinitionRuleLine wefd = new WithElementPredicateFunctionDefinitionRuleLine();
                                Variable variable = (Variable)unaryExpression.getOperand();
                                airl.addChildRuleLine(wefd);
                                String path = variable.getPath();
                                ArchetypeElementVO archetypeElementVO =
                                        ArchetypeElements.getArchetypeElement(
                                                archetypeBinding.getTemplateId(),
                                                archetypeBinding.getArchetypeId()+path);
                                wefd.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
                                wefd.getFunctionRuleLineElement().setValue(unaryExpression.getOperator());
                            }
                        }
                    }
                }
            }
            if (guideDefinition.getPreConditionExpressions()!=null){
                for (ExpressionItem expressionItem : guideDefinition.getPreConditionExpressions()) {
                    processExpressionItem(
                            readableGuide.getPreconditionRuleLines(), null,
                            expressionItem, gtCodeElementMap);
                }
            }

            Map<String, Rule> rulesMap = guideDefinition.getRules();
            if (rulesMap!=null){
                List<Rule> rules = new ArrayList<Rule>(rulesMap.values());
                Collections.sort(rules, new RulePriorityComparator());
                for (Rule rule : rules) {
                    ReadableRule rr = new ReadableRule(readableGuide.getTermDefinition(), rule.getId());
                    readableGuide.getReadableRules().put(rule.getId(), rr);
                    if (rule.getWhenStatements()!=null){
                        for (ExpressionItem expressionItem : rule.getWhenStatements()) {
                            processExpressionItem(
                                    rr.getConditionRuleLines(), null,
                                    expressionItem, gtCodeElementMap);
                        }
                    }
                    if (rule.getThenStatements()!=null){
                        for (ExpressionItem expressionItem : rule.getThenStatements()) {
                            processExpressionItem(
                                    rr.getActionRuleLines(), null,
                                    expressionItem, gtCodeElementMap);
                        }
                    }
                }
            }
        }
        updateTermDefinitions(readableGuide, termDefinition);
        return readableGuide;
    }

    public static void updateTermDefinitions(ReadableGuide readableGuide, TermDefinition termDefinition){
        setTermDefinitionsOnRuleLines(readableGuide.getDefinitionRuleLines(), termDefinition);
        setTermDefinitionsOnRuleLines(readableGuide.getPreconditionRuleLines(), termDefinition);
        for (ReadableRule readableRule : readableGuide.getReadableRules().values()) {
            setTermDefinitionsOnRuleLines(readableRule.getConditionRuleLines(), termDefinition);
            setTermDefinitionsOnRuleLines(readableRule.getActionRuleLines(), termDefinition);
        }
    }

    public static TermDefinition getTermDefinition(Guide guide, String lang){
        TermDefinition termDefinition = null;
        if (guide.getOntology()!=null){
            if (guide.getOntology().getTermDefinitions()!=null){
                termDefinition = guide.getOntology().getTermDefinitions().get(lang);
                if (termDefinition==null){
                    termDefinition = guide.getOntology().getTermDefinitions().get(guide.getLanguage().getOriginalLanguage().getCodeString());
                }
            }
        }
        if (termDefinition==null){
            termDefinition = new TermDefinition();
        }
        return termDefinition;
    }

    private static void setTermDefinitionsOnRuleLines(Collection<RuleLine> ruleLines, TermDefinition termDefinition){
        for (RuleLine ruleLine : ruleLines) {
            ruleLine.setTermDefinition(termDefinition);
            setTermDefinitionsOnRuleLines(ruleLine.getChildrenRuleLines(), termDefinition);
        }
    }

    private static void addRuleLine(RuleLine ruleLine, Collection<RuleLine> ruleLines, RuleLine parentRuleLine){
        if(parentRuleLine!=null){
            parentRuleLine.addChildRuleLine(ruleLine);
        }else{
            ruleLines.add(ruleLine);
        }
    }

    protected static DataValue parseDataValue(String rmType, String dvStr,  ArchetypeElementVO archetypeElementVO){
        DataValue dv = DataValue.parseValue(rmType+","+dvStr);
        if (dv instanceof DvCodedText){
            if (archetypeElementVO!=null){
                DvCodedText dvCT = (DvCodedText)dv;
                String name = CodedTexts.getName(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), dvCT);
                if (name!=null){
                    dvCT.setValue(name);
                }
            }
        }else if (dv instanceof DvOrdinal){
            if (archetypeElementVO!=null){
                DvOrdinal dvOrdinal= (DvOrdinal)dv;
                String name = Ordinals.getName(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), dvOrdinal);
                if (name!=null){
                    dvOrdinal.getSymbol().setValue(name);
                }
            }
        }
        return dv;
    }

    protected static void processAssigmentExpression(
            Collection<RuleLine> ruleLines,
            AssignmentExpression assignmentExpression,
            Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap) throws InternalErrorException {
        String gtCode = assignmentExpression.getVariable().getCode();
        ExpressionItem expressionItemAux = assignmentExpression.getAssignment();
        String attribute = assignmentExpression.getVariable().getAttribute();
        GTCodeRuleLineElement gtCodeRuleLineElement =
                gtCodeELementMap.get(gtCode).getGTCodeRuleLineElement();
        if (attribute==null){
            if (expressionItemAux instanceof Variable){
                String gtCodeAux = ((Variable)expressionItemAux).getCode();
                SetElementWithElementActionRuleLine sewearl = new SetElementWithElementActionRuleLine();
                sewearl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                GTCodeRuleLineElement gtCodeRuleLineElementAux =
                        gtCodeELementMap.get(gtCodeAux).getGTCodeRuleLineElement();
                sewearl.getSecondArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElementAux);
                ruleLines.add(sewearl);
            }else if (expressionItemAux instanceof ConstantExpression){
                SetElementWithDataValueActionRuleLine sedvar = new SetElementWithDataValueActionRuleLine();
                sedvar.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                DataValueRuleLineElement dataValueRuleLineElement = sedvar.getDataValueRuleLineElement();
                String dvStr = ((ConstantExpression)expressionItemAux).getValue();
                ArchetypeElementVO archetypeElementVO = gtCodeELementMap.get(gtCode).getArchetypeElement();
                if (archetypeElementVO==null){
                    throw new InternalErrorException(new Exception("Archetype element not found for gtCode '"+gtCode+"'"));
                }
                log.debug("processAssigmentExpression for varialbe: " + gtCode);

                String rmType = archetypeElementVO.getRMType();
                DataValue dv = parseDataValue(rmType, dvStr, archetypeElementVO);
                dataValueRuleLineElement.setValue(dv);
                ruleLines.add(sedvar);
            }else{
                throw new InternalErrorException(new Exception("Unknown expression '"+expressionItemAux.getClass().getName()+"'"));
            }
        }else{
            SetElementAttributeActionRuleLine seaarl = new SetElementAttributeActionRuleLine();
            seaarl.getArchetypeElementAttributeRuleLineElement().setAttribute(attribute);
            ArchetypeElementRuleLineElement aerle = new ArchetypeElementRuleLineElement(seaarl);
            aerle.setValue(gtCodeRuleLineElement);
            seaarl.getArchetypeElementAttributeRuleLineElement().setValue(aerle);
            seaarl.getExpressionRuleLineElement().setValue(expressionItemAux);
            ruleLines.add(seaarl);
        }
    }

    protected static void processBinaryExpression(
            Collection<RuleLine> ruleLines,
            RuleLine parentRuleLine,
            BinaryExpression binaryExpression,
            Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap) throws InternalErrorException {
        if (OperatorKind.OR.equals(binaryExpression.getOperator())){
            OrOperatorRuleLine orOperatorRuleLine = new OrOperatorRuleLine();
            processExpressionItem(ruleLines, orOperatorRuleLine.getLeftRuleLineBranch(), binaryExpression.getLeft(), gtCodeELementMap);
            processExpressionItem(ruleLines, orOperatorRuleLine.getRightRuleLineBranch(), binaryExpression.getRight(), gtCodeELementMap);
            addRuleLine(orOperatorRuleLine, ruleLines, parentRuleLine);
        }else if (OperatorKind.AND.equals(binaryExpression.getOperator())){
            processExpressionItem(ruleLines, parentRuleLine, binaryExpression.getLeft(), gtCodeELementMap);
            processExpressionItem(ruleLines, parentRuleLine, binaryExpression.getRight(), gtCodeELementMap);
        }else if (OperatorKind.EQUALITY.equals(binaryExpression.getOperator())||
                OperatorKind.INEQUAL.equals(binaryExpression.getOperator())||
                OperatorKind.IS_A.equals(binaryExpression.getOperator())||
                OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator())||
                OperatorKind.GREATER_THAN.equals(binaryExpression.getOperator())||
                OperatorKind.GREATER_THAN_OR_EQUAL.equals(binaryExpression.getOperator())||
                OperatorKind.LESS_THAN.equals(binaryExpression.getOperator())||
                OperatorKind.LESS_THAN_OR_EQUAL.equals(binaryExpression.getOperator())){
            processComparisonExpression(ruleLines, parentRuleLine, binaryExpression, gtCodeELementMap);
        }else{
            throw new InternalErrorException(new Exception("Unknown operator '"+binaryExpression.getOperator()+"'"));
        }

    }

    protected static void processComparisonExpression(
            Collection<RuleLine> ruleLines,
            RuleLine parentRuleLine,
            BinaryExpression binaryExpression,
            Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap) throws InternalErrorException {
        GTCodeRuleLineElement gtCodeRuleLineElement = null;
        String attribute = null;
        String gtCode = null;
        if (binaryExpression.getLeft() instanceof Variable){
            Variable var = (Variable)binaryExpression.getLeft();
            gtCode = var.getCode();

            ArchetypeElementInstantiationRuleLine aeirl = gtCodeELementMap.get(gtCode);
            if (aeirl!=null){
                gtCodeRuleLineElement = aeirl.getGTCodeRuleLineElement();
            }else{
                log.warn("gtCode not found! ("+gtCode+")");
            }
            attribute = var.getAttribute();
        }
        if (gtCodeRuleLineElement!=null){
            if (attribute==null){
                if (binaryExpression.getRight() instanceof ConstantExpression){
                    ConstantExpression constantExpression = (ConstantExpression)binaryExpression.getRight();
                    String dvStr = constantExpression.getValue();
                    DataValue dv = null;
                    if (!dvStr.equals("null")){
                        log.debug("codeRuleLineElement: " + gtCodeRuleLineElement.getValue());
                        String rmType = null;
                        ArchetypeElementVO archetypeElementVO = null;
                        if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)){
                            archetypeElementVO = gtCodeELementMap.get(gtCodeRuleLineElement.getValue()).getArchetypeElement();
                            if (archetypeElementVO==null){
                                throw new InternalErrorException(new Exception("Archetype element not found for gtCode '"+gtCodeRuleLineElement.getValue()+"'"));
                            }
                            rmType = archetypeElementVO.getRMType();
                            if (OpenEHRDataValues.DV_TEXT.equals(rmType) &&
                                    (OperatorKind.IS_A.equals(binaryExpression.getOperator()) || OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()))){
                                rmType = OpenEHRDataValues.DV_CODED_TEXT;
                            }
                        }else{
                            rmType = OpenEHRDataValues.DV_DATE_TIME;
                        }
                        dv = parseDataValue(rmType, dvStr, archetypeElementVO);
                    }
                    if (dv!=null){
                        ElementComparisonWithDVConditionRuleLine eccrl = new ElementComparisonWithDVConditionRuleLine();
                        eccrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                        eccrl.getDataValueRuleLineElement().setValue(dv);
                        eccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                        addRuleLine(eccrl, ruleLines, parentRuleLine);
                    }else{
                        ElementInitializedConditionRuleLine eicrl = new ElementInitializedConditionRuleLine();
                        eicrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                        eicrl.getExistenceOperatorRuleLineElement().setOperator(binaryExpression.getOperator());
                        addRuleLine(eicrl, ruleLines, parentRuleLine);
                    }
                }else if (binaryExpression.getRight() instanceof Variable){
                    Variable varRight = (Variable)binaryExpression.getRight();
                    String gtCodeAux = varRight.getCode();
                    ElementComparisonWithElementConditionRuleLine eccrl = new ElementComparisonWithElementConditionRuleLine();
                    eccrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                    eccrl.getSecondArchetypeElementRuleLineElement().setValue(gtCodeELementMap.get(gtCodeAux).getGTCodeRuleLineElement());
                    eccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                    addRuleLine(eccrl, ruleLines, parentRuleLine);
                }else{
                    throw new InternalErrorException(new Exception("Unknown expression '"+binaryExpression.getRight().getClass().getName()+"'"));
                }
            }else{

                if (attribute.equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)){

                    ElementComparisonWithNullValueConditionRuleLine ecwnvc = new ElementComparisonWithNullValueConditionRuleLine();
                    ecwnvc.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                    ConstantExpression constantExpression = (ConstantExpression)binaryExpression.getRight();
                    String dvStr = constantExpression.getValue();
                    DataValue dv = parseDataValue(OpenEHRDataValues.DV_CODED_TEXT, dvStr, null);
                    if (dv instanceof DvCodedText){
                        ecwnvc.getNullValueRuleLineElement().setValue((DvCodedText)dv);
                    }
                    ecwnvc.getEqualityComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                    addRuleLine(ecwnvc, ruleLines, parentRuleLine);
                }else{//Expression
                    ElementAttributeComparisonConditionRuleLine eaccrl = new ElementAttributeComparisonConditionRuleLine();
                    eaccrl.getArchetypeElementAttributeRuleLineElement().setAttribute(attribute);
                    ArchetypeElementRuleLineElement aerle = new ArchetypeElementRuleLineElement(eaccrl);
                    aerle.setValue(gtCodeRuleLineElement);
                    eaccrl.getArchetypeElementAttributeRuleLineElement().setValue(aerle);
                    eaccrl.getExpressionRuleLineElement().setValue(binaryExpression.getRight());
                    eaccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                    addRuleLine(eaccrl, ruleLines, parentRuleLine);
                }
            }
        }else{
            throw new InternalErrorException(new Exception("Unknown expression '"+binaryExpression.getLeft().getClass().getName()+"'"));
        }
    }

    protected static void processExpressionItem(
            Collection<RuleLine> ruleLines,
            RuleLine parentRuleLine,
            ExpressionItem expressionItem,
            Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap) throws InternalErrorException {
        if (expressionItem instanceof AssignmentExpression){
            processAssigmentExpression(ruleLines, (AssignmentExpression)expressionItem, gtCodeELementMap);
        }else if (expressionItem instanceof BinaryExpression){
            processBinaryExpression(ruleLines, parentRuleLine, (BinaryExpression)expressionItem, gtCodeELementMap);
        }else if (expressionItem instanceof UnaryExpression){
            processUnaryExpression(ruleLines, parentRuleLine, (UnaryExpression)expressionItem, gtCodeELementMap);
        }else{
            throw new InternalErrorException(new Exception("Unknown expression '"+expressionItem.getClass().getName()+"'"));
        }
    }

    protected static void processUnaryExpression(
            Collection<RuleLine> ruleLines,
            RuleLine parentRuleLine,
            UnaryExpression unaryExpression,
            Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap) throws InternalErrorException {
        if (OperatorKind.FOR_ALL.equals(unaryExpression.getOperator())){
            ForAllOperatorRuleLine forAllOperatorRuleLine = new ForAllOperatorRuleLine();
            processExpressionItem(ruleLines, forAllOperatorRuleLine, unaryExpression.getOperand(), gtCodeELementMap);
            addRuleLine(forAllOperatorRuleLine, ruleLines, parentRuleLine);
        }else{
            throw new InternalErrorException(new Exception("Unknown operator '"+unaryExpression.getOperator()+"'"));
        }
    }


    private static Logger log = Logger.getLogger(GuideImporter.class);
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
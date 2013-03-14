package se.cambio.cds.gdl.model.readable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.GuideDefinition;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.expression.AssignmentExpression;
import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ConstantExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.gdl.model.expression.UnaryExpression;
import se.cambio.cds.gdl.model.expression.Variable;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ElementAttributeComparisonConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ElementComparisonWithDVConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ElementComparisonWithElementConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ElementComparisonWithNullValueConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ElementInitializedConditionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.ForAllOperatorRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.OrOperatorRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.SetElementAttributeActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.SetElementWithDataValueActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.SetElementWithElementActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.WithElementPredicateAttributeDefinitionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.WithElementPredicateExpressionDefinitionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.DataValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.util.RulePriorityComparator;
import se.cambio.cds.model.facade.execution.vo.ArchetypeReference;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.util.OpenEHRConst;
import se.cambio.cds.openehr.view.applicationobjects.ArchetypeElements;
import se.cambio.cds.util.OpenEHRDataValues;

public class GuideImporter {


    private GuideImporter(){

    }

    public static ReadableGuide importGuide(Guide guide, String language){
	Logger.getLogger(GuideImporter.class).debug("Importing guide: "+guide.getId()+", lang="+language);
	Map<String, ArchetypeElementInstantiationRuleLine> gtCodeElementMap = 
		new HashMap<String, ArchetypeElementInstantiationRuleLine>();
	ArchetypeElementInstantiationRuleLine dummyAEIRL = new ArchetypeElementInstantiationRuleLine(new ArchetypeInstantiationRuleLine());
	dummyAEIRL.setGTCode("currentDateTime");
	gtCodeElementMap.put("currentDateTime", dummyAEIRL);
	GuideDefinition guideDefinition = guide.getDefinition();
	TermDefinition termDefinition = null;
	if (guide.getOntology().getTermDefinitions()!=null && 
		guide.getOntology().getTermDefinitions().get(language)!=null){
	    termDefinition = guide.getOntology().getTermDefinitions().get(language);
	}else{
	    termDefinition = new TermDefinition();
	}
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
				    archetypeBinding.getTemplateId(), 
				    archetypeBinding.getFunction());
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
				    WithElementPredicateAttributeDefinitionRuleLine wepdrl = new WithElementPredicateAttributeDefinitionRuleLine();
				    airl.addChildRuleLine(wepdrl);
				    String path = variable.getPath();
				    String dvStr = constantExpression2.getValue();
				    ArchetypeElementVO archetypeElementVO = 
					    ArchetypeElements.getArchetypeElement(
						    archetypeBinding.getTemplateId(), 
						    archetypeBinding.getArchetypeId()+path);
				    wepdrl.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
				    String rmType = archetypeElementVO.getRMType();
				    if (OpenEHRDataValues.DV_TEXT.equals(rmType) &&
					    (OperatorKind.IS_A.equals(binaryExpression.getOperator()) || OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()))){
					rmType = OpenEHRDataValues.DV_CODED_TEXT;
				    }
				    DataValue dv = parseDataValue(rmType, dvStr);
				    wepdrl.getDataValueRuleLineElement().setValue(dv);
				    wepdrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
				}else if (binaryExpression.getLeft() instanceof Variable &&
					binaryExpression.getRight() instanceof ExpressionItem){
				    Variable variable = (Variable)binaryExpression.getLeft();
				    ExpressionItem expressionItemAux = binaryExpression.getRight();
				    WithElementPredicateExpressionDefinitionRuleLine wepdrl = new WithElementPredicateExpressionDefinitionRuleLine();
				    airl.addChildRuleLine(wepdrl);
				    String path = variable.getPath();
				    ArchetypeElementVO archetypeElementVO = 
					    ArchetypeElements.getArchetypeElement(
						    archetypeBinding.getTemplateId(), 
						    archetypeBinding.getArchetypeId()+path);
				    wepdrl.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
				    wepdrl.getExpressionRuleLineElement().setValue(expressionItemAux);
				    wepdrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
				}
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
	setTermDefinitionsOnRuleLines(readableGuide.getDefinitionRuleLines(), termDefinition);
	setTermDefinitionsOnRuleLines(readableGuide.getPreconditionRuleLines(), termDefinition);
	for (ReadableRule readableRule : readableGuide.getReadableRules().values()) {
	    setTermDefinitionsOnRuleLines(readableRule.getConditionRuleLines(), termDefinition);
	    setTermDefinitionsOnRuleLines(readableRule.getActionRuleLines(), termDefinition);
	}
	return readableGuide;
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

    protected static DataValue parseDataValue(String rmType, String dvStr){
	return DataValue.parseValue(rmType+","+dvStr);
    }

    protected static void processAssigmentExpression(
	    Collection<RuleLine> ruleLines, 
	    AssignmentExpression assignmentExpression,
	    Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap){
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

		log.debug("processAssigmentExpression for varialbe: " + gtCode);

		String rmType = archetypeElementVO.getRMType();
		DataValue dv = parseDataValue(rmType, dvStr);
		dataValueRuleLineElement.setValue(dv);
		ruleLines.add(sedvar);
	    }else{
		log.error("Unknown expression '"+expressionItemAux.getClass().getName()+"'");
	    }
	}else{
	    SetElementAttributeActionRuleLine seaarl = new SetElementAttributeActionRuleLine();
	    seaarl.getArchetypeElementAttributeRuleLineElement().setAttributeFunction(attribute);
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
	    Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap){
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
	    log.error("Unknown operator '"+binaryExpression.getOperator()+"'");
	}

    }

    protected static void processComparisonExpression(
	    Collection<RuleLine> ruleLines, 
	    RuleLine parentRuleLine, 
	    BinaryExpression binaryExpression,
	    Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap){
	GTCodeRuleLineElement gtCodeRuleLineElement = null;
	String attribute = null;
	String gtCode = null;
	if (binaryExpression.getLeft() instanceof Variable){
	    Variable var = (Variable)binaryExpression.getLeft();
	    gtCode = var.getCode();
	    log.debug("gtCode: " + gtCode);
	    gtCodeRuleLineElement = 
		    gtCodeELementMap.get(gtCode).getGTCodeRuleLineElement();

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
			if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)){
			    ArchetypeElementVO archetypeElementVO = gtCodeELementMap.get(gtCodeRuleLineElement.getValue()).getArchetypeElement();
			    rmType = archetypeElementVO.getRMType();
			    if (OpenEHRDataValues.DV_TEXT.equals(rmType) &&
				    (OperatorKind.IS_A.equals(binaryExpression.getOperator()) || OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()))){
				rmType = OpenEHRDataValues.DV_CODED_TEXT;
			    }
			}else{
			    rmType = OpenEHRDataValues.DV_DATE_TIME;
			}
			dv = parseDataValue(rmType, dvStr);
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
		    log.error("Unknown expression '"+binaryExpression.getRight().getClass().getName()+"'");
		}
	    }else{

		if (attribute.equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)){

		    ElementComparisonWithNullValueConditionRuleLine ecwnvc = new ElementComparisonWithNullValueConditionRuleLine();
		    ecwnvc.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
		    ConstantExpression constantExpression = (ConstantExpression)binaryExpression.getRight();
		    String dvStr = constantExpression.getValue();
		    DataValue dv = parseDataValue(OpenEHRDataValues.DV_CODED_TEXT, dvStr);
		    if (dv instanceof DvCodedText){
			ecwnvc.getNullValueRuleLineElement().setValue((DvCodedText)dv);
		    }
		    ecwnvc.getEqualityComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
		    addRuleLine(ecwnvc, ruleLines, parentRuleLine);
		}else{//Expression
		    ElementAttributeComparisonConditionRuleLine eaccrl = new ElementAttributeComparisonConditionRuleLine();
		    eaccrl.getArchetypeElementAttributeRuleLineElement().setAttributeFunction(attribute);
		    ArchetypeElementRuleLineElement aerle = new ArchetypeElementRuleLineElement(eaccrl);
		    aerle.setValue(gtCodeRuleLineElement);
		    eaccrl.getArchetypeElementAttributeRuleLineElement().setValue(aerle);
		    eaccrl.getExpressionRuleLineElement().setValue(binaryExpression.getRight());
		    eaccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
		    addRuleLine(eaccrl, ruleLines, parentRuleLine);
		}
	    }
	}else{
	    log.error("Unknown expression '"+binaryExpression.getLeft().getClass().getName()+"'");
	}
    }

    protected static void processExpressionItem(
	    Collection<RuleLine> ruleLines, 
	    RuleLine parentRuleLine, 
	    ExpressionItem expressionItem,
	    Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap){
	if (expressionItem instanceof AssignmentExpression){
	    processAssigmentExpression(ruleLines, (AssignmentExpression)expressionItem, gtCodeELementMap);
	}else if (expressionItem instanceof BinaryExpression){
	    processBinaryExpression(ruleLines, parentRuleLine, (BinaryExpression)expressionItem, gtCodeELementMap);
	}else if (expressionItem instanceof UnaryExpression){
	    processUnaryExpression(ruleLines, parentRuleLine, (UnaryExpression)expressionItem, gtCodeELementMap);
	}else{
	    log.error("Unknown expression '"+expressionItem.getClass().getName()+"'");
	}
    }

    protected static void processUnaryExpression(
	    Collection<RuleLine> ruleLines, 
	    RuleLine parentRuleLine, 
	    UnaryExpression unaryExpression,
	    Map<String, ArchetypeElementInstantiationRuleLine> gtCodeELementMap){
	if (OperatorKind.FOR_ALL.equals(unaryExpression.getOperator())){
	    ForAllOperatorRuleLine forAllOperatorRuleLine = new ForAllOperatorRuleLine();
	    processExpressionItem(ruleLines, forAllOperatorRuleLine, unaryExpression.getOperand(), gtCodeELementMap);
	    addRuleLine(forAllOperatorRuleLine, ruleLines, parentRuleLine);
	}else{
	    log.error("Unknown operator '"+unaryExpression.getOperator()+"'");
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
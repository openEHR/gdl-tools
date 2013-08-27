package se.cambio.cds.gdl.converters.drools;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.util.DVDefSerializer;
import se.cambio.cds.util.Domains;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class GDLDroolsConverter {

    public GDLDroolsConverter(Guide guide) {
	this.guide = guide;
    }

    private static Logger log = Logger.getLogger(GDLDroolsConverter.class);
    private Guide guide;

    // Drools keywords
    private static String RULE = "rule";
    private static String WHEN = "when";
    private static String THEN = "then";
    private static String END = "end";
    private static String DEFAULT_CONFIG = "no-loop true";

    private static String CODE_FUNCTION_SEPARATOR = "#";

    private Map<String, String> _gtElementToDefinition = new HashMap<String, String>();

    // STATS
    enum RefStat {
	REFERENCE, ATT_SET_REF, SET, ATT_FUNCTIONS, ATT_FUNCTIONS_REF
    }

    public String convertToDrools() throws InternalErrorException{
	StringBuffer sb = new StringBuffer();
	sb.append(getGuideHeader());

	int predicateCount = 0;
	Map<String, ArchetypeElementVO> elementMap = new HashMap<String, ArchetypeElementVO>();
	// Add currentTime
	elementMap.put(OpenEHRConst.CURRENT_DATE_TIME_ID,
		ArchetypeElements.CURRENT_DATE_TIME);

	Map<Integer, String> archetypeBindingIndexToDefinition = new HashMap<Integer, String>();

	Map<String, Integer> gtElementToArchetypeBindingIndex = new HashMap<String, Integer>();
	List<ArchetypeBinding> archetypeBindings = guide.getDefinition().getArchetypeBindings();
	int arCount = 0;
	String arID = "archetypeReference";
	if (archetypeBindings!=null){
	    for (int i = 0, j = archetypeBindings.size(); i < j; i++) {
		arCount++;
		ArchetypeBinding archetypeBinding = archetypeBindings.get(i);
		StringBuffer archetypeBindingMVELSB = new StringBuffer();

		archetypeBindingMVELSB.append("   ");
		archetypeBindingMVELSB.append("$"+arID+arCount);
		String idDomain = archetypeBinding.getDomain();
		archetypeBindingMVELSB.append(":ArchetypeReference");
		archetypeBindingMVELSB.append("(");
		if (idDomain!=null){
		    archetypeBindingMVELSB.append("idDomain==\""+idDomain+"\", ");
		}
		archetypeBindingMVELSB.append("idArchetype==\""+archetypeBinding.getArchetypeId()+"\"");
		if ((Domains.CDS_ID.equals(archetypeBinding.getDomain()))
			&&
			archetypeBinding.getTemplateId()!=null){
		    archetypeBindingMVELSB.append(", idTemplate==\""+archetypeBinding.getTemplateId()+"\"");
		}
		if (archetypeBinding.getFunction()!=null){
		    archetypeBindingMVELSB.append(", aggregationFunction==\""+archetypeBinding.getFunction()+"\"");
		}
		archetypeBindingMVELSB.append(")\n");

		// Predicates
		if (archetypeBinding.getPredicateStatements() != null) {
		    for (ExpressionItem expressionItem : archetypeBinding
			    .getPredicateStatements()) {
			if (expressionItem instanceof BinaryExpression) {
			    BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
			    if (binaryExpression.getLeft() instanceof Variable
				    && binaryExpression.getRight() instanceof ConstantExpression) {
				predicateCount++;
				Variable variable = (Variable) binaryExpression
					.getLeft();
				ConstantExpression constantExpression = (ConstantExpression) binaryExpression
					.getRight();
				String idElement = archetypeBinding
					.getArchetypeId() + variable.getPath();
				archetypeBindingMVELSB.append("      ");
				archetypeBindingMVELSB.append("$predicate"
					+ predicateCount);
				archetypeBindingMVELSB
				.append(":ElementInstance(id==\""
					+ idElement
					+ "\", archetypeReference==$"
					+ arID+arCount + ")\n");
				ArchetypeElementVO archetypeElement = ArchetypeElements
					.getArchetypeElement(
						archetypeBinding.getTemplateId(),
						idElement);
				if (archetypeElement!=null){
				    String rmType = archetypeElement.getRMType();
				    archetypeBindingMVELSB.append("      ");
				    archetypeBindingMVELSB
				    .append("eval("+ 
					    getOperatorMVELLine(
						    "$predicate"+ predicateCount,
						    binaryExpression.getOperator(),
						    DVDefSerializer.getDVInstantiation(DataValue.parseValue(rmType+ ","+ constantExpression.getValue())), 
						    true)+
					    ")\n");
				}else{
				    throw new InternalErrorException(new Exception("Element not found '"+idElement+"'"));
				}
			    }
			}
		    }				
		}

		archetypeBindingIndexToDefinition.put(i,
			archetypeBindingMVELSB.toString());
		for (ElementBinding element : archetypeBinding.getElements().values()) {
		    StringBuffer elementDefinitionSB = new StringBuffer();
		    String idElement = archetypeBinding.getArchetypeId()
			    + element.getPath();

		    ArchetypeElementVO value =  ArchetypeElements.getArchetypeElement(
			    archetypeBinding.getTemplateId(), idElement);

		    elementMap.put(element.getId(), value);				
		    elementDefinitionSB.append("ElementInstance(id==\""+idElement+"\", archetypeReference==$"+arID+arCount+")");
		    //elementDefinitionSB.append(":ElementInstance(id==\""
		    //	+ idElement + "\")\n");
		    _gtElementToDefinition.put(element.getId(),
			    elementDefinitionSB.toString());
		    gtElementToArchetypeBindingIndex.put(element.getId(), i);
		}
	    }
	}

	Map<RefStat, Set<String>> preconditionStats = initStats();
	String preconditionStr = null;
	if (guide.getDefinition().getPreConditionExpressions() != null) {
	    preconditionStr = 
		    convertExpressionsToMVEL(
			    guide.getDefinition().getPreConditionExpressions(), 
			    elementMap,
			    preconditionStats);
	}
	StringBuffer preconditionInitSB = new StringBuffer();

	String preconditionInitStr = preconditionInitSB.toString();
	if (guide.getDefinition().getRules()!=null){
	    for (Rule rule : guide.getDefinition().getRules().values()) {
		Map<RefStat, Set<String>> ruleStats = initStats();
		String whenStr = convertExpressionsToMVEL(rule.getWhenStatements(),
			elementMap, ruleStats);
		String thenStr = convertAssigmentExpressionsToMVEL(
			rule.getThenStatements(), elementMap, ruleStats);
		Set<String> gtCodesRef = new HashSet<String>();
		gtCodesRef.addAll(ruleStats.get(RefStat.REFERENCE));
		gtCodesRef.addAll(preconditionStats.get(RefStat.REFERENCE));
		gtCodesRef.remove(OpenEHRConst.CURRENT_DATE_TIME_ID);
		String definition = 
			getDefinitionForRule(gtCodesRef,
				archetypeBindingIndexToDefinition,
				gtElementToArchetypeBindingIndex);
		ruleStats.get(RefStat.ATT_SET_REF).remove(OpenEHRConst.CURRENT_DATE_TIME_ID);
		String hasValueChecks = 
			getHasValueStr(ruleStats.get(RefStat.ATT_SET_REF));
		//Check if a function is used, add whatever extra code necessary for it (for now, just count)
		Set<String> functionsRefs = new HashSet<String>();
		functionsRefs.addAll(ruleStats.get(RefStat.ATT_FUNCTIONS));
		functionsRefs.addAll(preconditionStats.get(RefStat.ATT_FUNCTIONS));
		String functionExtraCode = getFunctionsExtraCode(functionsRefs);
		sb.append(RULE + " \"" + guide.getId() + "/" + rule.getId() + "\"\n");
		sb.append("salience " + rule.getPriority() + "\n");
		sb.append(DEFAULT_CONFIG + "\n");
		sb.append(WHEN + "\n");
		if (definition != null){
		    sb.append(definition);
		}
		if (functionExtraCode != null){
		    sb.append(functionExtraCode);
		}
		if (hasValueChecks != null){
		    sb.append(hasValueChecks);
		}
		if (preconditionInitStr != null){
		    sb.append(preconditionInitStr);
		}
		if (preconditionStr != null){
		    sb.append(preconditionStr);
		}
		if (whenStr != null){
		    sb.append(whenStr);
		}
		sb.append(THEN + "\n");
		if (thenStr != null){
		    sb.append(thenStr);
		}
		sb.append(END + "\n\n");
	    }
	}
	return sb.toString();
    }

    private String getDefinitionForRule(Set<String> gtCodesRef,
	    Map<Integer, String> archetypeBindingIndexToDefinition,
	    Map<String, Integer> gtElementToArchetypeBindingIndex) {

	Map<Integer, StringBuffer> archetypeDefinitions = new HashMap<Integer, StringBuffer>();
	for (String elementGtCode : gtCodesRef) {
	    Integer archetypeBindingIndex = gtElementToArchetypeBindingIndex
		    .get(elementGtCode);
	    StringBuffer definition = archetypeDefinitions
		    .get(archetypeBindingIndex);
	    if (definition == null) {
		definition = new StringBuffer();
		definition.append(archetypeBindingIndexToDefinition
			.get(archetypeBindingIndex));
		archetypeDefinitions.put(archetypeBindingIndex, definition);
	    }
	    definition.append("      $"+elementGtCode+":"+_gtElementToDefinition.get(elementGtCode)+"\n");
	}
	;
	StringBuffer resultSB = new StringBuffer();
	for (StringBuffer definition : archetypeDefinitions.values()) {
	    resultSB.append(definition.toString());
	}
	return resultSB.toString();
    }

    private Map<RefStat, Set<String>> initStats() {
	Map<RefStat, Set<String>> stats = new HashMap<RefStat, Set<String>>();
	for (RefStat refStat : RefStat.values()) {
	    stats.put(refStat, new HashSet<String>());
	}
	return stats;
    }

    private String convertExpressionsToMVEL(
	    Collection<ExpressionItem> expressionItems,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	StringBuffer sb = new StringBuffer();
	if (expressionItems != null) {
	    for (ExpressionItem expressionItem : expressionItems) {
		sb.append("   ");
		processExpressionItem(sb, expressionItem, elementMap, stats);
		sb.append("\n");
	    }
	}
	return sb.toString();
    }

    private String convertAssigmentExpressionsToMVEL(
	    Collection<AssignmentExpression> expressionItems,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	StringBuffer sb = new StringBuffer();
	if (expressionItems != null) {
	    for (ExpressionItem expressionItem : expressionItems) {
		sb.append("   ");
		processExpressionItem(sb, expressionItem, elementMap, stats);
		sb.append("\n");
	    }
	    for (String gtCodes : stats.get(RefStat.SET)) {
		sb.append("   ");
		sb.append("modify($"+gtCodes+"){};\n");
	    }
	}
	return sb.toString();
    }

    protected void processExpressionItem(StringBuffer sb,
	    ExpressionItem expressionItem,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	if (expressionItem instanceof AssignmentExpression) {
	    processAssigmentExpression(sb,
		    (AssignmentExpression) expressionItem, elementMap, stats);
	} else if (expressionItem instanceof BinaryExpression) {
	    processBinaryExpression(sb, (BinaryExpression) expressionItem,
		    elementMap, stats);
	} else if (expressionItem instanceof UnaryExpression) {
	    processUnaryExpression(sb, (UnaryExpression) expressionItem,
		    elementMap, stats);
	} else {
	    Logger.getLogger(GDLDroolsConverter.class).error(
		    "Unknown expression '"
			    + expressionItem.getClass().getName() + "'");
	}
    }

    protected void processAssigmentExpression(StringBuffer sb,
	    AssignmentExpression assignmentExpression,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	String gtCode = assignmentExpression.getVariable().getCode();
	stats.get(RefStat.REFERENCE).add(gtCode);
	ExpressionItem expressionItemAux = assignmentExpression.getAssignment();
	Variable var = assignmentExpression.getVariable();
	String attribute = var.getAttribute();
	stats.get(RefStat.SET).add(gtCode);
	if (attribute == null) {
	    if (expressionItemAux instanceof Variable) {
		String gtCodeAux = ((Variable) expressionItemAux).getCode();
		sb.append(
			"$"+gtCode+"setDataValue($"+gtCodeAux+getDataValueMethod(gtCode)+");"+
				"$"+gtCode+".setNullFlavour(null);"+
				"$executionLogger.addLog(drools, $"+ gtCode + ");");
	    } else if (expressionItemAux instanceof ConstantExpression) {
		String dvStr = 
			((ConstantExpression) expressionItemAux).getValue();
		ArchetypeElementVO archetypeElementVO = elementMap.get(gtCode);
		String rmType = archetypeElementVO.getRMType();
		DataValue dv = DataValue.parseValue(rmType + "," + dvStr);
		sb.append(
			"$"+gtCode+".setDataValue("+ DVDefSerializer.getDVInstantiation(dv)+");"+
				"$"+gtCode+".setNullFlavour(null);"+
				"$executionLogger.addLog(drools, $"+gtCode +");");
	    } else {
		Logger.getLogger(GDLDroolsConverter.class).error(
			"Unknown expression '"
				+ expressionItemAux.getClass().getName() + "'");
	    }
	} else {
	    if (attribute.equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)){
		String dvStr = ((ConstantExpression) expressionItemAux).getValue();
		DataValue dv = DataValue.parseValue(OpenEHRDataValues.DV_CODED_TEXT + "," + dvStr);
		Map<RefStat, Set<String>> statsAux = initStats();
		stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));
		sb.append("$"+gtCode+ ".setDataValue(null);"+
			"$"+gtCode+".setNullFlavour("+ DVDefSerializer.getDVInstantiation(dv)+");"+
			"$executionLogger.addLog(drools, $"+gtCode +");");
	    }else{
		String rmName = elementMap.get(gtCode).getRMType();
		Map<RefStat, Set<String>> statsAux = initStats();
		String arithmeticExpStr = 
			getArithmeticExpressionStr(elementMap, expressionItemAux, statsAux);
		stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));
		stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.ATT_FUNCTIONS_REF));
		stats.get(RefStat.ATT_SET_REF).addAll(statsAux.get(RefStat.REFERENCE));
		stats.get(RefStat.ATT_FUNCTIONS).addAll(statsAux.get(RefStat.ATT_FUNCTIONS));
		sb.append(
			"$"+ gtCode+ "."+ getAttributeSettingStr(gtCode, rmName, attribute, arithmeticExpStr)+ ";"+
				"$"+ gtCode+ ".setNullFlavour(null);"+
				"$executionLogger.addLog(drools, $"+ gtCode + ");");
	    }
	}
    }

    private String getDataValueMethod(String gtCode) {
	if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)) {
	    return ".getDataValue()";
	} else {
	    return "";
	}
    }

    private String getAttributeSettingStr(String gtCode, String rmName,
	    String attributeName, String setStr) {
	return "setDataValue(DVUtil.createDV($" + gtCode + ",\"" + rmName
		+ "\",\"" + attributeName + "\"," + setStr + "))";
    }

    protected void processBinaryExpression(StringBuffer sb,
	    BinaryExpression binaryExpression,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	if (OperatorKind.OR.equals(binaryExpression.getOperator())) {
	    sb.append("(");
	    processExpressionItem(sb, binaryExpression.getLeft(), elementMap,
		    stats);
	    sb.append(" or ");
	    processExpressionItem(sb, binaryExpression.getRight(), elementMap,
		    stats);
	    sb.append(")");
	} else if (OperatorKind.AND.equals(binaryExpression.getOperator())) {
	    sb.append("(");
	    processExpressionItem(sb, binaryExpression.getLeft(), elementMap,
		    stats);
	    sb.append(" and ");
	    processExpressionItem(sb, binaryExpression.getRight(), elementMap,
		    stats);
	    sb.append(")");
	} else if (OperatorKind.EQUALITY.equals(binaryExpression.getOperator())
		|| OperatorKind.INEQUAL.equals(binaryExpression.getOperator())
		|| OperatorKind.IS_A.equals(binaryExpression.getOperator())
		|| OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator())
		|| OperatorKind.GREATER_THAN.equals(binaryExpression.getOperator())
		|| OperatorKind.GREATER_THAN_OR_EQUAL.equals(binaryExpression.getOperator())
		|| OperatorKind.LESS_THAN.equals(binaryExpression.getOperator())
		|| OperatorKind.LESS_THAN_OR_EQUAL.equals(binaryExpression.getOperator())) {
	    processComparisonExpression(sb, binaryExpression, elementMap, stats);
	} else {
	    Logger.getLogger(GDLDroolsConverter.class)
	    .error("Unknown operator '"
		    + binaryExpression.getOperator() + "'");
	}

    }

    protected void processUnaryExpression(StringBuffer sb,
	    UnaryExpression unaryExpression,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	if (OperatorKind.NOT.equals(unaryExpression.getOperator())) {
	    sb.append("not(");
	    processExpressionItem(sb, unaryExpression.getOperand(), elementMap,
		    stats);
	    sb.append(")");
	} else if (OperatorKind.FOR_ALL.equals(unaryExpression.getOperator())) {
	    sb.append("forall(");
	    processExpressionItem(sb, unaryExpression.getOperand(), elementMap,
		    stats);
	    sb.append(")");
	} else {
	    Logger.getLogger(GDLDroolsConverter.class).error(
		    "Unknown operator '" + unaryExpression.getOperator() + "'");
	}
    }

    protected void processComparisonExpression(StringBuffer sb,
	    BinaryExpression binaryExpression,
	    Map<String, ArchetypeElementVO> elementMap,
	    Map<RefStat, Set<String>> stats) {
	Variable var = null;
	if (binaryExpression.getLeft() instanceof Variable) {
	    var = (Variable) binaryExpression.getLeft();
	    stats.get(RefStat.REFERENCE).add(var.getCode());
	}
	if (var != null) {
	    if (var.getAttribute() == null) {
		if (binaryExpression.getRight() instanceof ConstantExpression) {
		    ConstantExpression constantExpression = 
			    (ConstantExpression) binaryExpression.getRight();
		    String dvStr = constantExpression.getValue();
		    DataValue dv = null;
		    if (!dvStr.equals("null")) {
			ArchetypeElementVO archetypeElementVO = elementMap.get(var.getCode());
			String rmType = archetypeElementVO.getRMType();
			dv = DataValue.parseValue(rmType + "," + dvStr);
		    }
		    if (dv != null) {
			sb.append("eval(");
			if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(var.getCode())){
			    sb.append("$" + var.getCode() + ".hasValue() && ");
			}
			sb.append(getOperatorMVELLine("$"+var.getCode(),
				binaryExpression.getOperator(),
				DVDefSerializer.getDVInstantiation(dv)));
			sb.append(")");
		    } else {
			if (OperatorKind.EQUALITY.equals(binaryExpression
				.getOperator())) {
			    sb.append("eval($" + var.getCode()
				    + ".hasNoValue())");
			} else if (OperatorKind.INEQUAL.equals(binaryExpression
				.getOperator())) {
			    sb.append("eval($" + var.getCode() + ".hasValue())");
			}
		    }
		} else if (binaryExpression.getRight() instanceof Variable) {
		    Variable varRight = (Variable) binaryExpression.getRight();
		    String gtCodeAux = varRight.getCode();
		    sb.append("eval($" + var.getCode() + ".hasValue() && ");
		    sb.append("$" + gtCodeAux + ".hasValue() && ");
		    sb.append(getOperatorMVELLine("$"+var.getCode(),
			    binaryExpression.getOperator(), "$" + gtCodeAux));
		    sb.append(")");
		    stats.get(RefStat.REFERENCE).add(gtCodeAux);
		} else {
		    Logger.getLogger(GDLDroolsConverter.class).error(
			    "Unknown expression '"
				    + binaryExpression.getRight().getClass()
				    .getName() + "'");
		}
	    } else {
		if (var.getAttribute().equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)){
		    ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
		    String dvStr = constantExpression.getValue();
		    DataValue dv = DataValue.parseValue(OpenEHRDataValues.DV_CODED_TEXT + "," + dvStr);
		    sb.append("eval(");
		    String opNeg = (binaryExpression.getOperator().equals(OperatorKind.INEQUAL))?"!":"";
		    sb.append(opNeg+"DVUtil.nullValueEquals($"+var.getCode()+".getNullFlavour(), "+DVDefSerializer.getDVInstantiation(dv)+"))");
		}else{//Expression
		    Map<RefStat, Set<String>> statsAux = initStats();
		    String artimeticExpStr = 
			    getArithmeticExpressionStr(elementMap, binaryExpression.getRight(), statsAux);
		    //Add stats
		    getArithmeticExpressionStr(elementMap, binaryExpression.getLeft(), statsAux);
		    stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));
		    stats.get(RefStat.ATT_FUNCTIONS).addAll(statsAux.get(RefStat.ATT_FUNCTIONS));
		    sb.append("eval(");
		    statsAux.get(RefStat.REFERENCE).remove(OpenEHRConst.CURRENT_DATE_TIME_ID);
		    for (String gtCode : statsAux.get(RefStat.REFERENCE)) {
			sb.append("$" + gtCode + ".hasValue() && ");
		    }
		    String rmName = elementMap.get(var.getCode()).getRMType();
		    sb.append("(");
		    String varCall = getVariableWithAttributeStr(rmName, var);
		    if (isString(rmName, var.getAttribute())){
			sb.append(getAttributeOperatorMVELLine(varCall, binaryExpression.getOperator(), artimeticExpStr));
		    }else{
			sb.append(varCall);
			sb.append(binaryExpression.getOperator().getSymbol());
			sb.append(artimeticExpStr);
		    }
		    sb.append("))");
		}
	    }
	} else {
	    Logger.getLogger(GDLDroolsConverter.class).error("Unknown expression '" + binaryExpression.getLeft() + "'");
	}
    }

    private String getHasValueStr(Collection<String> gtCodes) {
	if (!gtCodes.isEmpty()) {
	    StringBuffer sb = new StringBuffer();
	    sb.append("   eval(");
	    int count = 0;
	    for (String gtCode : gtCodes) {
		sb.append("$" + gtCode + ".hasValue()");
		if (++count < gtCodes.size()) {
		    sb.append(" && ");
		}
	    }
	    sb.append(")\n");
	    return sb.toString();
	} else {
	    return null;
	}
    }


    private String getFunctionsExtraCode(Collection<String> gtCodesWithFunctions){
	StringBuffer sb = new StringBuffer();
	for (String gtCodesWithFunction : gtCodesWithFunctions) {
	    String[] codeSplit = gtCodesWithFunction.split(CODE_FUNCTION_SEPARATOR);
	    String code = codeSplit[0];
	    String att = codeSplit[1];
	    if (isFunction(att)){
		if (OpenEHRDataValues.FUNCTION_COUNT.equals(att)){
		    String def = _gtElementToDefinition.get(code);
		    def = def.substring(0, def.length()-1)+", dataValue!=null)";
		    sb.append("   Number($"+code+att+":intValue) from accumulate ("+def+", count(1))\n   ");
		}
	    }
	}
	String str = sb.toString();
	return str.isEmpty()?null:str;
    }

    private String getOperatorMVELLine(String handle, OperatorKind ok, String value) {
	return getOperatorMVELLine(handle, ok, value, false);
    }

    private String getOperatorMVELLine(
	    String handle, OperatorKind ok,
	    String value, boolean inPredicate) {
	if (OperatorKind.EQUALITY.equals(ok)) {
	    return getEqualsString(handle, value, inPredicate);
	} else if (OperatorKind.INEQUAL.equals(ok)) {
	    return "!" + getEqualsString(handle, value, inPredicate);
	} else if (OperatorKind.IS_A.equals(ok)) {
	    return "DVUtil.isSubClassOf("+ inPredicate+", "+ handle + ", "+ getTermBindings(value) + ")";
	} else if (OperatorKind.IS_NOT_A.equals(ok)) {
	    return "DVUtil.isNotSubClassOf(" + inPredicate + ", "+ handle+", "+ getTermBindings(value) + ")";
	} else if (OperatorKind.GREATER_THAN.equals(ok)) {
	    return getComparisonString(handle, value) + ">0";
	} else if (OperatorKind.GREATER_THAN_OR_EQUAL.equals(ok)) {
	    return getComparisonString(handle, value) + ">=0";
	} else if (OperatorKind.LESS_THAN.equals(ok)) {
	    return getComparisonString(handle, value) + "<0";
	} else if (OperatorKind.LESS_THAN_OR_EQUAL.equals(ok)) {
	    return getComparisonString(handle, value) + "<=0";
	} else {
	    return null;
	}
    }


    private static String getEqualsString(String handle, String value, boolean inPredicate){
	StringBuffer sb = new StringBuffer();
	sb.append("DVUtil.equalDV("+inPredicate+", "+handle+"," + value);
	sb.append(getDataValueStrIfNeeded(value)+")");
	return sb.toString();
    }

    private static String getComparisonString(String handle, String value){
	StringBuffer sb = new StringBuffer();
	sb.append("DVUtil.compatibleComparison(" + handle +getDataValueStrIfNeeded(handle)+ ", $auxDV="+ value+getDataValueStrIfNeeded(value) + ") && ");
	sb.append("DVUtil.compareDVs("+handle+".getDataValue(), $auxDV)");
	return sb.toString();
    }

    private static String getDataValueStrIfNeeded(String value){
	if (value.startsWith("$")){
	    return ".getDataValue()";
	}else{
	    return "";
	}
    }


    private String getAttributeOperatorMVELLine(
	    String handle, 
	    OperatorKind ok,
	    String value) {
	if (OperatorKind.EQUALITY.equals(ok)) {
	    return handle + ".equals("+ value + ")";
	} else if (OperatorKind.INEQUAL.equals(ok)) {
	    return "!" + handle + ".equals(" + value + ")";
	} else {
	    Logger.getLogger(GDLDroolsConverter.class).warn("Illegar operator '"+ok.getSymbol()+"' used in handle '"+handle+"'.");
	    return "false";
	}
    }

    /*
     * Parse code from string value and generate right 
     * code_phrase array for subClass evaluation
     * 
     * possible values:
     * 1. new DvCodedText("Dubois and Dubois","local","gt0008")
     * 2. new DvText("local::gt0100")
     * 2. new DvText("local::gt0100|Hypertension|")
     */
    protected String parseCode(String value) {
	int i = value.indexOf("local");

	log.debug("value after IS_A: " + value);

	if (i < 0) {
	    return value;
	}

	String code;


	if(value.contains("DvCodedText")) {
	    code = value.substring(i + 8, value.length() - 2);
	} else if(value.contains("'")) { // due to a logic somewhere in gdl-editor introducing single quotation to code_phrase
	    code = value.substring(i + 7, value.length() - 3);
	} else {
	    code = value.substring(i + 7, value.length() - 2);
	}

	int j = code.indexOf("|");		
	if(j > 0) {
	    code = code.substring(0, j);
	}

	log.debug("code parsed from value: " + code);

	return code;
    }

    private String getTermBindings(String value) {
	if (value.startsWith("$")){
	    Logger.getLogger(GDLDroolsConverter.class).warn("Subclass comparison between elements is not supported.");
	    //TODO Give support to subclass comparison between elements
	    return "null";
	}
	Map<String, TermBinding> termBindings = guide.getOntology().getTermBindings();

	// TODO log.warn if gt code is unbound to terminologies
	if(termBindings == null) {
	    //Logger.getLogger(GDLDroolsConverter.class).warn("Needed terminology binding not found on guide.");
	    return value;
	}
	String code = parseCode(value);
	StringBuffer buf = new StringBuffer("new DvCodedText[] {");
	boolean first = true;		

	for(String terminology : termBindings.keySet()) {			

	    log.debug("terminology: " + terminology);

	    TermBinding termBinding = termBindings.get(terminology);			
	    Map<String, Binding> bindings = termBinding.getBindings();

	    log.debug("bindings: " + bindings);			

	    if(bindings.containsKey(code)) {

		log.debug("hasCode: " + code);

		Binding binding = bindings.get(code); 
		if(binding.getCodes() != null) {
		    for(CodePhrase cp : binding.getCodes()) {
			if(first) {
			    first = false;
			} else {
			    buf.append(",");
			}
			buf.append("new DvCodedText(\"text\",\"");
			buf.append(terminology);
			buf.append("\",\"");
			buf.append(cp.getCodeString());
			buf.append("\")");
		    }
		}				
	    }
	}
	buf.append("}");
	return buf.toString();
    }

    private String getArithmeticExpressionStr(
	    Map<String, ArchetypeElementVO> elementMap,
	    ExpressionItem expressionItem, Map<RefStat, Set<String>> stats) {
	StringBuffer sb = new StringBuffer();
	if (expressionItem instanceof BinaryExpression) {
	    BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
	    if (OperatorKind.EXPONENT.equals(binaryExpression.getOperator())) {
		sb.append("Math.pow(");
		sb.append(getArithmeticExpressionStr(elementMap,
			binaryExpression.getLeft(), stats));
		sb.append(",");
		sb.append(getArithmeticExpressionStr(elementMap,
			binaryExpression.getRight(), stats));
		sb.append(")");
	    } else {
		sb.append("("
			+ getArithmeticExpressionStr(elementMap,
				binaryExpression.getLeft(), stats));
		sb.append(binaryExpression.getOperator().getSymbol());
		sb.append(getArithmeticExpressionStr(elementMap,
			binaryExpression.getRight(), stats) + ")");
	    }
	} else if (expressionItem instanceof Variable) {
	    Variable var = (Variable) expressionItem;
	    String rmName = elementMap.get(var.getCode()).getRMType();
	    sb.append(getVariableWithAttributeStr(rmName, var));
	    if (isFunction(var.getAttribute())){
		stats.get(RefStat.ATT_FUNCTIONS).add(var.getCode()+CODE_FUNCTION_SEPARATOR+var.getAttribute());
		stats.get(RefStat.ATT_FUNCTIONS_REF).add(var.getCode());
	    }else{
		stats.get(RefStat.REFERENCE).add(var.getCode());
	    }
	} else if (expressionItem instanceof StringConstant) {
	    sb.append(expressionItem.toString());
	} else if (expressionItem instanceof ConstantExpression) {
	    sb.append(formatConstantValue((ConstantExpression) expressionItem));
	} else {
	    Logger.getLogger(GDLDroolsConverter.class).error(
		    "Unknown expression '"
			    + expressionItem.getClass().getName() + "'");
	}
	return sb.toString();
    }

    /*
     * Parse for units of hr and convert value to milliseconds
     */
    private String formatConstantValue(ConstantExpression exp) {
	String value = exp.getValue();
	int i = value.indexOf(",");
	if(i > 0 ){
	    //Convert time units to milliseconds
	    String units = value.substring(i+1).trim();
	    if (units.equals("a")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "31556926000L*" + d;
	    }else if (units.equals("mo")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "2629743830L*" + d;
	    }else if (units.equals("wk")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "604800000L*" + d;
	    }else if (units.equals("d")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "86400000L*" + d;
	    }else if (units.equals("h")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "3600000L*" + d;
	    }else if (units.equals("min")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "60000L*" + d;
	    }else if (units.equals("s")) {
		double d = Double.parseDouble(value.substring(0, i));
		value = "1000L*" + d;
	    } else { //milliseconds or any other unit
		double d = Double.parseDouble(value.substring(0, i));
		value = "" + d;
	    }
	}
	return value;
    }

    public String getVariableWithAttributeStr(String rmName, Variable var) {

	log.debug("Var.code: " + var.getCode() + ", attr: " + var.getAttribute());		

	String dvClassName = DVDefSerializer.getDVClassName(rmName);
	String ret = null;

	// TODO fix setting currentDateTime
	if("currentDateTime".equals(var.getCode()) && var.getAttribute()==null) {
	    ret = "$currentDateTime.getDateTime().getMillis()";
	} else if("value".equals(var.getAttribute()) &&("DvDateTime".equals(dvClassName) 
		|| "DvDate".equals(dvClassName))) {
	    ret = "((" + dvClassName + ")$" + var.getCode() + 
		    getDataValueMethod(var.getCode()) + 
		    ").getDateTime().getMillis()";
	} else {
	    if (isFunction(var.getAttribute())){
		//Function (Only working for count yet)
		if (OpenEHRDataValues.FUNCTION_COUNT.equals(var.getAttribute())){
		    ret = "$"+var.getCode()+var.getAttribute();
		}
	    }else{
		//Attribute
		ret = "((" + dvClassName + ")$" + var.getCode()
			+ getDataValueMethod(var.getCode()) + ").get"
			+ StringUtils.capitalize(var.getAttribute()) + "()";
	    }
	}
	return ret;
    }

    private boolean isFunction(String attribute){
	return OpenEHRDataValuesUI.getFunctionNames().contains(attribute);
    }

    private boolean isString(String rmName, String attribute){
	return (OpenEHRDataValues.DV_TEXT.equals(rmName) && OpenEHRDataValues.VALUE_ATT.equals(attribute)) ||
		(OpenEHRDataValues.DV_CODED_TEXT.equals(rmName) && OpenEHRDataValues.VALUE_ATT.equals(attribute)) ||
		OpenEHRDataValues.UNITS_ATT.equals(attribute) ||
		OpenEHRDataValues.CODE_ATT.equals(attribute) ||
		OpenEHRDataValues.TEMINOLOGYID_ATT.equals(attribute);
    }

    private String getGuideHeader() {
	return "package se.cambio.cds;\n"
		+ "import se.cambio.cds.model.instance.ArchetypeReference;\n"
		+ "import se.cambio.cds.model.instance.ElementInstance;\n"
		+ "import se.cambio.cds.model.instance.ContainerInstance;\n"
		+ "import se.cambio.cds.util.DVUtil;\n"
		+ "import org.openehr.rm.datatypes.quantity.DvCount;\n"
		+ "import org.openehr.rm.datatypes.quantity.DvOrdinal;\n"
		+ "import org.openehr.rm.datatypes.quantity.DvQuantity;\n"
		+ "import org.openehr.rm.datatypes.quantity.datetime.DvDate;\n"
		+ "import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;\n"
		+ "import org.openehr.rm.datatypes.quantity.datetime.DvDuration;\n"
		+ "import org.openehr.rm.datatypes.quantity.datetime.DvTime;\n"
		+ "import org.openehr.rm.datatypes.quantity.DvProportion;\n"
		+ "import org.openehr.rm.datatypes.quantity.ProportionKind;\n"
		+ "import org.openehr.rm.datatypes.basic.DvBoolean;\n"
		+ "import org.openehr.rm.datatypes.text.DvCodedText;\n"
		+ "import org.openehr.rm.datatypes.text.DvText;\n"
		+ "global se.cambio.cds.util.ExecutionLogger $executionLogger;\n"
		+ "global org.openehr.rm.datatypes.basic.DataValue $auxDV;\n"
		+ "global org.openehr.rm.datatypes.quantity.datetime.DvDateTime $"+OpenEHRConst.CURRENT_DATE_TIME_ID + ";\n\n";
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
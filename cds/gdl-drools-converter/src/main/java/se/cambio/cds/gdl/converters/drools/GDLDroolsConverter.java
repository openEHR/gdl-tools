package se.cambio.cds.gdl.converters.drools;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.DVDefSerializer;
import se.cambio.cds.util.ExpressionUtil;
import se.cambio.cds.util.RefStat;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
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
    private static String DEFAULT_CONFIG = "no-loop true\nauto-focus true";
    private static String AGENDA_GROUP = "agenda-group";

    private Map<String, String> _gtElementToWholeDefinition = new HashMap<String, String>();
    private Map<String, String> _gtElementToDefinition = new HashMap<String, String>();
    private Map<String, String> _gtElementToElementId = new HashMap<String, String>();
    private int creationIndex = 0;
    public String convertToDrools() throws InternalErrorException{
        StringBuffer sb = new StringBuffer();
        sb.append(getGuideHeader());

        Map<String, ArchetypeReference> archetypeReferenceMap = new HashMap<String, ArchetypeReference>();
        Map<String, ArchetypeElementVO> elementMap = new HashMap<String, ArchetypeElementVO>();
        // Add currentTime
        elementMap.put(OpenEHRConst.CURRENT_DATE_TIME_ID,
                ArchetypeElements.CURRENT_DATE_TIME);
        Map<Integer, String> archetypeBindingIndexToDefinition = new HashMap<Integer, String>();
        Map<String, Integer> gtElementToArchetypeBindingIndex = new HashMap<String, Integer>();
        Map<String, ArchetypeBinding> archetypeBindings = guide.getDefinition().getArchetypeBindings();
        fillDefinitions(
                archetypeBindings!=null?archetypeBindings.values():null,
                archetypeBindingIndexToDefinition,
                gtElementToArchetypeBindingIndex,
                archetypeReferenceMap,
                elementMap);
        Map<RefStat, Set<String>> preconditionStats = initStats();
        String preconditionStr = null;
        if (guide.getDefinition().getPreConditionExpressions() != null) {
            preconditionStr =
                    convertExpressionsToMVEL(
                            guide.getDefinition().getPreConditionExpressions(),
                            archetypeReferenceMap,
                            elementMap,
                            preconditionStats);
        }
        if (guide.getDefinition().getRules()!=null){
            for (Rule rule : guide.getDefinition().getRules().values()) {
                Map<RefStat, Set<String>> ruleStats = initStats();
                String whenStr = convertExpressionsToMVEL(rule.getWhenStatements(),
                        archetypeReferenceMap,
                        elementMap, ruleStats);
                String thenStr = convertAssigmentExpressionsToMVEL(
                        rule.getThenStatements(),
                        archetypeReferenceMap,
                        elementMap, ruleStats);
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
                sb.append(AGENDA_GROUP + " '"+guide.getId()+"'\n"); //Isolate rule execution to guide context
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

    private void fillDefinitions(
            Collection<ArchetypeBinding> archetypeBindings,
            Map<Integer, String> archetypeBindingIndexToDefinition,
            Map<String, Integer> gtElementToArchetypeBindingIndex,
            Map<String, ArchetypeReference> archetypeReferenceMap,
            Map<String, ArchetypeElementVO> elementMap) throws InternalErrorException {
        int arCount = 0;
        int predicateCount = 0;
        String arID = "archetypeReference";
        if (archetypeBindings!=null){
            for(ArchetypeBinding archetypeBinding: archetypeBindings){
                arCount++;
                StringBuffer archetypeBindingMVELSB = new StringBuffer();
                archetypeBindingMVELSB.append("   ");
                archetypeBindingMVELSB.append("$"+arID+arCount);
                String idDomain = archetypeBinding.getDomain();
                archetypeBindingMVELSB.append(":ArchetypeReference");
                archetypeBindingMVELSB.append("(");
                if (idDomain!=null){
                    archetypeBindingMVELSB.append("idDomain==\""+idDomain+"\", ");
                }
                String archetypeId = archetypeBinding.getArchetypeId();
                String templateId = archetypeBinding.getTemplateId();
                archetypeBindingMVELSB.append("idArchetype==\""+archetypeId+"\"");
                /*
                if ((Domains.CDS_ID.equals(archetypeBinding.getDomain()))&&
                        archetypeBinding.getTemplateId()!=null){
                    archetypeBindingMVELSB.append(", idTemplate==\""+templateId+"\"");
                }
                */
                archetypeBindingMVELSB.append(")\n");
                String gtCode = archetypeBinding.getId();
                archetypeReferenceMap.put(gtCode, new ArchetypeReference(idDomain, archetypeId, templateId));
                // Predicates
                if (archetypeBinding.getPredicateStatements() != null) {
                    for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
                        if (expressionItem instanceof BinaryExpression) {
                            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
                            if (binaryExpression.getLeft() instanceof Variable){
                                predicateCount++;
                                Variable variable = (Variable) binaryExpression
                                        .getLeft();

                                if (binaryExpression.getRight() instanceof ConstantExpression) {
                                    ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
                                    String idElement = archetypeBinding
                                            .getArchetypeId() + variable.getPath();
                                    archetypeBindingMVELSB.append("      ");
                                    archetypeBindingMVELSB.append("$predicate"
                                            + predicateCount);
                                    archetypeBindingMVELSB.append(":ElementInstance(id==\""
                                            + idElement
                                            + "\", archetypeReference==$"
                                            + arID+arCount + ")\n");
                                    ArchetypeElementVO archetypeElement =
                                            ArchetypeElements.getArchetypeElement(
                                                    archetypeBinding.getTemplateId(),
                                                    idElement);
                                    if (archetypeElement==null){
                                        throw new InternalErrorException(new Exception("Element not found '"+idElement+"'"+(archetypeBinding.getTemplateId()!=null?"("+archetypeBinding.getTemplateId()+")":"")));
                                    }
                                    String rmType = archetypeElement.getRMType();
                                    archetypeBindingMVELSB.append("      ");
                                    String dvStr = "null";
                                    if (!constantExpression.getValue().equals("null")){
                                        dvStr = DVDefSerializer.getDVInstantiation(DataValue.parseValue(rmType+ ","+ constantExpression.getValue()));
                                    }
                                    archetypeBindingMVELSB
                                            .append("eval("+
                                                    getOperatorMVELLine(
                                                            "$predicate"+ predicateCount,
                                                            binaryExpression.getOperator(),
                                                            dvStr,
                                                            true)+
                                                    ")\n");

                                }else if (binaryExpression.getRight() instanceof ExpressionItem) {
                                    String path = variable.getPath();
                                    String attribute = path.substring(path.lastIndexOf("/value/")+7, path.length());
                                    path = path.substring(0, path.length()-attribute.length()-7);
                                    String idElement = archetypeBinding
                                            .getArchetypeId() + path;
                                    archetypeBindingMVELSB.append("      ");
                                    String predicateHandle = "predicate"+ predicateCount;
                                    archetypeBindingMVELSB.append("$"+predicateHandle);
                                    archetypeBindingMVELSB
                                            .append(":ElementInstance(id==\""
                                                    + idElement +"\", "
                                                    + "dataValue!=null, "
                                                    + "archetypeReference==$"
                                                    + arID+arCount + ")\n");
                                    ArchetypeElementVO archetypeElement = ArchetypeElements
                                            .getArchetypeElement(
                                                    archetypeBinding.getTemplateId(),
                                                    idElement);
                                    if (archetypeElement!=null){
                                        String rmName = archetypeElement.getRMType();
                                        String aritmeticExpStr = //We cast it to long because all elements from CurrentTime fit into this class, but we must make it more generic (TODO)
                                                "((long)"+ExpressionUtil.getArithmeticExpressionStr(elementMap, binaryExpression.getRight(), null)+")";
                                        archetypeBindingMVELSB.append("      ");
                                        archetypeBindingMVELSB.append("eval(");
                                        Variable var = new Variable(predicateHandle, predicateHandle, path, attribute);
                                        String varCall = ExpressionUtil.getVariableWithAttributeStr(rmName,var);
                                        archetypeBindingMVELSB.append("(");
                                        if (isString(rmName, attribute)){
                                            archetypeBindingMVELSB.append(getAttributeOperatorMVELLine(varCall, binaryExpression.getOperator(), aritmeticExpStr));
                                        }else{
                                            archetypeBindingMVELSB.append(varCall);
                                            archetypeBindingMVELSB.append(binaryExpression.getOperator().getSymbol());
                                            archetypeBindingMVELSB.append(aritmeticExpStr);
                                        }
                                        archetypeBindingMVELSB.append("))\n");
                                    }else{
                                        throw new InternalErrorException(new Exception("Element not found '"+idElement+"'"+(archetypeBinding.getTemplateId()!=null?"("+archetypeBinding.getTemplateId()+")":"")));
                                    }
                                }
                            }
                        }else if (expressionItem instanceof UnaryExpression) {
                            UnaryExpression unaryExpression = (UnaryExpression) expressionItem;
                            predicateCount++;
                            Variable variable = (Variable) unaryExpression.getOperand();
                            String idElement = archetypeBinding
                                    .getArchetypeId() + variable.getPath();
                            archetypeBindingMVELSB.append("      ");
                            archetypeBindingMVELSB
                                    .append("ElementInstance(id==\""
                                            + idElement
                                            + "\", archetypeReference==$"
                                            + arID+arCount +", "
                                            + "predicate || dataValue instanceof Comparable, "
                                            + "$predDV"+ predicateCount+":dataValue"
                                            + ")\n");
                            ArchetypeElementVO archetypeElement = ArchetypeElements
                                    .getArchetypeElement(
                                            archetypeBinding.getTemplateId(),
                                            idElement);
                            OperatorKind op = unaryExpression.getOperator();
                            String opStr = null;
                            if (OperatorKind.MAX.equals(op)){
                                opStr=">";
                            }else if (OperatorKind.MIN.equals(op)){
                                opStr="<";
                            }else{
                                Logger.getLogger(GDLDroolsConverter.class).warn("Guide="+guide.getId()+", Operator for predicate '"+op+"' is not valid.");
                            }
                            if (archetypeElement!=null && opStr!=null){

                                String predAuxDef = getComparisonPredicateChecks(archetypeBinding, predicateCount);
                                String predicateArchetypeRef = "";
                                archetypeBindingMVELSB.append("      ");
                                archetypeBindingMVELSB.append("not(\n");
                                if (predAuxDef!=null){
                                    archetypeBindingMVELSB.append(predAuxDef);
                                    predicateArchetypeRef = "archetypeReference==$archetypeReferencePredicate"+predicateCount+",";
                                }
                                archetypeBindingMVELSB.append("      ");
                                archetypeBindingMVELSB
                                        .append("ElementInstance(id==\""
                                                + idElement + "\", "
                                                + predicateArchetypeRef
                                                + "DVUtil.areDomainsCompatible($"+arID+arCount +".getIdDomain(), archetypeReference.getIdDomain()),"
                                                + "DVUtil.checkMaxMin($predDV"+ predicateCount+", dataValue, \""+op.getSymbol()+"\")"
                                                +"))\n");
                            }else{
                                throw new InternalErrorException(new Exception("Element not found '"+idElement+"'"));
                            }
                        }
                    }
                }

                archetypeBindingIndexToDefinition.put(arCount,
                        archetypeBindingMVELSB.toString());
                Map<String, ElementBinding> elementBindingsMap = archetypeBinding.getElements();
                if (elementBindingsMap!=null){
                    for (ElementBinding element :elementBindingsMap.values()) {
                        StringBuffer elementDefinitionSB = new StringBuffer();
                        String idElement = archetypeBinding.getArchetypeId()
                                + element.getPath();

                        ArchetypeElementVO value =  ArchetypeElements.getArchetypeElement(
                                archetypeBinding.getTemplateId(), idElement);

                        elementMap.put(element.getId(), value);
                        elementDefinitionSB.append("ElementInstance(id==\""+idElement+"\", archetypeReference==$"+arID+arCount+")");
                        _gtElementToDefinition.put(element.getId(),
                                elementDefinitionSB.toString());
                        _gtElementToElementId.put(element.getId(), idElement);
                        gtElementToArchetypeBindingIndex.put(element.getId(), arCount);
                    }
                }
            }
        }

    }


    private String getComparisonPredicateChecks(ArchetypeBinding archetypeBinding, Integer predicateCount){
        StringBuffer sb = new StringBuffer();
        for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
            if (expressionItem instanceof BinaryExpression) {
                BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
                if (binaryExpression.getLeft() instanceof Variable
                        && binaryExpression.getRight() instanceof ConstantExpression) {
                    Variable variable = (Variable) binaryExpression.getLeft();
                    ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
                    String idElement = archetypeBinding.getArchetypeId() + variable.getPath();
                    sb.append("      ");
                    sb.append("$predicateAux"+ predicateCount);
                    sb.append(":ElementInstance(id==\"");
                    sb.append(idElement);
                    sb.append("\", archetypeReference==$archetypeReferencePredicate"+predicateCount+") and \n");
                    ArchetypeElementVO archetypeElement = ArchetypeElements
                            .getArchetypeElement(
                                    archetypeBinding.getTemplateId(),
                                    idElement);
                    if (archetypeElement!=null){
                        String rmType = archetypeElement.getRMType();
                        String dvStr = "null";
                        if (!"null".equals(constantExpression.getValue())){
                            dvStr = DVDefSerializer.getDVInstantiation(DataValue.parseValue(rmType+ ","+ constantExpression.getValue()));
                        }
                        sb.append("      ");
                        sb.append("eval("+
                                getOperatorMVELLine(
                                        "$predicateAux"+ predicateCount,
                                        binaryExpression.getOperator(),
                                        dvStr,
                                        true)+
                                ") and \n");
                    }
                }
            }
        }
        String predicateDef = sb.toString();
        if (!predicateDef.isEmpty()){
            sb = new StringBuffer();
            sb.append("      ");
            sb.append("$archetypeReferencePredicate"+predicateCount+":ArchetypeReference(idDomain==\"EHR\",");
            sb.append("idArchetype==\""+archetypeBinding.getArchetypeId()+"\") and \n");
            return sb.toString()+predicateDef;
        }else{
            return null;
        }
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
                definition.append(archetypeBindingIndexToDefinition.get(archetypeBindingIndex));
                archetypeDefinitions.put(archetypeBindingIndex, definition);
            }
            definition.append("      $"+elementGtCode+":"+_gtElementToDefinition.get(elementGtCode)+"\n");
        }
        StringBuffer resultSB = new StringBuffer();
        for (StringBuffer definition : archetypeDefinitions.values()) {
            resultSB.append(definition.toString());
        }

        for (String elementGtCode: gtCodesRef){
            Integer archetypeBindingIndex = gtElementToArchetypeBindingIndex
                    .get(elementGtCode);
            _gtElementToWholeDefinition.put(elementGtCode, archetypeDefinitions.get(archetypeBindingIndex).toString());
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
            Map<String, ArchetypeReference> archetypeReferenceMap,
            Map<String, ArchetypeElementVO> elementMap,
            Map<RefStat, Set<String>> stats) throws InternalErrorException {
        StringBuffer sb = new StringBuffer();
        if (expressionItems != null) {
            for (ExpressionItem expressionItem : expressionItems) {
                sb.append("   ");
                processExpressionItem(sb, expressionItem, archetypeReferenceMap, elementMap, stats);
                sb.append("\n");
            }
        }
        return sb.toString();
    }

    private String convertAssigmentExpressionsToMVEL(
            Collection<AssignmentExpression> expressionItems,
            Map<String, ArchetypeReference> archetypeReferenceMap,
            Map<String, ArchetypeElementVO> elementMap,
            Map<RefStat, Set<String>> stats) throws InternalErrorException {
        StringBuffer sb = new StringBuffer();
        if (expressionItems != null) {
            for (ExpressionItem expressionItem : expressionItems) {
                sb.append("   ");
                processExpressionItem(sb, expressionItem, archetypeReferenceMap, elementMap, stats);
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
                                         Map<String, ArchetypeReference> archetypeReferenceMap,
                                         Map<String, ArchetypeElementVO> elementMap,
                                         Map<RefStat, Set<String>> stats) throws InternalErrorException {
        if (expressionItem instanceof AssignmentExpression) {
            processAssigmentExpression(sb,
                    (AssignmentExpression) expressionItem, archetypeReferenceMap, elementMap, stats, false);
        } else if (expressionItem instanceof BinaryExpression) {
            processBinaryExpression(sb, (BinaryExpression) expressionItem, archetypeReferenceMap,
                    elementMap, stats);
        } else if (expressionItem instanceof UnaryExpression) {
            processUnaryExpression(sb, (UnaryExpression) expressionItem, archetypeReferenceMap,
                    elementMap, stats);
        } else {
            throw new InternalErrorException(new Exception("Unknown expression '"+ expressionItem.getClass().getName() + "'"));
        }
    }

    protected void processAssigmentExpression(StringBuffer sb,
                                              AssignmentExpression assignmentExpression,
                                              Map<String, ArchetypeReference> archetypeReferenceMap,
                                              Map<String, ArchetypeElementVO> elementMap,
                                              Map<RefStat, Set<String>> stats,
                                              boolean creatingInstance) throws InternalErrorException {
        String gtCode = assignmentExpression.getVariable().getCode();
        Variable var = assignmentExpression.getVariable();
        String attribute = var.getAttribute();
        processAssigmentExpression(sb, gtCode, gtCode, attribute, assignmentExpression.getAssignment(), archetypeReferenceMap, elementMap, stats, creatingInstance);
    }

    protected void processAssigmentExpression(StringBuffer sb,
                                              String gtCode,
                                              String eiId,
                                              String attribute,
                                              ExpressionItem expressionItemAux,
                                              Map<String, ArchetypeReference> archetypeReferenceMap,
                                              Map<String, ArchetypeElementVO> elementMap,
                                              Map<RefStat, Set<String>> stats,
                                              boolean creatingInstance) throws InternalErrorException {
        if (!CreateInstanceExpression.FUNCTION_CREATE_NAME.equals(attribute) && !creatingInstance){
            stats.get(RefStat.REFERENCE).add(gtCode);
            stats.get(RefStat.SET).add(gtCode);
        }
        if (attribute == null) {
            if (expressionItemAux instanceof Variable) {
                Variable var2 = (Variable) expressionItemAux;
                String gtCodeAux = var2.getCode();
                stats.get(RefStat.REFERENCE).add(gtCodeAux);
                sb.append("$"+eiId+".setDataValue($"+gtCodeAux+ExpressionUtil.getDataValueMethod(gtCode)+");");
                sb.append("$"+eiId+".setNullFlavour(null);");
                sb.append("$executionLogger.addLog(drools, $"+ eiId + ");");
            } else if (expressionItemAux instanceof ConstantExpression) {
                String dvStr =
                        ((ConstantExpression) expressionItemAux).getValue();
                ArchetypeElementVO archetypeElementVO = elementMap.get(gtCode);
                if (archetypeElementVO==null){
                    throw new InternalErrorException(new Exception("Guide="+guide.getId()+", Unknown element for gtCode '"+gtCode+"'"));
                }
                String rmType = archetypeElementVO.getRMType();
                DataValue dv = DataValue.parseValue(rmType + "," + dvStr);
                sb.append("$"+eiId+".setDataValue("+ DVDefSerializer.getDVInstantiation(dv)+");");
                sb.append("$"+eiId+".setNullFlavour(null);");
                sb.append("$executionLogger.addLog(drools, $"+eiId +");");
            } else {
                throw new InternalErrorException(new Exception("Guide="+guide.getId()+", Unknown expression '"+expressionItemAux+"'"));
            }
        } else {
            if (attribute.equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)){
                String dvStr = ((ConstantExpression) expressionItemAux).getValue();
                DataValue dv = DataValue.parseValue(OpenEHRDataValues.DV_CODED_TEXT + "," + dvStr);
                //Map<RefStat, Set<String>> statsAux = initStats(); //????
                //stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));  //????
                sb.append("$"+eiId+ ".setDataValue(null);");
                sb.append("$"+eiId+".setNullFlavour("+ DVDefSerializer.getDVInstantiation(dv)+");");
                sb.append("$executionLogger.addLog(drools, $"+eiId +");");
            }else if (attribute.equals(CreateInstanceExpression.FUNCTION_CREATE_NAME)) {
                ArchetypeReference ar = archetypeReferenceMap.get(gtCode);
                String arId = "newAR"+creationIndex;
                sb.append("ArchetypeReference " + arId + " = new ArchetypeReference(\"CDS\", \"" + ar.getIdArchetype() + "\","+(ar.getIdTemplate()!=null?"\""+ar.getIdTemplate()+"\"":"null")+");\n");
                sb.append("   insert("+arId+");\n");
                insertAssignments(sb, arId, archetypeReferenceMap, elementMap, expressionItemAux, stats);
                creationIndex++;
            }else{
                ArchetypeElementVO archetypeElementVO = elementMap.get(gtCode);
                if (archetypeElementVO==null){
                    throw new InternalErrorException(new Exception("GTCode '"+gtCode+"' not found. (guideId='"+guide.getId()+"')"));
                }
                String rmName = archetypeElementVO.getRMType();
                Map<RefStat, Set<String>> statsAux = initStats();
                String arithmeticExpStr =
                        ExpressionUtil.getArithmeticExpressionStr(elementMap, expressionItemAux, statsAux);
                stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));
                stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.ATT_FUNCTIONS_REF));
                stats.get(RefStat.ATT_SET_REF).addAll(statsAux.get(RefStat.REFERENCE));
                stats.get(RefStat.ATT_FUNCTIONS).addAll(statsAux.get(RefStat.ATT_FUNCTIONS));
                sb.append("$"+ eiId+ "."+ getAttributeSettingStr(eiId, rmName, attribute, arithmeticExpStr)+ ";");
                sb.append("$"+ eiId+ ".setNullFlavour(null);");
                sb.append("$executionLogger.addLog(drools, $"+ eiId + ");");
            }
        }
    }

    private void insertAssignments(
            StringBuffer sb, String arId,
            Map<String, ArchetypeReference> archetypeReferenceMap,
            Map<String, ArchetypeElementVO> elementMap,
            ExpressionItem expressionItem,
            Map<RefStat, Set<String>> stats) throws InternalErrorException {
        if (!(expressionItem instanceof MultipleAssignmentExpression)){
            throw new InternalErrorException(new Exception("Guide="+guide.getId()+", Incorrect expression inside creation expression '"+expressionItem+"'"));
        }
        MultipleAssignmentExpression multipleAssignmentExpression = (MultipleAssignmentExpression)expressionItem;
        int i = 0;
        Map<String, String> elementIdsMap = new HashMap<String, String>();
        for(AssignmentExpression assignmentExpressionAux: multipleAssignmentExpression.getAssignmentExpressions()){
            String gtCode = assignmentExpressionAux.getVariable().getCode();
            ArchetypeElementVO archetypeElementVO = elementMap.get(gtCode);
            if (archetypeElementVO==null){
                throw new InternalErrorException(new Exception("GTCode '"+gtCode+"' not found. (guideId='"+guide.getId()+"')"));
            }
            String elementId = archetypeElementVO.getId();
            String eiId = elementIdsMap.get(elementId);
            if (eiId==null){
                eiId = "ei"+creationIndex+"_"+i;
                elementIdsMap.put(elementId, eiId);
                sb.append("      ElementInstance $"+eiId+" = new ElementInstance(\""+elementId+"\", null, "+arId+", null, null);\n");
            }else{
                sb.append("\n");
            }
            sb.append("      ");
            String attribute = assignmentExpressionAux.getVariable().getAttribute();
            processAssigmentExpression(sb, gtCode, eiId, attribute, assignmentExpressionAux.getAssignment(), archetypeReferenceMap, elementMap, stats, true);
            sb.append("insert($" + eiId + ");");
            i++;
        }
    }

    private String getAttributeSettingStr(String gtCode, String rmName,
                                          String attributeName, String setStr) {
        return "setDataValue(DVUtil.createDV($" + gtCode + ",\"" + rmName
                + "\",\"" + attributeName + "\"," + setStr + "))";
    }

    protected void processBinaryExpression(StringBuffer sb,
                                           BinaryExpression binaryExpression,
                                           Map<String, ArchetypeReference> archetypeReferenceMap,
                                           Map<String, ArchetypeElementVO> elementMap,
                                           Map<RefStat, Set<String>> stats) throws InternalErrorException {
        if (OperatorKind.OR.equals(binaryExpression.getOperator())) {
            sb.append("(");
            processExpressionItem(sb, binaryExpression.getLeft(), archetypeReferenceMap, elementMap,
                    stats);
            sb.append(" or ");
            processExpressionItem(sb, binaryExpression.getRight(), archetypeReferenceMap, elementMap,
                    stats);
            sb.append(")");
        } else if (OperatorKind.AND.equals(binaryExpression.getOperator())) {
            sb.append("(");
            processExpressionItem(sb, binaryExpression.getLeft(), archetypeReferenceMap, elementMap,
                    stats);
            sb.append(" and ");
            processExpressionItem(sb, binaryExpression.getRight(), archetypeReferenceMap, elementMap,
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
            throw new InternalErrorException(new Exception("Unknown operator '"
                    + binaryExpression.getOperator() + "'"));
        }

    }

    protected void processUnaryExpression(StringBuffer sb,
                                          UnaryExpression unaryExpression,
                                          Map<String, ArchetypeReference> archetypeReferenceMap,
                                          Map<String, ArchetypeElementVO> elementMap,
                                          Map<RefStat, Set<String>> stats) throws InternalErrorException {
        if (OperatorKind.NOT.equals(unaryExpression.getOperator())) {
            sb.append("not(");
            processExpressionItem(sb, unaryExpression.getOperand(), archetypeReferenceMap, elementMap, stats);
            sb.append(")");
        } else if (OperatorKind.FOR_ALL.equals(unaryExpression.getOperator())) {
            sb.append("forall(");
            processExpressionItem(sb, unaryExpression.getOperand(), archetypeReferenceMap,elementMap,
                    stats);
            sb.append(")");
        } else {
            throw new InternalErrorException(new Exception(
                    "Unknown operator '" + unaryExpression.getOperator() + "'"));
        }
    }

    protected void processComparisonExpression(StringBuffer sb,
                                               BinaryExpression binaryExpression,
                                               Map<String, ArchetypeElementVO> elementMap,
                                               Map<RefStat, Set<String>> stats) throws InternalErrorException {
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
                        if (archetypeElementVO==null){
                            throw new InternalErrorException(new Exception("Element '"+var.getCode()+"' not found. (guideId='"+guide.getId()+"')"));
                        }
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
                            sb.append("eval($" + var.getCode()+ ".hasNoValue(\""+guide.getId()+"/"+var.getCode()+"\"))");
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
                    throw new InternalErrorException(new Exception(
                            "Unknown expression '"
                                    + binaryExpression.getRight().getClass()
                                    .getName() + "'"));
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
                            ExpressionUtil.getArithmeticExpressionStr(elementMap, binaryExpression.getRight(), statsAux);
                    //Add stats
                    ExpressionUtil.getArithmeticExpressionStr(elementMap, binaryExpression.getLeft(), statsAux);
                    stats.get(RefStat.REFERENCE).addAll(statsAux.get(RefStat.REFERENCE));
                    stats.get(RefStat.ATT_FUNCTIONS).addAll(statsAux.get(RefStat.ATT_FUNCTIONS));

                    statsAux.get(RefStat.REFERENCE).remove(OpenEHRConst.CURRENT_DATE_TIME_ID);
                    sb.append("eval(");
                    for (String gtCode : statsAux.get(RefStat.REFERENCE)) {
                        sb.append("$" + gtCode + ".hasValue() && ");
                    }
                    String rmName = elementMap.get(var.getCode()).getRMType();
                    sb.append("(");
                    String varCall = ExpressionUtil.getVariableWithAttributeStr(rmName, var);
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
            throw new InternalErrorException(new Exception("Unknown expression '" + binaryExpression.getLeft() + "'"));
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
            String[] codeSplit = gtCodesWithFunction.split(ExpressionUtil.CODE_FUNCTION_SEPARATOR);
            String code = codeSplit[0];
            String att = codeSplit[1];
            if (ExpressionUtil.isFunction(att)){
                if (OpenEHRDataValues.FUNCTION_COUNT.equals(att)){
                    String elementId = _gtElementToElementId.get(code);
                    //String def = "ElementInstance(id==\""+elementId+"\", dataValue!=null)";
                    /*TODO HACK - Should be done in a proper way...*/
                    String definition = _gtElementToWholeDefinition.get(code);
                    String defAux =
                            definition
                                    .replace("\n"," and\n")
                                    .replace("$","$count_")
                                    .replace("eval(DVUtil.equalDV(true, $count_predicate", "eval(DVUtil.equalDV(false, $count_predicate")
                                    .replace("eval(DVUtil.isSubClassOf(true, $count_predicate", "eval(DVUtil.isSubClassOf(false, $count_predicate")
                                    .replace("$count_"+code+":ElementInstance(", "$count_"+code+":ElementInstance(!predicate, dataValue!=null, ")
                                    .replace("$count_"+OpenEHRConst.CURRENT_DATE_TIME_ID,"$"+OpenEHRConst.CURRENT_DATE_TIME_ID);
                    if (defAux.length()>5){
                        //Remove last 'and'
                        defAux = defAux.substring(0, defAux.length()-5);
                    }
                    sb.append("   Number($"+code+att+":intValue) from accumulate ("+defAux+", count($count_"+code+"))\n   ");
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
            return getEqualsString(handle, value, inPredicate, false);
        } else if (OperatorKind.INEQUAL.equals(ok)) {
            return getEqualsString(handle, value, inPredicate, true);
        } else if (OperatorKind.IS_A.equals(ok)) {
            return "DVUtil.isSubClassOf("+ inPredicate+", "+ handle + ", $bindingMap, "+ getTermBindings(value) + ")";
        } else if (OperatorKind.IS_NOT_A.equals(ok)) {
            return "DVUtil.isNotSubClassOf(" + inPredicate + ", "+ handle+", $bindingMap, "+ getTermBindings(value) + ")";
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


    private static String getEqualsString(String handle, String value, boolean inPredicate, boolean negated){
        StringBuffer sb = new StringBuffer();
        sb.append("DVUtil.equalDV("+inPredicate+", "+handle+"," + value+", "+negated);
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
            Logger.getLogger(GDLDroolsConverter.class).warn("Guide="+guide.getId()+", Illegar operator '"+ok.getSymbol()+"' used in handle '"+handle+"'.");
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
            Logger.getLogger(GDLDroolsConverter.class).warn("Guide="+guide.getId()+", Subclass comparison between elements is not supported.");
            //TODO Give support to subclass comparison between elements
            return "null";
        }
        Map<String, TermBinding> termBindings = guide.getOntology().getTermBindings();
        // TODO log.warn if gt code is unbound to terminologies
        if(termBindings == null) {
            //Logger.getLogger(GDLDroolsConverter.class).warn("Guide="+guide.getId()+", Needed terminology binding not found on guide.");
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
                + "import org.openehr.rm.datatypes.quantity.DvOrdered;\n"
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
                + "global org.openehr.rm.datatypes.quantity.datetime.DvDateTime $"+OpenEHRConst.CURRENT_DATE_TIME_ID + ";\n"
                + "global java.util.Map<se.cambio.cds.model.instance.ElementInstance, java.util.Map<String, Boolean>> $bindingMap;\n"
                + "\n";
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
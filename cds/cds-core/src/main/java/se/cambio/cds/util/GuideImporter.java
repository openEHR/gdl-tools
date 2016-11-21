package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.*;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeDataValueRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeElementRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.GTCodeDefiner;
import se.cambio.cds.gdl.model.readable.util.RulePriorityComparator;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.cm.model.util.OpenEHRRMUtil;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

public class GuideImporter {

    private ReadableGuide readableGuide;
    private TermDefinition termDefinition;
    private ArchetypeManager archetypeManager;
    private Map<String, GTCodeDefiner> gtCodeElementMap;
    private static Logger log = Logger.getLogger(GuideImporter.class);

    public GuideImporter(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
    }

    public ReadableGuide importGuide(Guide guide, String language) throws InternalErrorException {
        Logger.getLogger(GuideImporter.class).debug("Importing guide: " + guide.getId() + ", lang=" + language);
        GuideDefinition guideDefinition = guide.getDefinition();
        termDefinition = getTermDefinition(guide, language);
        readableGuide = new ReadableGuide(termDefinition, archetypeManager);
        generateGTCodeElementMap(guide);
        if (guideDefinition != null) {
            Map<String, ArchetypeBinding> archetypeBindings = guideDefinition.getArchetypeBindings();
            if (archetypeBindings != null) {
                for (ArchetypeBinding archetypeBinding : archetypeBindings.values()) {
                    ArchetypeInstantiationRuleLine airl = (ArchetypeInstantiationRuleLine) gtCodeElementMap.get(archetypeBinding.getId());
                    readableGuide.getDefinitionRuleLines().add(airl);
                    if (archetypeBinding.getPredicateStatements() != null) {
                        processPredicateExpressions(archetypeBinding, airl);
                    }
                }
            }
            List<ExpressionItem> preConditionExpressions = guideDefinition.getPreConditionExpressions();
            if (preConditionExpressions != null) {
                for (ExpressionItem expressionItem : preConditionExpressions) {
                    processExpressionItem(
                            readableGuide.getPreconditionRuleLines(), null,
                            expressionItem);
                }
            }
            List<AssignmentExpression> defaultActionExpressions = guideDefinition.getDefaultActionExpressions();
            if (defaultActionExpressions != null) {
                for (ExpressionItem expressionItem : defaultActionExpressions) {
                    processExpressionItem(readableGuide.getDefaultActions(), null, expressionItem);
                }
            }
            Map<String, Rule> rulesMap = guideDefinition.getRules();
            if (rulesMap != null) {
                List<Rule> rules = new ArrayList(rulesMap.values());
                Collections.sort(rules, new RulePriorityComparator());
                for (Rule rule : rules) {
                    ReadableRule rr = new ReadableRule(readableGuide.getTermDefinition(), rule.getId(), readableGuide);
                    readableGuide.getReadableRules().put(rule.getId(), rr);
                    if (rule.getWhenStatements() != null) {
                        for (ExpressionItem expressionItem : rule.getWhenStatements()) {
                            processExpressionItem(rr.getConditionRuleLines(), null, expressionItem);
                        }
                    }
                    if (rule.getThenStatements() != null) {
                        for (ExpressionItem expressionItem : rule.getThenStatements()) {
                            processExpressionItem(rr.getActionRuleLines(), null, expressionItem);
                        }
                    }
                }
            }
        }
        return readableGuide;
    }

    private void processPredicateExpressions(ArchetypeBinding archetypeBinding, ArchetypeInstantiationRuleLine airl) throws InternalErrorException {
        for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
            if (expressionItem instanceof BinaryExpression) {
                BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
                if (binaryExpression.getLeft() instanceof Variable &&
                        binaryExpression.getRight() instanceof ConstantExpression) {
                    Variable variable = (Variable) binaryExpression.getLeft();
                    ConstantExpression constantExpression2 = (ConstantExpression) binaryExpression.getRight();
                    String path = variable.getPath();
                    if ("/event/time".equals(path)) {
                        //Old event time detected //TODO Remove later on
                        path = OpenEHRRMUtil.EVENT_TIME_PATH;
                    }
                    String dvStr = constantExpression2.getValue();
                    ArchetypeElementVO archetypeElementVO =
                            airl.getArchetypeManager().getArchetypeElements().getArchetypeElement(
                                    archetypeBinding.getTemplateId(),
                                    archetypeBinding.getArchetypeId() + path);
                    if (archetypeElementVO == null) {
                        throw new InternalErrorException(new Exception("Element '" + archetypeBinding.getArchetypeId() + path + (archetypeBinding.getTemplateId() != null ? " (" + archetypeBinding.getTemplateId() + ")" : "") + "' not found!"));
                    }
                    if (!dvStr.equals("null")) {
                        WithElementPredicateAttributeDefinitionRuleLine wepdrl = new WithElementPredicateAttributeDefinitionRuleLine();
                        airl.addChildRuleLine(wepdrl);
                        wepdrl.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
                        String rmType = archetypeElementVO.getRMType();
                        if (OpenEHRDataValues.DV_TEXT.equals(rmType) &&
                                (OperatorKind.IS_A.equals(binaryExpression.getOperator()) || OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()))) {
                            rmType = OpenEHRDataValues.DV_CODED_TEXT;
                        }
                        DataValue dv = parseGTDataValue(rmType, dvStr);
                        wepdrl.getDataValueRuleLineElement().setValue(dv);
                        wepdrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                    } else {
                        WithElementPredicateExistsDefinitionRuleLine wepedrl = new WithElementPredicateExistsDefinitionRuleLine();
                        airl.addChildRuleLine(wepedrl);
                        wepedrl.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
                        wepedrl.getExistenceOperatorRuleLineElement().setValue(binaryExpression.getOperator().getSymbol() + "null");
                    }
                } else if (binaryExpression.getLeft() instanceof Variable &&
                        binaryExpression.getRight() instanceof ExpressionItem) {
                    Variable variable = (Variable) binaryExpression.getLeft();
                    ExpressionItem expressionItemAux = binaryExpression.getRight();
                    WithElementPredicateExpressionDefinitionRuleLine wepdrl = new WithElementPredicateExpressionDefinitionRuleLine(airl);
                    String path = variable.getPath();
                    String attribute = path.substring(path.lastIndexOf("/value/") + 7, path.length());
                    path = path.substring(0, path.length() - attribute.length() - 7);
                    if ("/event/time".equals(path)) {
                        //Old event time detected //TODO Remove later on
                        path = OpenEHRRMUtil.EVENT_TIME_PATH;
                    }
                    ArchetypeElementVO archetypeElementVO =
                            airl.getArchetypeManager().getArchetypeElements().getArchetypeElement(
                                    archetypeBinding.getTemplateId(),
                                    archetypeBinding.getArchetypeId() + path);
                    if (archetypeElementVO == null) {
                        throw new InternalErrorException(new Exception("Element '" + archetypeBinding.getArchetypeId() + path + (archetypeBinding.getTemplateId() != null ? " (" + archetypeBinding.getTemplateId() + ")" : "") + "' not found!"));
                    }
                    wepdrl.getArchetypeElementAttributeRuleLineDefinitionElement().setValue(archetypeElementVO);
                    wepdrl.getArchetypeElementAttributeRuleLineDefinitionElement().setAttribute(attribute);
                    wepdrl.getExpressionRuleLineElement().setValue(expressionItemAux);
                    wepdrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                }
            } else if (expressionItem instanceof UnaryExpression) {
                UnaryExpression unaryExpression = (UnaryExpression) expressionItem;
                WithElementPredicateFunctionDefinitionRuleLine wefd = new WithElementPredicateFunctionDefinitionRuleLine();
                Variable variable = (Variable) unaryExpression.getOperand();
                airl.addChildRuleLine(wefd);
                String path = variable.getPath();
                if ("/event/time".equals(path)) {
                    //Old event time detected //TODO Remove later on
                    path = OpenEHRRMUtil.EVENT_TIME_PATH;
                }
                ArchetypeElementVO archetypeElementVO =
                        airl.getArchetypeManager().getArchetypeElements().getArchetypeElement(
                                archetypeBinding.getTemplateId(),
                                archetypeBinding.getArchetypeId() + path);
                if (archetypeElementVO == null) {
                    throw new InternalErrorException(new Exception("Element '" + archetypeBinding.getArchetypeId() + path + (archetypeBinding.getTemplateId() != null ? " (" + archetypeBinding.getTemplateId() + ")" : "") + "' not found!"));
                }
                wefd.getArchetypeElementRuleLineDefinitionElement().setValue(archetypeElementVO);
                wefd.getFunctionRuleLineElement().setValue(unaryExpression.getOperator());
            }
        }
    }

    public static TermDefinition getTermDefinition(Guide guide, String lang) {
        TermDefinition termDefinition = null;
        if (guide.getOntology() != null) {
            if (guide.getOntology().getTermDefinitions() != null) {
                termDefinition = guide.getOntology().getTermDefinitions().get(lang);
                if (termDefinition == null) {
                    termDefinition = guide.getOntology().getTermDefinitions().get(guide.getLanguage().getOriginalLanguage().getCodeString());
                }
            }
        }
        if (termDefinition == null) {
            termDefinition = new TermDefinition();
        }
        return termDefinition;
    }

    private void addRuleLine(RuleLine ruleLine, RuleLineCollection ruleLines, RuleLine parentRuleLine) {
        if (parentRuleLine != null) {
            parentRuleLine.addChildRuleLine(ruleLine);
        } else {
            ruleLines.add(ruleLine);
        }
    }

    protected DataValue parseDataValue(String rmType, String dvStr, ArchetypeElementVO archetypeElementVO) {
        DataValue dv = DataValue.parseValue(rmType + "," + dvStr);
        if (dv instanceof DvCodedText) {
            if (archetypeElementVO != null) {
                DvCodedText dvCT = (DvCodedText) dv;
                String name = archetypeManager.getCodedTexts().getText(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), dvCT.getCode(), UserConfigurationManager.instance().getLanguage());
                if (name != null) {
                    dvCT.setValue(name);
                }
            }
        } else if (dv instanceof DvOrdinal) {
            if (archetypeElementVO != null) {
                DvOrdinal dvOrdinal = (DvOrdinal) dv;
                String name = archetypeManager.getOrdinals().getText(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId(), dvOrdinal.getCode(), UserConfigurationManager.instance().getLanguage());
                if (name != null) {
                    dvOrdinal.getSymbol().setValue(name);
                }
            }
        }
        return dv;
    }

    protected DataValue parseGTDataValue(String rmType, String dvStr) {
        DataValue dv = DataValue.parseValue(rmType + "," + dvStr);
        if (dv instanceof DvCodedText) {
            if (termDefinition != null) {
                DvCodedText dvCT = (DvCodedText) dv;
                Term term = termDefinition.getTerms().get(dvCT.getCode());
                if (term != null && term.getText() != null) {
                    dvCT.setValue(term.getText());
                }
            }
        } else if (dv instanceof DvOrdinal) {
            if (termDefinition != null) {
                DvOrdinal dvOrdinal = (DvOrdinal) dv;
                String name = termDefinition.getTerms().get(dvOrdinal.getCode()).getText();
                if (name != null) {
                    dvOrdinal.getSymbol().setValue(name);
                }
            }
        }
        return dv;
    }

    protected void processAssignmentExpression(
            RuleLineCollection ruleLines,
            AssignmentExpression assignmentExpression) throws InternalErrorException {
        String gtCode = assignmentExpression.getVariable().getCode();
        ExpressionItem expressionItemAux = assignmentExpression.getAssignment();
        String attribute = assignmentExpression.getVariable().getAttribute();
        GTCodeRuleLineElement gtCodeRuleLineElement =
                gtCodeElementMap.get(gtCode).getGTCodeRuleLineElement();
        if (attribute == null) {
            if (expressionItemAux instanceof Variable) {
                String gtCodeAux = ((Variable) expressionItemAux).getCode();
                SetElementWithElementActionRuleLine sewearl = new SetElementWithElementActionRuleLine();
                ruleLines.add(sewearl);
                sewearl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                GTCodeRuleLineElement gtCodeRuleLineElementAux =
                        gtCodeElementMap.get(gtCodeAux).getGTCodeRuleLineElement();
                sewearl.getSecondArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElementAux);
            } else if (expressionItemAux instanceof ConstantExpression) {
                SetElementWithDataValueActionRuleLine sedvar = new SetElementWithDataValueActionRuleLine();
                ruleLines.add(sedvar);
                sedvar.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                ArchetypeDataValueRuleLineElement archetypeDataValueRuleLineElement = sedvar.getArchetypeDataValueRuleLineElement();
                String dvStr = ((ConstantExpression) expressionItemAux).getValue();
                ArchetypeElementVO archetypeElementVO = null;
                if (gtCodeElementMap.get(gtCode) instanceof ArchetypeElementRuleLine) {
                    archetypeElementVO =
                            ((ArchetypeElementRuleLine) gtCodeElementMap.get(gtCode)).getArchetypeElement();
                }
                if (archetypeElementVO == null) {
                    throw new InternalErrorException(new Exception("Archetype element not found for gtCode '" + gtCode + "'"));
                }
                log.debug("processAssignmentExpression for variable: " + gtCode);

                String rmType = archetypeElementVO.getRMType();
                DataValue dv = parseDataValue(rmType, dvStr, archetypeElementVO);
                archetypeDataValueRuleLineElement.setArchetypeElementVO(archetypeElementVO);
                archetypeDataValueRuleLineElement.setValue(dv);
            } else {
                throw new InternalErrorException(new Exception("Unknown expression '" + expressionItemAux.getClass().getName() + "'"));
            }
        } else {
            if (CreateInstanceExpression.FUNCTION_CREATE_NAME.equals(attribute)) {
                CreateInstanceActionRuleLine cirl = new CreateInstanceActionRuleLine();
                ruleLines.add(cirl);
                RuleLineCollection ruleLinesAssignmentInstance = new RuleLineCollection(cirl.getReadableGuide());
                if (!(expressionItemAux instanceof MultipleAssignmentExpression)) {
                    throw new InternalErrorException(new Exception("Unknown expression in creation statement '" + expressionItemAux.toString() + "'"));
                }
                Collection<AssignmentExpression> assignmentExpressions = ((MultipleAssignmentExpression) expressionItemAux).getAssignmentExpressions();
                for (AssignmentExpression assignmentExpressionAux : assignmentExpressions) {
                    processAssignmentExpression(ruleLinesAssignmentInstance, assignmentExpressionAux);
                }
                for (RuleLine ruleLine : ruleLinesAssignmentInstance.getRuleLines()) {
                    cirl.addChildRuleLine(ruleLine);
                }
                cirl.setCDSEntryGTCodeRuleLineElementValue(gtCodeRuleLineElement);
            } else if (OpenEHRConst.NULL_FLAVOR_ATTRIBUTE.equals(attribute)) {
                SetElementWithNullValueActionRuleLine sewnvrl = new SetElementWithNullValueActionRuleLine();
                ruleLines.add(sewnvrl);
                sewnvrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                String dvStr = ((ConstantExpression) expressionItemAux).getValue();
                DataValue dv = parseDataValue(OpenEHRDataValues.DV_CODED_TEXT, dvStr, null);
                sewnvrl.getNullValueRuleLineElement().setValue((DvCodedText) dv);
            } else {
                SetElementAttributeActionRuleLine seaarl = new SetElementAttributeActionRuleLine();
                ruleLines.add(seaarl);
                seaarl.getArchetypeElementAttributeRuleLineElement().setAttribute(attribute);
                ArchetypeElementRuleLineElement aerle = new ArchetypeElementRuleLineElement(seaarl);
                aerle.setValue(gtCodeRuleLineElement);
                seaarl.getArchetypeElementAttributeRuleLineElement().setValue(aerle);
                seaarl.getExpressionRuleLineElement().setValue(expressionItemAux);
            }
        }
    }

    protected void processBinaryExpression(
            RuleLineCollection ruleLines,
            RuleLine parentRuleLine,
            BinaryExpression binaryExpression) throws InternalErrorException {
        if (OperatorKind.OR.equals(binaryExpression.getOperator())) {
            OrOperatorRuleLine orOperatorRuleLine = new OrOperatorRuleLine();
            processExpressionItem(ruleLines, orOperatorRuleLine.getLeftRuleLineBranch(), binaryExpression.getLeft());
            processExpressionItem(ruleLines, orOperatorRuleLine.getRightRuleLineBranch(), binaryExpression.getRight());
            addRuleLine(orOperatorRuleLine, ruleLines, parentRuleLine);
        } else if (OperatorKind.AND.equals(binaryExpression.getOperator())) {
            processExpressionItem(ruleLines, parentRuleLine, binaryExpression.getLeft());
            processExpressionItem(ruleLines, parentRuleLine, binaryExpression.getRight());
        } else if (OperatorKind.EQUALITY.equals(binaryExpression.getOperator()) ||
                OperatorKind.INEQUAL.equals(binaryExpression.getOperator()) ||
                OperatorKind.IS_A.equals(binaryExpression.getOperator()) ||
                OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()) ||
                OperatorKind.GREATER_THAN.equals(binaryExpression.getOperator()) ||
                OperatorKind.GREATER_THAN_OR_EQUAL.equals(binaryExpression.getOperator()) ||
                OperatorKind.LESS_THAN.equals(binaryExpression.getOperator()) ||
                OperatorKind.LESS_THAN_OR_EQUAL.equals(binaryExpression.getOperator())) {
            processComparisonExpression(ruleLines, parentRuleLine, binaryExpression);
        } else {
            throw new InternalErrorException(new Exception("Unknown operator '" + binaryExpression.getOperator() + "'"));
        }

    }

    protected void processComparisonExpression(
            RuleLineCollection ruleLines,
            RuleLine parentRuleLine,
            BinaryExpression binaryExpression) throws InternalErrorException {
        GTCodeRuleLineElement gtCodeRuleLineElement = null;
        String attribute = null;
        String gtCode = null;
        if (binaryExpression.getLeft() instanceof Variable) {
            Variable var = (Variable) binaryExpression.getLeft();
            gtCode = var.getCode();
            GTCodeDefiner gtCodeDefiner = gtCodeElementMap.get(gtCode);
            if (gtCodeDefiner != null) {
                gtCodeRuleLineElement = gtCodeDefiner.getGTCodeRuleLineElement();
            } else {
                throw new InternalErrorException(new Exception("gtCode not found! (" + gtCode + ")"));
            }
            attribute = var.getAttribute();
        }
        if (gtCodeRuleLineElement != null) {
            if (attribute == null) {
                if (binaryExpression.getRight() instanceof ConstantExpression) {
                    ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
                    String dvStr = constantExpression.getValue();
                    DataValue dv = null;
                    ArchetypeElementVO archetypeElementVO = null;
                    if (!dvStr.equals("null")) {
                        log.debug("codeRuleLineElement: " + gtCodeRuleLineElement.getValue());
                        String rmType;
                        if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)) {
                            if (gtCodeElementMap.get(gtCode) instanceof ArchetypeElementRuleLine) {
                                archetypeElementVO = ((ArchetypeElementRuleLine) gtCodeElementMap.get(gtCode)).getArchetypeElement();
                            }
                            if (archetypeElementVO == null) {
                                throw new InternalErrorException(new Exception("Archetype element not found for gtCode '" + gtCodeRuleLineElement.getValue() + "'"));
                            }
                            rmType = archetypeElementVO.getRMType();
                            if (OpenEHRDataValues.DV_TEXT.equals(rmType) &&
                                    (OperatorKind.IS_A.equals(binaryExpression.getOperator()) ||
                                            OperatorKind.IS_NOT_A.equals(binaryExpression.getOperator()))) {
                                rmType = OpenEHRDataValues.DV_CODED_TEXT;
                            }
                        } else {
                            rmType = OpenEHRDataValues.DV_DATE_TIME;
                        }
                        dv = parseDataValue(rmType, dvStr, archetypeElementVO);
                    }
                    if (dv != null) {
                        ElementComparisonWithDVConditionRuleLine eccrl = new ElementComparisonWithDVConditionRuleLine();
                        addRuleLine(eccrl, ruleLines, parentRuleLine);
                        eccrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                        eccrl.getArchetypeDataValueRuleLineElement().setValue(dv);
                        eccrl.getArchetypeDataValueRuleLineElement().setArchetypeElementVO(archetypeElementVO);
                        eccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                    } else {
                        ElementInitializedConditionRuleLine eicrl = new ElementInitializedConditionRuleLine();
                        addRuleLine(eicrl, ruleLines, parentRuleLine);
                        eicrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                        eicrl.getExistenceOperatorRuleLineElement().setOperator(binaryExpression.getOperator());
                    }
                } else if (binaryExpression.getRight() instanceof Variable) {
                    Variable varRight = (Variable) binaryExpression.getRight();
                    String gtCodeAux = varRight.getCode();
                    ElementComparisonWithElementConditionRuleLine eccrl = new ElementComparisonWithElementConditionRuleLine();
                    addRuleLine(eccrl, ruleLines, parentRuleLine);
                    eccrl.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                    eccrl.getSecondArchetypeElementRuleLineElement().setValue(gtCodeElementMap.get(gtCodeAux).getGTCodeRuleLineElement());
                    eccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                } else {
                    throw new InternalErrorException(new Exception("Unknown expression '" + binaryExpression.getRight().getClass().getName() + "'"));
                }
            } else {
                if (attribute.equals(OpenEHRConst.NULL_FLAVOR_ATTRIBUTE)) {
                    ElementComparisonWithNullValueConditionRuleLine ecwnvc = new ElementComparisonWithNullValueConditionRuleLine();
                    addRuleLine(ecwnvc, ruleLines, parentRuleLine);
                    ecwnvc.getArchetypeElementRuleLineElement().setValue(gtCodeRuleLineElement);
                    ConstantExpression constantExpression = (ConstantExpression) binaryExpression.getRight();
                    String dvStr = constantExpression.getValue();
                    DataValue dv = parseDataValue(OpenEHRDataValues.DV_CODED_TEXT, dvStr, null);
                    if (dv instanceof DvCodedText) {
                        ecwnvc.getNullValueRuleLineElement().setValue((DvCodedText) dv);
                    }
                    ecwnvc.getEqualityComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                } else { //Expression
                    ElementAttributeComparisonConditionRuleLine eaccrl = new ElementAttributeComparisonConditionRuleLine();
                    addRuleLine(eaccrl, ruleLines, parentRuleLine);
                    eaccrl.getArchetypeElementAttributeRuleLineElement().setAttribute(attribute);
                    ArchetypeElementRuleLineElement aerle = new ArchetypeElementRuleLineElement(eaccrl);
                    aerle.setValue(gtCodeRuleLineElement);
                    eaccrl.getArchetypeElementAttributeRuleLineElement().setValue(aerle);
                    eaccrl.getExpressionRuleLineElement().setValue(binaryExpression.getRight());
                    eaccrl.getComparisonOperatorRuleLineElement().setValue(binaryExpression.getOperator());
                }
            }
        } else {
            throw new InternalErrorException(new Exception("Unknown expression '" + binaryExpression.getLeft().getClass().getName() + "'"));
        }
    }

    public void processExpressionItem(
            RuleLineCollection ruleLines,
            RuleLine parentRuleLine,
            ExpressionItem expressionItem) throws InternalErrorException {
        if (expressionItem instanceof AssignmentExpression) {
            processAssignmentExpression(ruleLines, (AssignmentExpression) expressionItem);
        } else if (expressionItem instanceof BinaryExpression) {
            processBinaryExpression(ruleLines, parentRuleLine, (BinaryExpression) expressionItem);
        } else if (expressionItem instanceof UnaryExpression) {
            processUnaryExpression(ruleLines, parentRuleLine, (UnaryExpression) expressionItem);
        } else {
            throw new InternalErrorException(new Exception("Unknown expression '" + expressionItem.getClass().getName() + "'"));
        }
    }

    protected void processUnaryExpression(
            RuleLineCollection ruleLines,
            RuleLine parentRuleLine,
            UnaryExpression unaryExpression) throws InternalErrorException {
        OperatorKind operator = unaryExpression.getOperator();
        if (OperatorKind.FIRED.equals(operator) ||
                OperatorKind.NOT_FIRED.equals(operator)) {
            processFiredRuleCondition(ruleLines, parentRuleLine, unaryExpression);
        } else if (OperatorKind.NOT.equals(operator)) {
            NotOperatorRuleLine notOperatorRuleLine = new NotOperatorRuleLine();
            processExpressionItem(ruleLines, notOperatorRuleLine, unaryExpression.getOperand());
            addRuleLine(notOperatorRuleLine, ruleLines, parentRuleLine);
        } else {
            throw new InternalErrorException(new Exception("Unknown operator '" + operator + "'"));
        }
    }

    private void processFiredRuleCondition(RuleLineCollection ruleLines, RuleLine parentRuleLine, UnaryExpression unaryExpression) throws InternalErrorException {
        if (unaryExpression.getOperand() instanceof Variable) {
            FiredRuleConditionRuleLine firedRuleConditionRuleLine = new FiredRuleConditionRuleLine();
            addRuleLine(firedRuleConditionRuleLine, ruleLines, parentRuleLine);
            Variable variable = (Variable) unaryExpression.getOperand();
            GTCodeDefiner gtCodeDefiner = gtCodeElementMap.get(variable.getCode());
            firedRuleConditionRuleLine.getFiredRuleReferenceRuleElement().setValue(gtCodeDefiner.getGTCodeRuleLineElement());
            firedRuleConditionRuleLine.getFiredRuleOperatorRuleLineElement().setValue(unaryExpression.getOperator());
        } else {
            throw new InternalErrorException(new Exception("Variable expected, got  '" + unaryExpression.getOperand() + "'"));
        }
    }

    public void generateGTCodeElementMap(Guide guide) throws InternalErrorException {
        gtCodeElementMap = new HashMap<String, GTCodeDefiner>();
        ArchetypeElementInstantiationRuleLine dummyAEIRL = new ArchetypeElementInstantiationRuleLine(new ArchetypeInstantiationRuleLine());
        dummyAEIRL.setReadableGuide(readableGuide);
        dummyAEIRL.setGTCode("currentDateTime");
        gtCodeElementMap.put("currentDateTime", dummyAEIRL);
        GuideDefinition guideDefinition = guide.getDefinition();
        Map<String, ArchetypeBinding> ab = guideDefinition.getArchetypeBindings();
        for (ArchetypeBinding archetypeBinding : ab.values()) {
            generateGtCodeMapForArchetypeBinding(archetypeBinding);
        }
        generateGTCodeMapForRules(guide);
    }

    private void generateGTCodeMapForRules(Guide guide) {
        for (Rule rule : guide.getDefinition().getRules().values()) {
            FiredRuleInstantiationRuleLine frirl = new FiredRuleInstantiationRuleLine();
            frirl.setGTCode(rule.getId());
            frirl.setReadableGuide(readableGuide);
            gtCodeElementMap.put(rule.getId(), frirl);
        }
    }

    private void generateGtCodeMapForArchetypeBinding(ArchetypeBinding archetypeBinding) throws InternalErrorException {
        ArchetypeInstantiationRuleLine airl =
                new ArchetypeInstantiationRuleLine();
        airl.setReadableGuide(readableGuide);
        airl.setGTCode(archetypeBinding.getId());
        ArchetypeReference ar =
                new ArchetypeReference(
                        archetypeBinding.getDomain(),
                        archetypeBinding.getArchetypeId(),
                        archetypeBinding.getTemplateId());
        airl.setArchetypeReference(ar);
        gtCodeElementMap.put(archetypeBinding.getId(), airl);
        if (archetypeBinding.getElements() != null) {
            for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                generateGTCodeMapForElementBinding(archetypeBinding, airl, elementBinding);
            }
        }
    }

    private void generateGTCodeMapForElementBinding(ArchetypeBinding archetypeBinding, ArchetypeInstantiationRuleLine airl, ElementBinding elementBinding) throws InternalErrorException {
        ArchetypeElementInstantiationRuleLine aeirl =
                new ArchetypeElementInstantiationRuleLine(airl);
        aeirl.setReadableGuide(readableGuide);
        aeirl.setGTCode(elementBinding.getId());
        if ("/event/time".equals(elementBinding.getPath())) {
            //Old event time detected //TODO Remove later on
            elementBinding.setPath(OpenEHRRMUtil.EVENT_TIME_PATH);
        }
        String elementId =
                archetypeBinding.getArchetypeId() + elementBinding.getPath();
        ArchetypeElementVO archetypeElementVO =
                archetypeManager.getArchetypeElements().getArchetypeElement(
                        archetypeBinding.getTemplateId(),
                        elementId);
        if (archetypeElementVO == null) {
            throw new InternalErrorException(new Exception("Element '" + elementId + (archetypeBinding.getTemplateId() != null ? " (" + archetypeBinding.getTemplateId() + ")" : "") + "' not found!"));
        }
        aeirl.setArchetypeElementVO(archetypeElementVO);
        gtCodeElementMap.put(elementBinding.getId(), aeirl);
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
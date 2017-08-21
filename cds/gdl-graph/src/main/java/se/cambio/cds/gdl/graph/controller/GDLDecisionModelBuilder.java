package se.cambio.cds.gdl.graph.controller;

import org.openehr.rm.datatypes.basic.DataValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;


class GDLDecisionModelBuilder {

    private Map<String, Guide> guideMap = null;
    private Map<RuleReference, ElementInstance> allElementInstancesByRuleReference = null;
    private Map<String, Collection<GuideBinaryExpressionItem>> allConditionsByElementIdMap;
    private Map<String, Collection<GuideAssignmentExpression>> allAssignmentsByElementIdMap;
    private Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> _decisionMap;
    private static Logger logger = LoggerFactory.getLogger(GDLDecisionModelBuilder.class);
    private ElementInstanceCollectionManager elementInstanceCollectionManager;


    GDLDecisionModelBuilder(Collection<Guide> guides, ElementInstanceCollectionManager elementInstanceCollectionManager) {
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
        guideMap = new HashMap<>();
        for (Guide guide : guides) {
            guideMap.put(guide.getId(), guide);
        }
    }

    Collection<GuideAssignmentExpression> getAssignmentDependencies(String guideId, ExpressionItem expressionItem) {
        if (expressionItem instanceof BinaryExpression) {
            GuideBinaryExpressionItem guideBinaryExpressionItem =
                    new GuideBinaryExpressionItem(guideId, (BinaryExpression) expressionItem);
            return getDecisionMap().get(guideBinaryExpressionItem);
        }
        return null;
    }

    private Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> getDecisionMap() throws InternalErrorException {
        if (_decisionMap == null) {
            _decisionMap = generateDecisionMap();
        }
        return _decisionMap;
    }

    private Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> generateDecisionMap() throws InternalErrorException {
        Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> decisionMap = new HashMap<>();
        for (String elementId : getAllConditionsByElementIdMap().keySet()) {
            Collection<GuideBinaryExpressionItem> guideBinaryExpressionItems = getAllConditionsByElementIdMap().get(elementId);
            Collection<GuideAssignmentExpression> guideAssignmentExpressions = getAllAssignmentsByElementIdMap().get(elementId);
            if (guideAssignmentExpressions != null) {
                Calendar cal = Calendar.getInstance();
                for (GuideBinaryExpressionItem guideBinaryExpressionItem : guideBinaryExpressionItems) {
                    for (GuideAssignmentExpression guideAssignmentExpression : guideAssignmentExpressions) {
                        if (matches(guideBinaryExpressionItem, guideAssignmentExpression, cal)) {
                            Set<GuideAssignmentExpression> guideAssignmentExpressionsAux = decisionMap.computeIfAbsent(guideBinaryExpressionItem, k -> new HashSet<>());
                            guideAssignmentExpressionsAux.add(guideAssignmentExpression);
                        }
                    }
                }
            }
        }
        return decisionMap;
    }

    private boolean matches(GuideBinaryExpressionItem guideBinaryExpressionItem, GuideAssignmentExpression guideAssignmentExpression, Calendar cal) {
        String gtCode = ((Variable) guideBinaryExpressionItem.getBinaryExpression().getLeft()).getCode();
        RuleReference ruleReference = new RuleReference(guideBinaryExpressionItem.getGuideId(), gtCode);
        ElementInstance conditionElementInstance = getElementInstanceByRuleReferenceMap().get(ruleReference);
        if (conditionElementInstance == null) {
            throw new InternalErrorException(new Exception("Element for binary expression " + guideAssignmentExpression + " not found!"));
        }
        gtCode = guideAssignmentExpression.getAssignmentExpression().getVariable().getCode();
        ruleReference = new RuleReference(guideAssignmentExpression.getGuideId(), gtCode);
        ElementInstance assignmentElementInstance = getElementInstanceByRuleReferenceMap().get(ruleReference);
        if (assignmentElementInstance == null) {
            throw new InternalErrorException(new Exception("Element for assignment expression " + guideAssignmentExpression + " not found!"));
        }
        GeneratedArchetypeReference ar1 = (GeneratedArchetypeReference) conditionElementInstance.getArchetypeReference();
        ArchetypeReference ar2 = assignmentElementInstance.getArchetypeReference();
        if (!elementInstanceCollectionManager.matches(ar1, ar2, guideMap, cal)) {
            return false;
        }
        DataValue dv1 = null;
        DataValue dv2 = null;
        ExpressionItem rightExpressionItem = guideBinaryExpressionItem.getBinaryExpression().getRight();
        if (rightExpressionItem instanceof ConstantExpression) {
            ConstantExpression constantExpression = (ConstantExpression) rightExpressionItem;
            if (constantExpression instanceof StringConstant || !constantExpression.getValue().equals("null")) {
                dv1 = GuideUtil.getDataValue(constantExpression);
            }
        } else {
            return false;
        }
        ExpressionItem assignmentExpressionItem = guideAssignmentExpression.getAssignmentExpression().getAssignment();
        if (assignmentExpressionItem instanceof ConstantExpression) {
            ConstantExpression constantExpression = (ConstantExpression) assignmentExpressionItem;
            if (constantExpression instanceof StringConstant || !constantExpression.getValue().equals("null")) {
                dv2 = GuideUtil.getDataValue((ConstantExpression) assignmentExpressionItem);
            }
        } else {
            return false;
        }
        Collection<Guide> guides = Collections.singleton(guideMap.get(guideBinaryExpressionItem.getGuideId()));
        return !(dv1 != null
                && dv2 != null
                && !dv1.getClass().equals(dv2.getClass()))
                && elementInstanceCollectionManager.matches(dv1, dv2, guideBinaryExpressionItem.getBinaryExpression().getOperator(), guides);
    }

    private Map<RuleReference, ElementInstance> getElementInstanceByRuleReferenceMap() {
        if (allElementInstancesByRuleReference == null) {
            allElementInstancesByRuleReference = generateElementInstanceByRuleReferenceMap();
        }
        return allElementInstancesByRuleReference;
    }

    private Map<RuleReference, ElementInstance> generateElementInstanceByRuleReferenceMap() {
        Map<RuleReference, ElementInstance> allElementInstancesByRuleReference = new HashMap<>();
        for (Guide guide : guideMap.values()) {
            if (guide.getDefinition() != null) {
                for (ArchetypeBinding archetypeBinding : guide.getDefinition().getArchetypeBindings().values()) {
                    ArchetypeReference archetypeReference =
                            GuideUtil.getGeneratedArchetypeReference(archetypeBinding, guide.getId());
                    for (ElementInstance elementInstance : archetypeReference.getElementInstancesMap().values()) {
                        if (elementInstance instanceof GeneratedElementInstance) {
                            GeneratedElementInstance generatedElementInstance = (GeneratedElementInstance) elementInstance;
                            for (RuleReference ruleReference : generatedElementInstance.getRuleReferences()) {
                                allElementInstancesByRuleReference.put(ruleReference, elementInstance);
                            }
                        }
                    }
                }
            }
        }
        return allElementInstancesByRuleReference;
    }

    private Map<String, Collection<GuideBinaryExpressionItem>> getAllConditionsByElementIdMap() {
        if (allConditionsByElementIdMap == null) {
            allConditionsByElementIdMap = generateAllConditionsByElementIdMap();
        }
        return allConditionsByElementIdMap;
    }

    private Map<String, Collection<GuideBinaryExpressionItem>> generateAllConditionsByElementIdMap() {
        Map<String, Collection<GuideBinaryExpressionItem>> allConditionsByElementId = new HashMap<>();
        for (Guide guide : guideMap.values()) {
            if (guide.getDefinition() != null) {
                for (Rule rule : guide.getDefinition().getRules().values()) {
                    Collection<BinaryExpression> simpleConditionsFromExpressionItems =
                            getSimpleConditionsFromExpressionItems(guide, rule);
                    for (BinaryExpression binaryExpression : simpleConditionsFromExpressionItems) {
                        if (binaryExpression.getLeft() instanceof Variable) {
                            Variable variable = (Variable) binaryExpression.getLeft();
                            RuleReference ruleReference = new RuleReference(guide.getId(), variable.getCode());
                            ElementInstance elementInstance = getElementInstanceByRuleReferenceMap().get(ruleReference);
                            if (elementInstance == null) {
                                if (!OpenEHRConst.CURRENT_DATE_TIME_ID.equals(ruleReference.getGtCode())) {
                                    logger.warn("ElementInstance not found for " + ruleReference);
                                }
                            } else {
                                Collection<GuideBinaryExpressionItem> guideBinaryExpressionItems =
                                        allConditionsByElementId.computeIfAbsent(elementInstance.getId(), k -> new ArrayList<>());
                                guideBinaryExpressionItems.add(new GuideBinaryExpressionItem(guide.getId(), binaryExpression));
                            }
                        }
                    }
                }
            }
        }
        return allConditionsByElementId;
    }

    private Map<String, Collection<GuideAssignmentExpression>> getAllAssignmentsByElementIdMap() {
        if (allAssignmentsByElementIdMap == null) {
            allAssignmentsByElementIdMap = generateAllAssignmentsByElementIdMap();
        }
        return allAssignmentsByElementIdMap;
    }

    private Map<String, Collection<GuideAssignmentExpression>> generateAllAssignmentsByElementIdMap() {
        Map<String, Collection<GuideAssignmentExpression>> allAssignmentsByElementId = new HashMap<>();
        for (Guide guide : guideMap.values()) {
            if (guide.getDefinition() != null) {
                for (Rule rule : guide.getDefinition().getRules().values()) {
                    Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems =
                            getSimpleAssignmentsFromExpressionItems(rule.getThenStatements());
                    for (AssignmentExpression assignmentExpression : simpleAssignmentsFromExpressionItems) {
                        Variable variable = assignmentExpression.getVariable();
                        RuleReference ruleReference = new RuleReference(guide.getId(), variable.getCode());
                        ElementInstance elementInstance = getElementInstanceByRuleReferenceMap().get(ruleReference);
                        if (elementInstance == null) {
                            logger.warn("ElementInstance not found for " + ruleReference);
                        } else {
                            Collection<GuideAssignmentExpression> guideAssignmentExpression =
                                    allAssignmentsByElementId.computeIfAbsent(elementInstance.getId(), k -> new ArrayList<>());
                            guideAssignmentExpression.add(new GuideAssignmentExpression(guide.getId(), assignmentExpression));
                        }
                    }
                }
            }
        }
        return allAssignmentsByElementId;
    }

    private Collection<BinaryExpression> getSimpleConditionsFromExpressionItems(Guide guide, Rule rule) {
        Collection<BinaryExpression> simpleConditionsExpressionItems = new ArrayList<>();
        for (ExpressionItem expressionItem : guide.getDefinition().getPreConditionExpressions()) {
            addSimpleConditionsFromExpressionItems(expressionItem, simpleConditionsExpressionItems);
        }
        for (ExpressionItem expressionItem : rule.getWhenStatements()) {
            addSimpleConditionsFromExpressionItems(expressionItem, simpleConditionsExpressionItems);
        }
        for (ExpressionItem expressionItem : rule.getThenStatements()) {
            addSimpleConditionsFromExpressionItems(expressionItem, simpleConditionsExpressionItems);
        }
        return simpleConditionsExpressionItems;
    }

    private void addSimpleConditionsFromExpressionItems(ExpressionItem expressionItem, Collection<BinaryExpression> simpleConditionsExpressionItems) {
        if (expressionItem instanceof BinaryExpression) {
            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
            if (OperatorKind.AND.equals(binaryExpression.getOperator())
                    || OperatorKind.OR.equals(binaryExpression.getOperator())) {
                addSimpleConditionsFromExpressionItems(binaryExpression.getLeft(), simpleConditionsExpressionItems);
                addSimpleConditionsFromExpressionItems(binaryExpression.getRight(), simpleConditionsExpressionItems);
                return;
            } else {
                if (isArithmeticOperator(binaryExpression.getOperator())) {
                    addSimpleConditionsFromComplexExpressions(binaryExpression.getLeft(), simpleConditionsExpressionItems);
                    addSimpleConditionsFromComplexExpressions(binaryExpression.getRight(), simpleConditionsExpressionItems);
                }
                if (!(binaryExpression.getLeft() instanceof Variable) || !(binaryExpression.getRight() instanceof ConstantExpression)) {
                    return; //Skip conditions that are not like format 'variable op constant'
                }
            }
            simpleConditionsExpressionItems.add(binaryExpression);
        } else if (expressionItem instanceof AssignmentExpression) {
            AssignmentExpression assignmentExpression = (AssignmentExpression) expressionItem;
            addSimpleConditionsFromExpressionItems(assignmentExpression.getAssignment(), simpleConditionsExpressionItems);
        }
    }

    private void addSimpleConditionsFromComplexExpressions(ExpressionItem expressionItem, Collection<BinaryExpression> simpleConditionsExpressionItems) {
        if (expressionItem instanceof BinaryExpression) {
            addSimpleConditionsFromExpressionItems(expressionItem, simpleConditionsExpressionItems);
        } else if (expressionItem instanceof Variable) {
            Variable variable = (Variable) expressionItem;
            simpleConditionsExpressionItems.add(new BinaryExpression(variable, new ConstantExpression("null"), OperatorKind.INEQUAL));
        }
    }

    private boolean isArithmeticOperator(OperatorKind operator) {
        return OperatorKind.ADDITION.equals(operator)
                || OperatorKind.SUBSTRATION.equals(operator)
                || OperatorKind.MULTIPLICATION.equals(operator)
                || OperatorKind.DIVISION.equals(operator);
    }

    private Collection<AssignmentExpression> getSimpleAssignmentsFromExpressionItems(
            List<AssignmentExpression> assignmentExpressionItems) {
        Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems = new ArrayList<>();
        for (AssignmentExpression assignmentExpression : assignmentExpressionItems) {
            addSimpleAssignmentFromExpressionItems(assignmentExpression, simpleAssignmentsFromExpressionItems);
        }
        return simpleAssignmentsFromExpressionItems;
    }

    private void addSimpleAssignmentFromExpressionItems(
            AssignmentExpression assignmentExpression, Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems) {
        if (assignmentExpression instanceof CreateInstanceExpression) {
            CreateInstanceExpression createInstanceExpression = (CreateInstanceExpression) assignmentExpression;
            simpleAssignmentsFromExpressionItems.addAll(createInstanceExpression.getAssignment().getAssignmentExpressions());
        } else {
            simpleAssignmentsFromExpressionItems.add(assignmentExpression);
        }
    }

    private static class GuideBinaryExpressionItem {
        private String guideId;
        private BinaryExpression binaryExpression;

        private GuideBinaryExpressionItem(String guideId, BinaryExpression binaryExpression) {
            this.guideId = guideId;
            this.binaryExpression = binaryExpression;
        }

        String getGuideId() {
            return guideId;
        }

        BinaryExpression getBinaryExpression() {
            return binaryExpression;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!(obj instanceof GuideBinaryExpressionItem)) {
                return false;
            }
            GuideBinaryExpressionItem that = (GuideBinaryExpressionItem) obj;
            return binaryExpression.equals(that.binaryExpression) && guideId.equals(that.guideId);
        }

        @Override
        public int hashCode() {
            int result = guideId.hashCode();
            result = 31 * result + binaryExpression.hashCode();
            return result;
        }

        @Override
        public String toString() {
            return "GuideBinaryExpressionItem{"
                    + "guideId='" + guideId + '\''
                    + ", binaryExpression=" + binaryExpression
                    + "}";
        }
    }

    public static class GuideAssignmentExpression {
        private String guideId;
        private AssignmentExpression assignmentExpression;

        private GuideAssignmentExpression(String guideId, AssignmentExpression assignmentExpression) {
            this.guideId = guideId;
            this.assignmentExpression = assignmentExpression;
        }

        String getGuideId() {
            return guideId;
        }

        AssignmentExpression getAssignmentExpression() {
            return assignmentExpression;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!(obj instanceof GuideAssignmentExpression)) {
                return false;
            }
            GuideAssignmentExpression that = (GuideAssignmentExpression) obj;
            return assignmentExpression.equals(that.assignmentExpression) && guideId.equals(that.guideId);
        }

        @Override
        public int hashCode() {
            int result = guideId.hashCode();
            result = 31 * result + assignmentExpression.hashCode();
            return result;
        }

        @Override
        public String toString() {
            return "GuideAssignmentExpression{"
                    + "guideId='" + guideId + '\''
                    + ", assignmentExpression=" + assignmentExpression
                    + "}";
        }
    }
}

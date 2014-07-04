package se.cambio.cds.gdl.graph;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
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
import se.cambio.cds.util.ElementInstanceCollectionUtil;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.*;

/**
 * User: Iago.Corbal
 * Date: 2014-07-03
 * Time: 17:12
 */
public class GDLDecisionModelBuilder {

    private Map<String, Guide> _guideMap = null;
    private Map<RuleReference, ElementInstance> _allElementInstancesByRuleReference = null;
    private Map<String, Collection<GuideBinaryExpressionItem>> _allConditionsByElementIdMap;
    private Map<String, Collection<GuideAssignmentExpression>> _allAssignmentsByElementIdMap;
    private Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> _decisionMap;

    public GDLDecisionModelBuilder(Collection<Guide> guides){
        _guideMap = new HashMap<String, Guide>();
        for(Guide guide: guides){
            _guideMap.put(guide.getId(), guide);
        }
    }

    public Collection<GuideAssignmentExpression> getAssignmentDependencies(String guideId, ExpressionItem expressionItem) throws InternalErrorException {
        if (expressionItem instanceof BinaryExpression){
            GuideBinaryExpressionItem guideBinaryExpressionItem =
                    new GuideBinaryExpressionItem(guideId, (BinaryExpression)expressionItem);
            return getDecisionMap().get(guideBinaryExpressionItem);
        }
        return null;
    }

    private Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> getDecisionMap() throws InternalErrorException {
        if (_decisionMap==null){
            _decisionMap = generateDecisionMap();
        }
        return _decisionMap;
    }

    private Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> generateDecisionMap() throws InternalErrorException {
        Map<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>> decisionMap =
                new HashMap<GuideBinaryExpressionItem, Set<GuideAssignmentExpression>>();
        for(String elementId: getAllConditionsByElementIdMap().keySet()){
            Collection<GuideBinaryExpressionItem> guideBinaryExpressionItems = getAllConditionsByElementIdMap().get(elementId);
            Collection<GuideAssignmentExpression> guideAssignmentExpressions = getAllAssignmentsByElementIdMap().get(elementId);
            if (guideAssignmentExpressions!=null){
                Calendar cal = Calendar.getInstance();
                for(GuideBinaryExpressionItem guideBinaryExpressionItem: guideBinaryExpressionItems){
                    for(GuideAssignmentExpression guideAssignmentExpression: guideAssignmentExpressions){
                        if (match(guideBinaryExpressionItem, guideAssignmentExpression, cal)){
                            Set<GuideAssignmentExpression> guideAssignmentExpressionsAux = decisionMap.get(guideBinaryExpressionItem);
                            if (guideAssignmentExpressionsAux==null){
                                guideAssignmentExpressionsAux = new HashSet<GuideAssignmentExpression>();
                                decisionMap.put(guideBinaryExpressionItem, guideAssignmentExpressionsAux);
                            }
                            guideAssignmentExpressionsAux.add(guideAssignmentExpression);
                        }
                    }
                }
            }
        }
        return decisionMap;
    }

    private boolean match(GuideBinaryExpressionItem guideBinaryExpressionItem, GuideAssignmentExpression guideAssignmentExpression, Calendar cal) throws InternalErrorException {
        String gtCode = ((Variable)guideBinaryExpressionItem.getBinaryExpression().getLeft()).getCode();
        RuleReference ruleReference = new RuleReference(guideBinaryExpressionItem.getGuideId(), gtCode);
        ElementInstance conditionElementInstance = getElementInstanceByRuleReferenceMap().get(ruleReference);
        if (conditionElementInstance==null){
            throw new InternalErrorException(new Exception("Element for binary expression "+guideAssignmentExpression+" not found!"));
        }
        gtCode = guideAssignmentExpression.getAssignmentExpression().getVariable().getCode();
        ruleReference = new RuleReference(guideAssignmentExpression.getGuideId(), gtCode);
        ElementInstance assignmentElementInstance = getElementInstanceByRuleReferenceMap().get(ruleReference);
        if (assignmentElementInstance==null){
            throw new InternalErrorException(new Exception("Element for assignment expression "+guideAssignmentExpression+" not found!"));
        }
        GeneratedArchetypeReference ar1 = (GeneratedArchetypeReference)conditionElementInstance.getArchetypeReference();
        ArchetypeReference ar2 = assignmentElementInstance.getArchetypeReference();
        if (!ElementInstanceCollectionUtil.matches(ar1, ar2,_guideMap, cal)){
            return false;
        }
        DataValue dv1 = null;
        DataValue dv2 = null;
        if (guideBinaryExpressionItem.getBinaryExpression().getRight() instanceof ConstantExpression){
             dv1 = GuideUtil.getDataValue((ConstantExpression)guideBinaryExpressionItem.getBinaryExpression().getRight());
        }else{
            return false;
        }
        if (guideAssignmentExpression.getAssignmentExpression().getAssignment() instanceof ConstantExpression){
            dv2 = GuideUtil.getDataValue((ConstantExpression)guideAssignmentExpression.getAssignmentExpression().getAssignment());
        }else{
            return false;
        }
        if (!ElementInstanceCollectionUtil.matches(dv1,dv2,
                guideBinaryExpressionItem.getBinaryExpression().getOperator(),
                _guideMap.get(guideBinaryExpressionItem.getGuideId()))){
            return false;
        }
        return true;
    }

    private  Map<RuleReference, ElementInstance> getElementInstanceByRuleReferenceMap(){
        if (_allElementInstancesByRuleReference==null){
            _allElementInstancesByRuleReference = generateElementInstanceByRuleReferenceMap();
        }
        return _allElementInstancesByRuleReference;
    }

    private Map<RuleReference, ElementInstance> generateElementInstanceByRuleReferenceMap(){
        Map<RuleReference, ElementInstance> allElementInstancesByRuleReference = new HashMap<RuleReference, ElementInstance>();
        for(Guide guide: _guideMap.values()){
            if (guide.getDefinition()!=null){
                for(ArchetypeBinding archetypeBinding: guide.getDefinition().getArchetypeBindings().values()){
                    ArchetypeReference archetypeReference =
                            GuideUtil.getGeneratedArchetypeReference(archetypeBinding, guide.getId());
                    for(ElementInstance elementInstance: archetypeReference.getElementInstancesMap().values()){
                        if (elementInstance instanceof GeneratedElementInstance){
                            GeneratedElementInstance generatedElementInstance = (GeneratedElementInstance)elementInstance;
                            for(RuleReference ruleReference: generatedElementInstance.getRuleReferences()){
                                allElementInstancesByRuleReference.put(ruleReference, elementInstance);
                            }
                        }
                    }
                }
            }
        }
        return allElementInstancesByRuleReference;
    }

    private Map<String, Collection<GuideBinaryExpressionItem>> getAllConditionsByElementIdMap(){
        if (_allConditionsByElementIdMap==null){
            _allConditionsByElementIdMap = generateAllConditionsByElementIdMap();
        }
        return _allConditionsByElementIdMap;
    }

    private Map<String, Collection<GuideBinaryExpressionItem>> generateAllConditionsByElementIdMap(){
        Map<String, Collection<GuideBinaryExpressionItem>> allConditionsByElementId = new HashMap<String, Collection<GuideBinaryExpressionItem>>();
        for(Guide guide: _guideMap.values()){
            if (guide.getDefinition()!=null){
                if (guide.getDefinition()!=null){
                    for(Rule rule: guide.getDefinition().getRules().values()){
                        Collection<BinaryExpression> simpleConditionsFromExpressionItems =
                                getSimpleConditionsFromExpressionItems(rule.getWhenStatements());
                        for(BinaryExpression binaryExpression: simpleConditionsFromExpressionItems){
                            if (binaryExpression.getLeft() instanceof Variable){
                                Variable variable = (Variable)binaryExpression.getLeft();
                                RuleReference ruleReference = new RuleReference(guide.getId(), variable.getCode());
                                ElementInstance elementInstance= getElementInstanceByRuleReferenceMap().get(ruleReference);
                                if (elementInstance==null){
                                    Logger.getLogger(GDLDecisionModelBuilder.class).warn("ElementInstance not found for "+ruleReference);
                                }else{
                                    Collection<GuideBinaryExpressionItem> guideBinaryExpressionItems =
                                            allConditionsByElementId.get(elementInstance.getId());
                                    if (guideBinaryExpressionItems ==null){
                                        guideBinaryExpressionItems = new ArrayList<GuideBinaryExpressionItem>();
                                        allConditionsByElementId.put(elementInstance.getId(), guideBinaryExpressionItems);
                                    }
                                    guideBinaryExpressionItems.add(new GuideBinaryExpressionItem(guide.getId(), binaryExpression));
                                }
                            }
                        }
                    }
                }
            }
        }
        return allConditionsByElementId;
    }

    private Map<String, Collection<GuideAssignmentExpression>> getAllAssignmentsByElementIdMap(){
        if (_allAssignmentsByElementIdMap==null){
            _allAssignmentsByElementIdMap = generateAllAssignmentsByElementIdMap();
        }
        return _allAssignmentsByElementIdMap;
    }

    private Map<String, Collection<GuideAssignmentExpression>> generateAllAssignmentsByElementIdMap(){
        Map<String, Collection<GuideAssignmentExpression>> allAssignmentsByElementId = new HashMap<String, Collection<GuideAssignmentExpression>>();
        for(Guide guide: _guideMap.values()){
            if (guide.getDefinition()!=null){
                if (guide.getDefinition()!=null){
                    for(Rule rule: guide.getDefinition().getRules().values()){
                        Collection<AssignmentExpression> simpleAssingmentsFromExpressionItems =
                                getSimpleAssignmentsFromExpressionItems(rule.getThenStatements());
                        for(AssignmentExpression assignmentExpression: simpleAssingmentsFromExpressionItems){
                            Variable variable = assignmentExpression.getVariable();
                            RuleReference ruleReference = new RuleReference(guide.getId(), variable.getCode());
                            ElementInstance elementInstance= getElementInstanceByRuleReferenceMap().get(ruleReference);
                            if (elementInstance==null){
                                Logger.getLogger(GDLDecisionModelBuilder.class).warn("ElementInstance not found for "+ruleReference);
                            }else{
                                Collection<GuideAssignmentExpression> guideAssignmentExpression =
                                        allAssignmentsByElementId.get(elementInstance.getId());
                                if (guideAssignmentExpression ==null){
                                    guideAssignmentExpression = new ArrayList<GuideAssignmentExpression>();
                                    allAssignmentsByElementId.put(elementInstance.getId(), guideAssignmentExpression);
                                }
                                guideAssignmentExpression.add(new GuideAssignmentExpression(guide.getId(), assignmentExpression));
                            }
                        }
                    }
                }
            }
        }
        return allAssignmentsByElementId;
    }

    private Collection<BinaryExpression> getSimpleConditionsFromExpressionItems(Collection<ExpressionItem> expressionItems){
        Collection<BinaryExpression> simpleConditionsExpressionItems = new ArrayList<BinaryExpression>();
        for(ExpressionItem expressionItem: expressionItems){
            addSimpleConditionsFromExpressionItems(expressionItem, simpleConditionsExpressionItems);
        }
        return simpleConditionsExpressionItems;
    }

    private void addSimpleConditionsFromExpressionItems(ExpressionItem expressionItem, Collection<BinaryExpression> simpleConditionsExpressionItems){
        if (expressionItem instanceof BinaryExpression){
            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
            if (OperatorKind.AND.equals(binaryExpression.getOperator()) ||
                    OperatorKind.OR.equals(binaryExpression.getOperator())){
                addSimpleConditionsFromExpressionItems(binaryExpression.getLeft(), simpleConditionsExpressionItems);
                addSimpleConditionsFromExpressionItems(binaryExpression.getRight(), simpleConditionsExpressionItems);
                return;
            }else {
                if (!(binaryExpression.getLeft() instanceof Variable) || !(binaryExpression.getRight() instanceof ConstantExpression)){
                    return; //Skip conditions that are not like format 'variable op constant'
                }
            }
            simpleConditionsExpressionItems.add(binaryExpression);
        }else{
            //Skip non binary conditions?
        }
    }

    private Collection<AssignmentExpression> getSimpleAssignmentsFromExpressionItems(List<AssignmentExpression> assignmentExpressionItems){
        Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems = new ArrayList<AssignmentExpression>();
        for(AssignmentExpression assignmentExpression: assignmentExpressionItems){
            addSimpleAssignmentFromExpressionItems(assignmentExpression, simpleAssignmentsFromExpressionItems);
        }
        return simpleAssignmentsFromExpressionItems;
    }

    private void addSimpleAssignmentFromExpressionItems(AssignmentExpression assignmentExpression, Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems){
        if (assignmentExpression instanceof CreateInstanceExpression){
            CreateInstanceExpression createInstanceExpression = (CreateInstanceExpression) assignmentExpression;
            simpleAssignmentsFromExpressionItems.addAll(createInstanceExpression.getAssigment().getAssignmentExpressions());
        }else {
            simpleAssignmentsFromExpressionItems.add(assignmentExpression);
        }
    }

    private class GuideBinaryExpressionItem {
        private String guideId;
        private BinaryExpression binaryExpression;

        private GuideBinaryExpressionItem(String guideId, BinaryExpression binaryExpression) {
            this.guideId = guideId;
            this.binaryExpression = binaryExpression;
        }

        public String getGuideId() {
            return guideId;
        }

        public void setGuideId(String guideId) {
            this.guideId = guideId;
        }

        public BinaryExpression getBinaryExpression() {
            return binaryExpression;
        }

        public void setBinaryExpression(BinaryExpression binaryExpression) {
            this.binaryExpression = binaryExpression;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof GuideBinaryExpressionItem)) return false;

            GuideBinaryExpressionItem that = (GuideBinaryExpressionItem) o;

            if (!binaryExpression.equals(that.binaryExpression)) return false;
            if (!guideId.equals(that.guideId)) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = guideId.hashCode();
            result = 31 * result + binaryExpression.hashCode();
            return result;
        }

        @Override
        public String toString() {
            return "GuideBinaryExpressionItem{" +
                    "guideId='" + guideId + '\'' +
                    ", binaryExpression=" + binaryExpression +
                    '}';
        }
    }

    public class GuideAssignmentExpression {
        private String guideId;
        private AssignmentExpression assignmentExpression;

        private GuideAssignmentExpression(String guideId, AssignmentExpression assignmentExpression) {
            this.guideId = guideId;
            this.assignmentExpression = assignmentExpression;
        }

        public String getGuideId() {
            return guideId;
        }

        public void setGuideId(String guideId) {
            this.guideId = guideId;
        }

        public AssignmentExpression getAssignmentExpression() {
            return assignmentExpression;
        }

        public void setAssignmentExpression(AssignmentExpression assignmentExpression) {
            this.assignmentExpression = assignmentExpression;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof GuideAssignmentExpression)) return false;

            GuideAssignmentExpression that = (GuideAssignmentExpression) o;

            if (!assignmentExpression.equals(that.assignmentExpression)) return false;
            if (!guideId.equals(that.guideId)) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = guideId.hashCode();
            result = 31 * result + assignmentExpression.hashCode();
            return result;
        }

        @Override
        public String toString() {
            return "GuideAssignmentExpression{" +
                    "guideId='" + guideId + '\'' +
                    ", assignmentExpression=" + assignmentExpression +
                    '}';
        }
    }
}

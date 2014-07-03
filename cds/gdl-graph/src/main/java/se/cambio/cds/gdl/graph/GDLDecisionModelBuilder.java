package se.cambio.cds.gdl.graph;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
  * User: Iago.Corbal
  * Date: 2014-07-03
  * Time: 17:12
  */
 public class GDLDecisionModelBuilder {

     private Collection<Guide> _guides = null;
     private Map<RuleReference, ElementInstance> _allElementInstancesByRuleReference = null;
     private Map<String, Collection<GuideBinaryExpressionItem>> _allConditionsByElementIdMap;
     private Map<String, Collection<GuideAssignmentExpression>> _allAssignmentsByElementIdMap;

     public GDLDecisionModelBuilder(Collection<Guide> guides){
         _guides = guides;
     }

     private Map<String, Collection<String>> generateDecisionMap(){
         Map<String, Collection<String>> decisionMap = new HashMap<String, Collection<String>>();
         for(Guide guide:_guides){
             if (guide.getDefinition()!=null){
                 for(Rule rule: guide.getDefinition().getRules().values()){
                     for(ExpressionItem expressionItem: rule.getWhenStatements()){

                     }
                 }
             }
         }
         return decisionMap;
     }

     private  Map<RuleReference, ElementInstance> getElementInstanceByRuleReferenceMap(){
         if (_allElementInstancesByRuleReference==null){
             _allElementInstancesByRuleReference = generateElementInstanceByRuleReferenceMap();
         }
         return _allElementInstancesByRuleReference;
     }

     private Map<RuleReference, ElementInstance> generateElementInstanceByRuleReferenceMap(){
         Map<RuleReference, ElementInstance> allElementInstancesByRuleReference = new HashMap<RuleReference, ElementInstance>();
         for(Guide guide: _guides){
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
         for(Guide guide: _guides){
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
         for(Guide guide: _guides){
             if (guide.getDefinition()!=null){
                 if (guide.getDefinition()!=null){
                     for(Rule rule: guide.getDefinition().getRules().values()){
                         Collection<AssignmentExpression> simpleAssingmentsFromExpressionItems =
                                 getSimpleAssignmentsFromExpressionItems(rule.getWhenStatements());
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

    private Collection<AssignmentExpression> getSimpleAssignmentsFromExpressionItems(Collection<ExpressionItem> expressionItems){
        Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems = new ArrayList<AssignmentExpression>();
        for(ExpressionItem expressionItem: expressionItems){
            addSimpleAssignmentFromExpressionItems(expressionItem, simpleAssignmentsFromExpressionItems);
        }
        return simpleAssignmentsFromExpressionItems;
    }

    private void addSimpleAssignmentFromExpressionItems(ExpressionItem expressionItem, Collection<AssignmentExpression> simpleAssignmentsFromExpressionItems){
        if (expressionItem instanceof CreateInstanceExpression){
            CreateInstanceExpression createInstanceExpression = (CreateInstanceExpression) expressionItem;
            simpleAssignmentsFromExpressionItems.addAll(createInstanceExpression.getAssigment().getAssignmentExpressions());
            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
        }else if (expressionItem instanceof AssignmentExpression){
            simpleAssignmentsFromExpressionItems.add((AssignmentExpression)expressionItem);
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
     }

     private class GuideAssignmentExpression {
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
     }
 }

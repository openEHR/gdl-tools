package se.cambio.cds.gdl.graph;

import com.mxgraph.view.mxGraph;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.rule.lines.CreateInstanceActionRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.GTCodeDefiner;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.*;

/**
 * User: Iago.Corbal
 * Date: 2014-07-01
 * Time: 13:39
 */
public class GDLDecisionGraph {

    private Guide _guide;
    private Map<String, GTCodeDefiner> _gtCodeElementsMap;
    private Map<String, Collection<String>> _writeGTCodesRuleMap;

    public GDLDecisionGraph(Guide guide) throws InternalErrorException {
             if (guide==null){
                 throw new InternalErrorException(new Exception("null guideline!"));
             }
             _guide = guide;
         }

    private Map<String, GTCodeDefiner> getGTCodeElementsMap(){
             if (_gtCodeElementsMap==null){
                 _gtCodeElementsMap = GuideImporter.generateGTCodeElementMap(_guide);
             }
             return _gtCodeElementsMap;
         }

    private Map<String, Collection<String>> getWriteGTCodesRuleMap() throws InternalErrorException {
             if(_writeGTCodesRuleMap==null){
                 _writeGTCodesRuleMap = new HashMap<String, Collection<String>>();
                 if (_guide.getDefinition()!=null){
                     for (Rule rule: _guide.getDefinition().getRules().values()){
                         Collection<String> writeGTCodes = GuideUtil.getGTCodesInWrites(rule);
                         _writeGTCodesRuleMap.put(rule.getId(), writeGTCodes);
                     }
                 }
             }
             return _writeGTCodesRuleMap;
         }

    public JComponent getGraph(String lang) throws InternalErrorException {
             TermDefinition termDefinition = GuideImporter.getTermDefinition(_guide, lang);
             if (termDefinition==null){
                 throw new InternalErrorException(new Exception("No TermDefinition for language '"+lang+"' inside the guideline"));
             }
             //TODO We should not need to do this
             for(GTCodeDefiner gtCodeDefiner: getGTCodeElementsMap().values()){
                 gtCodeDefiner.getGTCodeRuleLineElement().getParentRuleLine().setTermDefinition(termDefinition);
             }
             // Construct Model and Graph
             mxGraph graph = GDLGraphUtil.createGraph();

             for(Rule rule: _guide.getDefinition().getRules().values()){
                 createRuleNodes(graph, rule, termDefinition);
             }
             return  GDLGraphUtil.createGraphComponent(graph);
         }

    public void createRuleNodes(mxGraph graph, Rule rule, TermDefinition termDefinition)   throws InternalErrorException {
             Object ruleNode = GDLGraphUtil.createRuleNode(graph, rule, termDefinition);
             Collection<Object> currentNodes = Collections.singleton(ruleNode);
             for(ExpressionItem expressionItem: rule.getWhenStatements()){
                 Collection<Object> expressionNodes = addExpresionNodes(graph, rule.getId(), expressionItem, currentNodes, termDefinition);
                 currentNodes = expressionNodes;
             }
             Object endNode = GDLGraphUtil.createEndNode(graph);
             for(Object currentNode: currentNodes){
                 GDLGraphUtil.insertDirectionalEdge(graph, currentNode, endNode);
             }
             for(AssignmentExpression assignmentExpression: rule.getThenStatements()){
                 addAssignmentExpression(graph, assignmentExpression, endNode, termDefinition);
             }
         }

    public Collection<Object> addExpresionNodes(mxGraph graph, String ruleGTCode, ExpressionItem currentExpressionItem, Collection<Object> startNodes, TermDefinition termDefinition) throws InternalErrorException {
        if (currentExpressionItem instanceof BinaryExpression){
            BinaryExpression binaryExpression = (BinaryExpression) currentExpressionItem;
            if (OperatorKind.OR.equals(binaryExpression.getOperator())){
                Collection<Object> nodesLeft = addExpresionNodes(graph, ruleGTCode, binaryExpression.getLeft(), startNodes, termDefinition);
                Collection<Object> nodesRight = addExpresionNodes(graph, ruleGTCode, binaryExpression.getRight(), startNodes, termDefinition);
                Collection<Object> nodes = new ArrayList<Object>();
                nodes.addAll(nodesLeft);
                nodes.addAll(nodesRight);
                return nodes;
            }else if (OperatorKind.AND.equals(binaryExpression.getOperator())){
                Collection<Object> nodesLeft = addExpresionNodes(graph, ruleGTCode, binaryExpression.getLeft(), startNodes, termDefinition);
                Collection<Object> nodesRight = addExpresionNodes(graph, ruleGTCode, binaryExpression.getRight(), nodesLeft, termDefinition);
                return nodesRight;
            }
        }
        if (skipExpressionItemFromConditions(ruleGTCode, currentExpressionItem)){
            //Skip test
            return startNodes;
        }else{
            String label = getReadableExpression(currentExpressionItem, termDefinition);
            Object node = GDLGraphUtil.insertNode(graph, label, "#f8f668");
            for(Object startNode: startNodes){
                GDLGraphUtil.insertDirectionalEdge(graph, startNode, node);
            }
            return Collections.singleton(node);
        }
    }

    public boolean skipExpressionItemFromConditions(String ruleGTCode, ExpressionItem expressionItem) throws InternalErrorException {
        if (expressionItem instanceof BinaryExpression){
            BinaryExpression binaryExpression = (BinaryExpression)expressionItem;
            if (binaryExpression.getLeft() instanceof Variable){
                Variable variable = (Variable)binaryExpression.getLeft();
                Collection<String> writeGTCodes = getWriteGTCodesRuleMap().get(ruleGTCode);
                if (writeGTCodes.contains(variable.getCode())){
                    return true;
                }
            }
        }
        return false;
    }

    public void addAssignmentExpression(mxGraph graph, AssignmentExpression assignmentExpression, Object startNode, TermDefinition termDefinition) throws InternalErrorException {
        if (assignmentExpression instanceof CreateInstanceExpression){
            CreateInstanceExpression createInstanceExpression = (CreateInstanceExpression) assignmentExpression;
            String label = getReadableExpression(createInstanceExpression, termDefinition);
            Object createNode = GDLGraphUtil.insertNode(graph, label, "#b3b2e6");
            GDLGraphUtil.insertDirectionalEdge(graph, startNode, createNode);
            for(AssignmentExpression childAssignmentExpression :createInstanceExpression.getAssigment().getAssignmentExpressions()){
                addAssignmentExpression(graph, childAssignmentExpression, createNode, termDefinition);
            }
        }else{
            String label = getReadableExpression(assignmentExpression, termDefinition);
            Object node = GDLGraphUtil.insertNode(graph, label, "#7daeea");
            GDLGraphUtil.insertDirectionalEdge(graph, startNode, node);
        }
    }

    private String getReadableExpression(ExpressionItem expressionItem, TermDefinition termDefinition) throws InternalErrorException {
        Collection<RuleLine> ruleLines = new ArrayList<RuleLine>();
        GuideImporter.processExpressionItem(ruleLines, null, expressionItem, getGTCodeElementsMap());
        if (!ruleLines.isEmpty()){
            RuleLine ruleLine = ruleLines.iterator().next();
            ruleLine.setTermDefinition(termDefinition);
            if (ruleLine instanceof CreateInstanceActionRuleLine){
                return ((CreateInstanceActionRuleLine)ruleLine).toHTMLStringSingle(0,termDefinition.getId());
            }else{
                return ruleLine.toHTMLString(termDefinition.getId());

            }
        }else{
            return null;
        }
    }

}

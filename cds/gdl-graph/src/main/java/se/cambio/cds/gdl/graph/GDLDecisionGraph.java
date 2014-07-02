package se.cambio.cds.gdl.graph;

import com.jgraph.layout.JGraphFacade;
import com.jgraph.layout.hierarchical.JGraphHierarchicalLayout;
import org.jgraph.JGraph;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.GraphModel;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.expression.BinaryExpression;
import se.cambio.cds.gdl.model.expression.ExpressionItem;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.awt.*;
import java.util.*;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2014-07-01
 * Time: 13:39
 */
public class GDLDecisionGraph {

    private Guide _guide;

    public GDLDecisionGraph(Guide guide) throws InternalErrorException {
        if (guide==null){
            throw new InternalErrorException(new Exception("null guideline!"));
        }
        _guide = guide;
    }

    public JGraph getJGraph(String lang) throws InternalErrorException {
        TermDefinition termDefinition = GuideImporter.getTermDefinition(_guide, lang);
        if (termDefinition==null){
            throw new InternalErrorException(new Exception("No TermDefinition for language '"+lang+"' inside the guideline"));
        }
        // Construct Model and Graph
        GraphModel model = new DefaultGraphModel();
        JGraph graph = new JGraph(model);
        // Control-drag should clone selection
        //graph.setCloneable(true);
        // Enable edit without final RETURN keystroke
        graph.setInvokesStopCellEditing(true);
        // When over a cell, jump to its default port
        graph.setJumpToDefaultPort(true);
        Collection<DefaultGraphCell> cells = new ArrayList<DefaultGraphCell>();
        Set<String> elementGTCodesAdded = new HashSet<String>();
        for(Rule rule: _guide.getDefinition().getRules().values()){
            cells.addAll(createRuleNodes(rule, termDefinition));
        }
        if (!cells.isEmpty()){
            // Insert the cells via the cache, so they get selected
            graph.getGraphLayoutCache().insert(cells.toArray());
            final JGraphHierarchicalLayout gl = new JGraphHierarchicalLayout();
            gl.setFixRoots(true);
            gl.setLayoutFromSinks(true);
            final JGraphFacade graphFacade = new JGraphFacade(graph);
            gl.run(graphFacade);
            final Map nestedMap = graphFacade.createNestedMap(true, true);
            graph.getGraphLayoutCache().edit(nestedMap);
        }
        return graph;
    }

    public static Collection<DefaultGraphCell> createRuleNodes(Rule rule, TermDefinition termDefinition){
        List<DefaultGraphCell> graphCells = new ArrayList<DefaultGraphCell>();
        DefaultGraphCell startNode = createNode();
        graphCells.add(startNode);
        DefaultGraphCell endNode = createNode();
        for(ExpressionItem expressionItem: rule.getWhenStatements()){
            addGraphCells(expressionItem, startNode, endNode, graphCells);
            graphCells.add(endNode);
            startNode = endNode;
            endNode = createNode();
        }
        return graphCells;
    }

    public static void addGraphCells(ExpressionItem currentExpressionItem, DefaultGraphCell startNode, DefaultGraphCell endNode, List<DefaultGraphCell> graphCells){
        if (currentExpressionItem instanceof BinaryExpression){
            BinaryExpression binaryExpression = (BinaryExpression) currentExpressionItem;
            if (OperatorKind.OR.equals(binaryExpression.getOperator())){
                addGraphCells(binaryExpression.getLeft(), startNode, endNode, graphCells);
                addGraphCells(binaryExpression.getRight(), startNode, endNode, graphCells);
                return;
            }else if (OperatorKind.AND.equals(binaryExpression.getOperator())){
                DefaultGraphCell endNodeAux = createNode();
                addGraphCells(binaryExpression.getLeft(), startNode, endNodeAux, graphCells);
                startNode = endNodeAux;
                addGraphCells(binaryExpression.getRight(), startNode, endNode, graphCells);
                return;
            }
        }
        DefaultGraphCell edge = GDLGraphUtil.createDirectionalEdge(currentExpressionItem.toString(), startNode, endNode);
        graphCells.add(edge);
    }

    private static DefaultGraphCell createNode(){
        DefaultGraphCell node = GDLGraphUtil.createNode("  ", Color.YELLOW.brighter().brighter());
        return node;
    }

}

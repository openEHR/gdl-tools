package se.cambio.cds.gdl.graph;

import com.jgraph.layout.JGraphFacade;
import com.jgraph.layout.hierarchical.JGraphHierarchicalLayout;
import org.jgraph.JGraph;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.GraphModel;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.awt.*;
import java.util.*;

/**
 * User: Iago.Corbal
 * Date: 2014-07-01
 * Time: 13:39
 */
public class GDLDependencyGraph {

    private Guide _guide;

    public GDLDependencyGraph(Guide guide) throws InternalErrorException {
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
        Map<String, DefaultGraphCell> elementsNodeMap = new HashMap<String, DefaultGraphCell>();
        elementsNodeMap.put("currentDateTime", createSpecialNode("Current Date/Time", termDefinition));
        for(ArchetypeBinding archetypeBinding: _guide.getDefinition().getArchetypeBindings().values()){
            for(ElementBinding elementBinding: archetypeBinding.getElements().values()){
                DefaultGraphCell elementNode = createElementNode(elementBinding, termDefinition);
                elementsNodeMap.put(elementBinding.getId(), elementNode);
            }
        }
        Set<String> elementGTCodesAdded = new HashSet<String>();
        for(Rule rule: _guide.getDefinition().getRules().values()){
            DefaultGraphCell ruleNode = createRuleNode(rule, termDefinition);
            cells.add(ruleNode);
            Set<String> readGTCodes = GuideUtil.getGTCodesInReads(rule);
            Set<String> writeGTCodes = GuideUtil.getGTCodesInWrites(rule);
            readGTCodes.removeAll(writeGTCodes);//If write, ignore read (displays nicer graph)
            for(String readGTCode: readGTCodes){
                DefaultGraphCell elementNode = elementsNodeMap.get(readGTCode);
                if (elementNode==null){
                    throw new InternalErrorException(new Exception("Element not found for gtCode "+readGTCode));
                }
                if (!elementGTCodesAdded.contains(readGTCode)){
                    cells.add(elementNode);
                    elementGTCodesAdded.add(readGTCode);
                }
                DefaultEdge readEdge = GDLGraphUtil.createDirectionalEdge(elementNode, ruleNode);
                cells.add(readEdge);
            }


            for(String writeGTCode: writeGTCodes){
                DefaultGraphCell elementNode = elementsNodeMap.get(writeGTCode);
                if (!elementGTCodesAdded.contains(writeGTCode)){
                    cells.add(elementNode);
                    elementGTCodesAdded.add(writeGTCode);
                }
                DefaultEdge writeEdge = GDLGraphUtil.createDirectionalEdge(ruleNode, elementNode);
                cells.add(writeEdge);
            }
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

    public static DefaultGraphCell createRuleNode(Rule rule, TermDefinition termDefinition){
        String label = termDefinition.getTerms().get(rule.getId()).getText();
        DefaultGraphCell node = GDLGraphUtil.createNode(label, Color.GREEN);
        return node;
    }

    public static DefaultGraphCell createElementNode(ElementBinding elementBinding, TermDefinition termDefinition){
        String label = termDefinition.getTerms().get(elementBinding.getId()).getText();
        DefaultGraphCell node = GDLGraphUtil.createNode(label, Color.BLUE.brighter());
        return node;
    }

    public static DefaultGraphCell createSpecialNode(String label, TermDefinition termDefinition){
        DefaultGraphCell node = GDLGraphUtil.createNode(label, Color.GRAY);
        return node;
    }
}

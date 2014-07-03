package se.cambio.cds.gdl.graph;

import com.mxgraph.view.mxGraph;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.*;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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

    public JComponent getGraph(String lang) throws InternalErrorException {
        TermDefinition termDefinition = GuideImporter.getTermDefinition(_guide, lang);
        if (termDefinition==null){
            throw new InternalErrorException(new Exception("No TermDefinition for language '"+lang+"' inside the guideline"));
        }
        // Construct Model and Graph
        mxGraph graph = GDLGraphUtil.createGraph();
        Map<String, Object> elementsNodeMap = new HashMap<String, Object>();
        elementsNodeMap.put("currentDateTime", GDLGraphUtil.createSpecialNode(graph, "Current Date/Time", termDefinition));
        for(ArchetypeBinding archetypeBinding: _guide.getDefinition().getArchetypeBindings().values()){
            for(ElementBinding elementBinding: archetypeBinding.getElements().values()){
                Object elementNode = GDLGraphUtil.createElementNode(graph, elementBinding, termDefinition);
                elementsNodeMap.put(elementBinding.getId(), elementNode);
            }
        }
        Set<String> elementGTCodesAdded = new HashSet<String>();
        for(Rule rule: _guide.getDefinition().getRules().values()){
            Object ruleNode = GDLGraphUtil.createRuleNode(graph, rule, termDefinition);
            Set<String> readGTCodes = GuideUtil.getGTCodesInReads(rule);
            Set<String> writeGTCodes = GuideUtil.getGTCodesInWrites(rule);
            readGTCodes.removeAll(writeGTCodes);//If write, ignore read (displays nicer graph)
            for(String readGTCode: readGTCodes){
                Object elementNode = elementsNodeMap.get(readGTCode);
                if (elementNode==null){
                    throw new InternalErrorException(new Exception("Element not found for gtCode "+readGTCode));
                }
                if (!elementGTCodesAdded.contains(readGTCode)){
                    elementGTCodesAdded.add(readGTCode);
                }
                GDLGraphUtil.insertDirectionalEdge(graph, elementNode, ruleNode);
            }
            for(String writeGTCode: writeGTCodes){
                Object elementNode = elementsNodeMap.get(writeGTCode);
                if (!elementGTCodesAdded.contains(writeGTCode)){
                    elementGTCodesAdded.add(writeGTCode);
                }
                GDLGraphUtil.insertDirectionalEdge(graph, ruleNode, elementNode);
            }
        }
        return  GDLGraphUtil.createGraphComponent(graph);
    }
}

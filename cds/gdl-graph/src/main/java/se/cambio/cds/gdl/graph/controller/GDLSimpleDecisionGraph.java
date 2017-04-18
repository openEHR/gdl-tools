package se.cambio.cds.gdl.graph.controller;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.graph.controller.renderer.GraphRenderer;
import se.cambio.cds.gdl.graph.controller.renderer.GraphRendererFactory;
import se.cambio.cds.gdl.graph.controller.renderer.GraphRenderingException;
import se.cambio.cds.gdl.graph.model.*;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.*;

public class GDLSimpleDecisionGraph {

    private GraphGranularity graphGranularity;
    private Collection<Guide> guides;
    private Map<Guide, ReadableGuide> readableGuideMap;
    private Map<ExpressionItem, Collection<GraphNode>> _nodesMap;
    private Map<ExpressionItem, String> nodesToGuideIdMap;
    private ArchetypeManager archetypeManager;
    private Set<GraphNode> ruleNodesWithInputDependencies;
    private Set<GraphNode> ruleNodesWithOutputDependencies;
    private GraphRenderer graphRenderer = null;
    private Map<String, GraphGranularity> customGranularityMap;
    private NodeExploder nodeExploder;
    private Map<String, GraphNode> archetypeNodesMap;
    private Map<Guide, GraphNode> guideNodesMap;
    private Map<Rule, GraphNode> ruleNodesMap;
    private boolean viewArchetypeDependency = false;
    private static Logger logger = LoggerFactory.getLogger(GDLSimpleDecisionGraph.class);
    private ArchetypeReferencesManager archetypeReferencesManager;
    private ElementInstanceCollectionManager elementInstanceCollectionManager;


    public GDLSimpleDecisionGraph(
            Collection<Guide> guides,
            ArchetypeManager archetypeManager,
            GraphGranularity graphGranularity,
            NodeExploder nodeExploder,
            ArchetypeReferencesManager archetypeReferencesManager,
            ElementInstanceCollectionManager elementInstanceCollectionManager) throws GraphRenderingException {
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
        if (guides == null){
            throw new GraphRenderingException("Null guideline list!");
        }
        this.guides = guides;
        if (archetypeManager == null) {
            throw new RuntimeException("ArchetypeManager not set!");
        }
        this.archetypeManager = archetypeManager;
        if (graphGranularity == null) {
            throw new RuntimeException("Granularity not set!");
        }
        this.graphGranularity = graphGranularity;
        this.nodeExploder = nodeExploder;
    }

    public GraphGranularity getGraphGranularity() {
        if (graphGranularity == null) {
            graphGranularity = GraphGranularity.GUIDE;
        }
        return graphGranularity;
    }

    private boolean isViewArchetypeDependency() {
        return viewArchetypeDependency;
    }

    public void setViewArchetypeDependency(boolean viewArchetypeDependency) {
        this.viewArchetypeDependency = viewArchetypeDependency;
    }

    public void setGraphGranularity(GraphGranularity graphGranularity) {
        this.graphGranularity = graphGranularity;
        getCustomGranularityMap().clear();
    }

    public JComponent generateGraphComponent(String lang) throws InternalErrorException {
        init(lang);
        return  graphRenderer.generateGraphComponent();
    }

    public void generatePngGraphImage(File file, String lang) throws InternalErrorException {
        init(lang);
        BufferedImage img = graphRenderer.generateBufferedImage();
        try {
            ImageIO.write(img, "PNG", file);
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
    }

    private void init(String lang) throws InternalErrorException {
        this.ruleNodesWithInputDependencies = new HashSet<>();
        this.ruleNodesWithOutputDependencies = new HashSet<>();
        this.readableGuideMap = new HashMap<>();
        this.graphRenderer = GraphRendererFactory.getGraphRenderer();
        this.graphRenderer.setNodeExploder(this.nodeExploder);
        this.guideNodesMap = new HashMap<>();
        this.ruleNodesMap = new HashMap<>();
        getNodesMap().clear();
        getNodesToGuideIdMap().clear();
        processGuides(lang);
        addDecisionGraphDependencies();
        addStartEndNodes();
        if (isViewArchetypeDependency()) {
            addArchetypeDependencyNodes();
        }
    }

    private void processGuides(String lang) throws InternalErrorException {
        for (Guide guide: guides) {
            GuideImporter guideImporter = new GuideImporter(archetypeManager, archetypeReferencesManager);
            ReadableGuide readableGuide = guideImporter.importGuide(guide, lang);
            readableGuideMap.put(guide, readableGuide);
            if (readableGuide.getTermDefinition() == null){
                throw new GraphRenderingException("No TermDefinition for language '" + lang + "' inside the guideline");
            }
            processGuide(lang, guide);
        }
    }

    private void processGuide(String lang, Guide guide) throws InternalErrorException {
        GraphNode guideLineGraphNode = createGuideNode(guide);
        GraphGranularity localGranularity = getGranularityForNodeLabel(guide.getId());
        this.guideNodesMap.put(guide, guideLineGraphNode);
        if (GraphGranularity.GUIDE.equals(localGranularity)) {
            this.graphRenderer.insertGraphNode(guideLineGraphNode);
        }
        for (Rule rule : guide.getDefinition().getRules().values()) {
            GraphNode ruleGraphNode = createRuleNode(guide, rule, lang);
            this.ruleNodesMap.put(rule, ruleGraphNode);
            if (!GraphGranularity.GUIDE.equals(localGranularity)) {
                this.graphRenderer.insertGraphNode(ruleGraphNode);
            }
            GraphNode graphNode = getRuleLineLinkGraphNode(guideLineGraphNode, ruleGraphNode, localGranularity);
            processRuleLines(guide, rule, graphNode);
        }
    }

    private GraphNode getRuleLineLinkGraphNode(GraphNode guideLineGraphNode, GraphNode ruleGraphNode, GraphGranularity localGranularity) throws GraphRenderingException {
        GraphNode graphNode;
        if (!GraphGranularity.GUIDE.equals(localGranularity)) {
            graphNode = ruleGraphNode;
        } else {
            graphNode = guideLineGraphNode;
        }
        return graphNode;
    }

    private GraphNode createGuideNode(Guide guide) throws GraphRenderingException {
        Color color = new Color(153, 255, 153);
        return new GraphNodeBuilder()
                .setLabel(guide.getId())
                .setFillColor(color)
                .setShape(GraphNode.Shape.HEXAGON)
                .setTextAlignment(GraphNode.Alignment.CENTER)
                .createGraphNode();
    }


    private GraphNode createRuleNode(Guide guide, Rule rule, String lang)   throws InternalErrorException {
        GraphNode.Alignment textAlignment;
        ReadableGuide readableGuide = readableGuideMap.get(guide);
        String guideIdLabel = guide.getId() + "<br/>";
        String label = guideIdLabel + readableGuide.getTermDefinition().getTermText(rule.getId());
        GraphGranularity localGranularity = getGranularityForNodeLabel(label);
        if (GraphGranularity.RULELINE.equals(localGranularity)) {
            ReadableRule readableRule = readableGuide.getReadableRules().get(rule.getId());
            label = label + "<br/>" + getReadableRuleLines(lang, readableRule);
            textAlignment = GraphNode.Alignment.LEFT;
        } else {
            textAlignment = GraphNode.Alignment.CENTER;
        }
        Color color = new Color(185, 215, 255);
        return new GraphNodeBuilder()
                .setLabel(label)
                .setFillColor(color)
                .setShape(GraphNode.Shape.RECTANGLE)
                .setTextAlignment(textAlignment)
                .createGraphNode();
    }

    private String getReadableRuleLines(String lang, ReadableRule readableRule) {
        return StringUtils.substringAfter(readableRule.toHTMLString(lang), "<br>");
    }

    private void processRuleLines(Guide guide, Rule rule, GraphNode graphNode) throws InternalErrorException {
        for(ExpressionItem expressionItem : guide.getDefinition().getPreConditionExpressions()) {
            addExpressionNodes(guide.getId(), expressionItem, graphNode);
        }
        for(ExpressionItem expressionItem : rule.getWhenStatements()) {
            addExpressionNodes(guide.getId(), expressionItem, graphNode);
        }
        for(ExpressionItem expressionItem : rule.getThenStatements()) {
            addExpressionNodes(guide.getId(), expressionItem, graphNode);
        }
    }

    private void addExpressionNodes(String guideId, ExpressionItem currentExpressionItem, GraphNode graphNode) throws InternalErrorException {
        if (currentExpressionItem instanceof BinaryExpression) {
            BinaryExpression binaryExpression = (BinaryExpression) currentExpressionItem;
            if (OperatorKind.OR.equals(binaryExpression.getOperator()) ||
                    OperatorKind.AND.equals(binaryExpression.getOperator())) {
                addExpressionNodes(guideId, binaryExpression.getLeft(), graphNode);
                addExpressionNodes(guideId, binaryExpression.getRight(), graphNode);
                return;
            } else if (isArithmeticOperator(binaryExpression.getOperator())) {
                addSimpleConditionsFromComplexExpressions(guideId, binaryExpression.getLeft(), graphNode);
                addSimpleConditionsFromComplexExpressions(guideId, binaryExpression.getRight(), graphNode);
            }
        } else if (currentExpressionItem instanceof AssignmentExpression) {
            AssignmentExpression assignmentExpression = (AssignmentExpression) currentExpressionItem;
            addExpressionNodes(guideId, assignmentExpression.getAssignment(), graphNode);
        }
        addExpressionToNode(guideId, currentExpressionItem, graphNode);
    }

    private void addExpressionToNode(String guideId, ExpressionItem currentExpressionItem, GraphNode graphNode) {
        getNodes(currentExpressionItem).add(graphNode);
        getNodesToGuideIdMap().put(currentExpressionItem, guideId);
    }

    private void addSimpleConditionsFromComplexExpressions(String guideId, ExpressionItem expressionItem, GraphNode graphNode) throws InternalErrorException {
        if (expressionItem instanceof  BinaryExpression) {
            addExpressionNodes(guideId, expressionItem, graphNode);
        } else if (expressionItem instanceof Variable) {
            Variable variable = (Variable) expressionItem;
            BinaryExpression binaryExpression = new BinaryExpression(variable, new ConstantExpression("null"), OperatorKind.INEQUAL);
            addExpressionToNode(guideId, binaryExpression, graphNode);
        }
    }

    private boolean isArithmeticOperator(OperatorKind operator) {
        return OperatorKind.ADDITION.equals(operator) ||
                OperatorKind.SUBSTRATION.equals(operator) ||
                OperatorKind.MULTIPLICATION.equals(operator) ||
                OperatorKind.DIVISION.equals(operator);
    }

    private void addDecisionGraphDependencies() throws InternalErrorException {
        GDLDecisionModelBuilder gdlDecisionModelBuilder = new GDLDecisionModelBuilder(this.guides, elementInstanceCollectionManager);
        Collection<Map.Entry<GraphNode, GraphNode>> dependenciesIncluded = new ArrayList<>();
        for(ExpressionItem expressionItem: getNodesMap().keySet()){
            if (!(expressionItem instanceof AssignmentExpression)){
                String guideId = getNodesToGuideIdMap().get(expressionItem);
                if (guideId == null) {
                    logger.warn("Unknown guide id for expression '" + expressionItem.toString() + "'");
                }
                Collection<GDLDecisionModelBuilder.GuideAssignmentExpression> guideAssignmentExpressions =
                        gdlDecisionModelBuilder.getAssignmentDependencies(guideId, expressionItem);
                if (guideAssignmentExpressions != null && !guideAssignmentExpressions.isEmpty()){
                    for(GDLDecisionModelBuilder.GuideAssignmentExpression guideAssignmentExpression: guideAssignmentExpressions){
                        Collection<GraphNode> assignmentNodes =  getNodesMap().get(guideAssignmentExpression.getAssignmentExpression());
                        if (assignmentNodes != null) {
                            for(GraphNode assignmentNode: assignmentNodes) {
                                Collection<GraphNode> conditionNodes = getNodesMap().get(expressionItem);
                                for (GraphNode conditionNode : conditionNodes) {
                                    Map.Entry<GraphNode, GraphNode> entry = new HashMap.SimpleEntry<>(assignmentNode, conditionNode);
                                    if (!dependenciesIncluded.contains(entry) && !assignmentNode.equals(conditionNode)) {
                                        GraphEdge graphEdge = new GraphEdgeBuilder()
                                                .setGraphNodeA(assignmentNode)
                                                .setGraphNodeB(conditionNode)
                                                .setStyle(GraphEdge.Style.DASHED)
                                                .createGraphEdge();
                                        this.graphRenderer.insertGraphEdge(graphEdge);
                                        ruleNodesWithOutputDependencies.add(assignmentNode);
                                        ruleNodesWithInputDependencies.add(conditionNode);
                                        dependenciesIncluded.add(entry);
                                    }
                                }
                            }
                        } else {
                            logger.warn("Assignment node for '" + guideAssignmentExpression.getAssignmentExpression() + "' not found!");
                        }
                    }
                }
            }
        }
    }

    private void addStartEndNodes() throws GraphRenderingException {
        Color color = new Color(200, 200, 200);
        GraphNode startGraphNode = new GraphNodeBuilder()
                .setLabel("START")
                .setFillColor(color)
                .setShape(GraphNode.Shape.ELLIPSE)
                .setTextAlignment(GraphNode.Alignment.CENTER)
                .createGraphNode();
        this.graphRenderer.insertGraphNode(startGraphNode);
        GraphNode endGraphNode = new GraphNodeBuilder()
                .setLabel("END")
                .setFillColor(color)
                .setShape(GraphNode.Shape.ELLIPSE)
                .setTextAlignment(GraphNode.Alignment.CENTER)
                .createGraphNode();
        this.graphRenderer.insertGraphNode(endGraphNode);
        Collection<GraphNode> startNodesIncluded = new ArrayList<>();
        Collection<GraphNode> endNodesIncluded = new ArrayList<>();
        for (ExpressionItem expressionItem : getNodesMap().keySet()) {
            if (expressionItem instanceof AssignmentExpression) {
                for (GraphNode node : getNodesMap().get(expressionItem)) {
                    if (!this.ruleNodesWithOutputDependencies.contains(node)) {
                        if (!endNodesIncluded.contains(node)) {
                            GraphEdge graphEdge = new GraphEdgeBuilder()
                                    .setGraphNodeA(node)
                                    .setGraphNodeB(endGraphNode)
                                    .setStyle(GraphEdge.Style.DASHED)
                                    .createGraphEdge();
                            this.graphRenderer.insertGraphEdge(graphEdge);
                            endNodesIncluded.add(node);
                        }
                    }
                }
            } else {
                for (GraphNode node : getNodesMap().get(expressionItem)) {
                    if (!this.ruleNodesWithInputDependencies.contains(node)) {
                        if (!startNodesIncluded.contains(node)) {
                            GraphEdge graphEdge = new GraphEdgeBuilder()
                                    .setGraphNodeA(startGraphNode)
                                    .setGraphNodeB(node)
                                    .setStyle(GraphEdge.Style.DASHED)
                                    .createGraphEdge();
                            this.graphRenderer.insertGraphEdge(graphEdge);
                            startNodesIncluded.add(node);
                        }
                    }
                }
            }
        }
    }

    private void addArchetypeDependencyNodes() throws InternalErrorException {
        for (Guide guide : guides) {
            generateArchetypeNodes(guide);
        }
        Set<String> archetypeKeysAdded = new HashSet<>();
        for (Guide guide : guides) {
            addArchetypeDependencyNodes(guide, archetypeKeysAdded);
        }
    }

    private Map<String, GraphNode> getArchetypeNodesMap() {
        if (archetypeNodesMap == null) {
            archetypeNodesMap = new HashMap<>();
        }
        return archetypeNodesMap;
    }

    private void addArchetypeDependencyNodes(Guide guide, Set<String> archetypeKeysAdded) throws InternalErrorException {
        Map<String, String> gtCodeToArchetypeKey = generateGtCodeToArchetypeMap(guide);
        Collection<Map.Entry<GraphNode, GraphNode>> insertedEdges = new ArrayList<>();
        for(Rule rule: guide.getDefinition().getRules().values()){
            Set<String> readArchetypeKeys = getReadArchetypeKeys(guide, rule, gtCodeToArchetypeKey);
            Set<String> writeArchetypeKeys = getWriteArchetypeKeys(rule, gtCodeToArchetypeKey);
            readArchetypeKeys.removeAll(writeArchetypeKeys); //If write, ignore read (displays nicer graph)
            for(String archetypeKey: readArchetypeKeys){
                addArchetypeNode(guide, rule, insertedEdges, archetypeKeysAdded, archetypeKey, false);
            }
            for(String archetypeKey: writeArchetypeKeys){
                addArchetypeNode(guide, rule, insertedEdges, archetypeKeysAdded, archetypeKey, true);
            }
        }
    }

    private Set<String> getReadArchetypeKeys(Guide guide, Rule rule, Map<String, String> gtCodeToArchetypeKey) throws InternalErrorException {
        Set<String> archetypeKeys = new HashSet<>();
        Set<String> readGtCodes = GuideUtil.getGTCodesInReads(rule);
        readGtCodes.addAll(GuideUtil.getPreconditionGTCodesInReads(guide));
        return getArchetypeKeys(gtCodeToArchetypeKey, archetypeKeys, readGtCodes);
    }

    private Set<String> getWriteArchetypeKeys(Rule rule, Map<String, String> gtCodeToArchetypeKey) throws InternalErrorException {
        Set<String> archetypeKeys = new HashSet<>();
        Set<String> readGtCodes = GuideUtil.getGTCodesInWrites(rule);
        return getArchetypeKeys(gtCodeToArchetypeKey, archetypeKeys, readGtCodes);
    }

    private Set<String> getArchetypeKeys(Map<String, String> gtCodeToArchetypeKey, Set<String> archetypeKeys, Set<String> gtCodes) throws InternalErrorException {
        for (String gtCode: gtCodes) {
            if (OpenEHRConst.CURRENT_DATE_TIME_ID.equals(gtCode)) {
                continue;
            }
            String archetypeKey = gtCodeToArchetypeKey.get(gtCode);
            if (archetypeKey != null) {
                archetypeKeys.add(archetypeKey);
            }
        }
        return archetypeKeys;
    }

    private Map<String, String> generateGtCodeToArchetypeMap(Guide guide) {
        Map<String, String> gtCodeToArchetypeKey = new HashMap<>();
        for(ArchetypeBinding archetypeBinding: guide.getDefinition().getArchetypeBindings().values()) {
            String archetypeKey = getArchetypeKey(archetypeBinding);
            for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                gtCodeToArchetypeKey.put(elementBinding.getId(), archetypeKey);
            }
        }
        return gtCodeToArchetypeKey;
    }

    private String getArchetypeKey(ArchetypeBinding archetypeBinding) {
        return archetypeBinding.getDomain() + "|" + archetypeBinding.getArchetypeId() + archetypeBinding.getTemplateId();
    }

    private void generateArchetypeNodes(Guide guide) {
        for(ArchetypeBinding archetypeBinding: guide.getDefinition().getArchetypeBindings().values()){
            String archetypeKey = getArchetypeKey(archetypeBinding);
            String label = archetypeBinding.getDomain() + " | " + archetypeBinding.getArchetypeId();
            Color archetypeBindingColor = getArchetypeNodeColor(archetypeBinding);
            GraphNode archetypeNode = new GraphNodeBuilder()
                    .setLabel(label)
                    .setShape(GraphNode.Shape.RECTANGLE)
                    .setFillColor(archetypeBindingColor)
                    .createGraphNode();
            getArchetypeNodesMap().put(archetypeKey, archetypeNode);
        }
    }

    private Color getArchetypeNodeColor(ArchetypeBinding archetypeBinding) {
        String domain = archetypeBinding.getDomain();
        if (Domains.CDS_ID.equals(domain)) {
            return new Color(153, 153, 255);
        } else if (Domains.EHR_ID.equals(domain)) {
            return new Color(255, 255, 153);
        } else {
            return new Color(255, 153, 255);
        }
    }

    private void addArchetypeNode(Guide guide, Rule rule, Collection<Map.Entry<GraphNode, GraphNode>> insertedEdges, Set<String> archetypeKeysAdded, String archetypeKey, boolean isWriteAction) throws InternalErrorException {


        GraphNode archetypeNode = getArchetypeNodesMap().get(archetypeKey);
        if (archetypeNode == null) {
            throw new InternalErrorException(new Exception("Element not found for arcketypeKey " + archetypeKey));
        }
        if (!archetypeKeysAdded.contains(archetypeKey)) {
            graphRenderer.insertGraphNode(archetypeNode);
            archetypeKeysAdded.add(archetypeKey);
        }
        GraphNode ruleOrGuideNode = getRuleOrGuideNode(guide, rule);
        GraphNode graphNodeA = isWriteAction ? ruleOrGuideNode : archetypeNode;
        GraphNode graphNodeB = isWriteAction ? archetypeNode : ruleOrGuideNode;
        Map.Entry<GraphNode, GraphNode> edgeEntry = new AbstractMap.SimpleEntry<>(graphNodeA, graphNodeB);
        if (insertedEdges.contains(edgeEntry)) {
            return;
        }
        insertedEdges.add(edgeEntry);
        GraphEdge graphEdge = new GraphEdgeBuilder()
                .setGraphNodeA(graphNodeA)
                .setGraphNodeB(graphNodeB)
                .createGraphEdge();
        graphRenderer.insertGraphEdge(graphEdge);
    }

    private GraphNode getRuleOrGuideNode(Guide guide, Rule rule) {
        if (GraphGranularity.GUIDE.equals(getGraphGranularity())) {
            GraphGranularity localGranularity = getGranularityForNodeLabel(guide.getId());
            if (GraphGranularity.GUIDE.equals(localGranularity)) {
                return guideNodesMap.get(guide);
            } else {
                return ruleNodesMap.get(rule);
            }
        } else {
            return ruleNodesMap.get(rule);
        }
    }

    private Collection<GraphNode> getNodes(ExpressionItem expressionItem) {
        return getNodesMap().computeIfAbsent(expressionItem, k -> new ArrayList<>());
    }

    private Map<ExpressionItem, Collection<GraphNode>> getNodesMap(){
        if (_nodesMap == null){
            _nodesMap = new HashMap<>();
        }
        return _nodesMap;
    }

    private Map<ExpressionItem, String> getNodesToGuideIdMap() {
        if (nodesToGuideIdMap == null) {
            nodesToGuideIdMap = new HashMap<>();
        }
        return nodesToGuideIdMap;
    }

    private GraphGranularity getGranularityForNodeLabel(String nodeLabel){
        GraphGranularity localGranularity = getCustomGranularityMap().get(nodeLabel);
        if (localGranularity != null) {
            return localGranularity;
        } else {
            return this.getGraphGranularity();
        }
    }

    public Map<String, GraphGranularity> getCustomGranularityMap() {
        if (customGranularityMap == null) {
            customGranularityMap = new HashMap<>();
        }
        return customGranularityMap;
    }
}

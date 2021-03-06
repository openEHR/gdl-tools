package se.cambio.cds.controller.guide;

import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.basic.DvBoolean;
import org.openehr.rm.datatypes.quantity.DvCount;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.openehr.rm.support.terminology.TerminologyService;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.parser.DADLSerializer;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.execution.vo.*;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.CurrentTimeExpressionDataValue;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.GeneratedElementInstanceCollection;

import java.io.InputStream;
import java.util.*;

public class GuideUtil {

    public GuideUtil() {
    }

    public static final DvCodedText NULL_FLAVOUR_CODE_NO_INFO = new DvCodedText(
            "no information", new CodePhrase(TerminologyService.OPENEHR, "271"));

    public static void fillElementInstanceCollection(
            Guide guide,
            GeneratedElementInstanceCollection elementInstanceCollection) {
        Collection<ArchetypeReference> archetypeReferences = getArchetypeReferences(guide, null, false);
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            elementInstanceCollection.add(archetypeReference);
        }
    }

    public static Collection<ArchetypeReference> getArchetypeReferences(Guide guide, DateTime dateTime, boolean resolvePredicates) {
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<>();
        Map<String, ArchetypeBinding> abs = guide.getDefinition().getArchetypeBindings();
        if (abs != null) {
            for (ArchetypeBinding archetypeBinding : abs.values()) {
                ArchetypeReference ar = getGeneratedArchetypeReference(archetypeBinding, guide.getId(), guide, dateTime, resolvePredicates);
                archetypeReferences.add(ar);
            }
        }
        return archetypeReferences;
    }

    public static GeneratedArchetypeReference getGeneratedArchetypeReference(ArchetypeBinding archetypeBinding, String guideId) {
        return getGeneratedArchetypeReference(archetypeBinding, guideId, null, null, false);
    }

    private static GeneratedArchetypeReference getGeneratedArchetypeReference(
            ArchetypeBinding archetypeBinding, String guideId, Guide guide, DateTime dateTime, boolean resolvePredicates) {
        GeneratedArchetypeReference ar =
                new GeneratedArchetypeReference(
                        archetypeBinding.getDomain(),
                        archetypeBinding.getArchetypeId(),
                        archetypeBinding.getTemplateId());
        if (archetypeBinding.getElements() != null) {
            for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                String idElement =
                        archetypeBinding.getArchetypeId() + elementBinding.getPath();
                GeneratedElementInstance gei = new GeneratedElementInstance(
                        idElement,
                        null,
                        ar,
                        null,
                        NULL_FLAVOUR_CODE_NO_INFO);
                gei.getRuleReferences().add(new RuleReference(guideId, elementBinding.getId()));
            }
        }
        generatePredicateElements(archetypeBinding, ar, guideId, guide, dateTime, resolvePredicates);
        return ar;
    }

    public static void generatePredicateElements(
            ArchetypeBinding archetypeBinding, ArchetypeReference ar, String guideId, Guide guide, DateTime dateTime, boolean resolvePredicates) {
        if (archetypeBinding.getPredicateStatements() != null) {
            for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
                if (expressionItem instanceof BinaryExpression) {
                    generatePredicateElementsForBinaryExpression(archetypeBinding, ar, guideId, guide, dateTime, resolvePredicates, (BinaryExpression) expressionItem);
                } else if (expressionItem instanceof UnaryExpression) {
                    generatePredicateElementsForUnaryExpression(archetypeBinding, ar, guide, dateTime, resolvePredicates, (UnaryExpression) expressionItem);
                }
            }
        }
    }

    private static void generatePredicateElementsForUnaryExpression(
            ArchetypeBinding archetypeBinding, ArchetypeReference ar, Guide guide, DateTime dateTime,
            boolean resolvePredicates, UnaryExpression expressionItem) {
        UnaryExpression ue = expressionItem;
        OperatorKind op = ue.getOperator();
        ExpressionItem operand = ue.getOperand();
        if (operand instanceof Variable) {
            String idElement =
                    archetypeBinding.getArchetypeId() + ((Variable) operand).getPath();
            DataValue dv = null;
            generateElementInstanceForPredicate(ar, op, idElement, dv, guide, dateTime, resolvePredicates);
            //TODO No rule references added (no gt codes)
        }
    }

    private static void generatePredicateElementsForBinaryExpression(
            ArchetypeBinding archetypeBinding, ArchetypeReference ar, String guideId, Guide guide,
            DateTime dateTime, boolean resolvePredicates, BinaryExpression expressionItem) {
        BinaryExpression be = expressionItem;
        ExpressionItem left = be.getLeft();
        ExpressionItem right = be.getRight();
        if (left instanceof Variable) {
            String path = ((Variable) left).getPath();
            if (right instanceof ConstantExpression) {
                String idElement =
                        archetypeBinding.getArchetypeId() + path;
                ConstantExpression ce = (ConstantExpression) right;
                DataValue dv = null;
                if (!"null".equals(ce.getValue())) {
                    dv = getDataValue(ce);
                }

                PredicateGeneratedElementInstance ei = generateElementInstanceForPredicate(ar, be.getOperator(), idElement, dv, guide, dateTime, resolvePredicates);
                String gtCode = getGTCodeForPredicate(archetypeBinding, path, dv);
                if (gtCode != null) {
                    ei.getRuleReferences().add(new RuleReference(guideId, gtCode));
                }
            } else if (right != null) {
                String attribute = path.substring(path.lastIndexOf("/value/") + 7, path.length());
                path = path.substring(0, path.length() - attribute.length() - 7);
                String idElement = archetypeBinding.getArchetypeId() + path;
                DataValue dv = new CurrentTimeExpressionDataValue(right, attribute);
                generateElementInstanceForPredicate(ar, be.getOperator(), idElement, dv, guide, dateTime, resolvePredicates);
                //TODO No rule references added (no gt codes)
            }
        }
    }

    private static String getGTCodeForPredicate(ArchetypeBinding archetypeBinding, String path, DataValue dv) {
        DvCodedText dvCodedText = null;
        if (dv instanceof DvCodedText) {
            dvCodedText = ((DvCodedText) dv);
        } else if (dv instanceof DvOrdinal) {
            dvCodedText = ((DvOrdinal) dv).getSymbol();
        }
        if (dvCodedText != null
                && "local".equals(dvCodedText.getTerminologyId())
                && dvCodedText.getCode().startsWith("gt")) {
            return dvCodedText.getCode();
        }
        if (archetypeBinding.getElements() != null) {
            for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                if (elementBinding.getPath().equals(path)) {
                    return elementBinding.getId();
                }
            }
        }
        return null;
    }

    private static PredicateGeneratedElementInstance generateElementInstanceForPredicate(
            ArchetypeReference ar, OperatorKind op, String idElement, DataValue dv,
            Guide guide, DateTime dateTime, boolean resolvePredicates) {
        Collection<RuleReference> previousRuleReferences = new ArrayList<>();
        ElementInstance elementInstance = ar.getElementInstancesMap().get(idElement);
        if (elementInstance instanceof GeneratedElementInstance) {
            GeneratedElementInstance generatedElementInstance = (GeneratedElementInstance) elementInstance;
            previousRuleReferences.addAll(generatedElementInstance.getRuleReferences());
        }
        if (dv != null && guide != null && dateTime != null && resolvePredicates) {
            dv = ElementInstanceCollectionManager.resolvePredicate(dv, op, Collections.singleton(guide), dateTime.toCalendar(Locale.getDefault()));
        }
        PredicateGeneratedElementInstance predicateGeneratedElementInstance = new PredicateGeneratedElementInstanceBuilder()
                .setId(idElement)
                .setDataValue(dv)
                .setArchetypeReference(ar)
                .setOperatorKind(op)
                .createPredicateGeneratedElementInstance();
        predicateGeneratedElementInstance.getRuleReferences().addAll(previousRuleReferences);
        return predicateGeneratedElementInstance;
    }


    public static DataValue getDataValue(ConstantExpression constantExpression) {
        if (constantExpression instanceof CodedTextConstant) {
            return ((CodedTextConstant) constantExpression).getCodedText();
        } else if (constantExpression instanceof QuantityConstant) {
            return ((QuantityConstant) constantExpression).getQuantity();
        } else if (constantExpression instanceof StringConstant) {
            return new DvText(((StringConstant) constantExpression).getString());
        } else if (constantExpression instanceof OrdinalConstant) {
            return ((OrdinalConstant) constantExpression).getOrdinal();
        } else if (constantExpression instanceof DateTimeConstant) {
            return new DvDateTime(constantExpression.getValue());
        } else if ("true".equals(constantExpression.getValue()) || "false".equals(constantExpression.getValue())) {
            return new DvBoolean(constantExpression.getValue());
        } else if (constantExpression.getValue().startsWith("openehr::")) {
            return DataValue.parseValue("DV_CODED_TEXT," + constantExpression.getValue());
        } else if (isParsableInteger(constantExpression.getValue())) {
            int count = Integer.parseInt(constantExpression.getValue());
            return new DvCount(count);
        } else {
            LoggerFactory.getLogger(GuideUtil.class).warn("Unknown data value for constant expression '{}'", constantExpression);
            return null;
        }
    }

    private static boolean isParsableInteger(String value) {
        try {
            Integer.parseInt(value);
            return true;
        } catch (NumberFormatException ex) {
            return false;
        }
    }

    public static List<RuleReference> getRuleReferences(List<String> firedRules) {
        List<RuleReference> ruleReferences = new ArrayList<>();
        if (firedRules != null) {
            for (String firedRule : firedRules) {
                if (!firedRule.endsWith("/default")) {
                    ruleReferences.add(new RuleReference(firedRule));
                }
            }
        }
        return ruleReferences;
    }

    public static String serializeGuide(Guide guide) {
        StringBuilder sb = new StringBuilder();
        DADLSerializer serializer = new DADLSerializer();
        for (String line : serializer.toDADL(guide)) {
            sb.append(line).append("\n");
        }
        return sb.toString();
    }

    public static Guide parseGuide(InputStream input) {
        GDLParser parser = new GDLParser();
        return parser.parse(input);
    }

    public static Set<String> getGTCodesInReads(Guide guide) {
        Set<String> gtCodes = new HashSet<>();
        if (guide.getDefinition() == null || guide.getDefinition().getRules() == null) {
            return gtCodes;
        }
        addGtCodesForRules(guide, gtCodes);
        addGtCodesForPreconditions(guide, gtCodes);
        addGtCodesForDefaultActions(guide, gtCodes);
        return gtCodes;
    }

    public static Set<String> getGTCodesInReads(Rule rule) {
        Set<String> gtCodes = new HashSet<>();
        if (rule.getWhenStatements() != null) {
            for (ExpressionItem expressionItem : rule.getWhenStatements()) {
                addGTCodesInReads(expressionItem, gtCodes);
            }
        }
        if (rule.getThenStatements() != null) {
            for (ExpressionItem expressionItem : rule.getThenStatements()) {
                addGTCodesInReads(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }

    private static void addGtCodesForDefaultActions(Guide guide, Set<String> gtCodes) {
        gtCodes.addAll(getDefaultActionGTCodesInReads(guide));
    }

    private static void addGtCodesForPreconditions(Guide guide, Set<String> gtCodes) {
        gtCodes.addAll(getPreconditionGTCodesInReads(guide));
    }

    private static void addGtCodesForRules(Guide guide, Set<String> gtCodes) {
        for (Rule rule : guide.getDefinition().getRules().values()) {
            gtCodes.addAll(getGTCodesInReads(rule));
        }
    }

    public static Set<String> getPreconditionGTCodesInReads(Guide guide) {
        Set<String> gtCodes = new HashSet<>();
        List<ExpressionItem> preConditionExpressions = guide.getDefinition().getPreConditionExpressions();
        if (preConditionExpressions != null) {
            for (ExpressionItem expressionItem : preConditionExpressions) {
                addGTCodesInReads(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }


    private static Collection<String> getDefaultActionGTCodesInReads(Guide guide) {
        Set<String> gtCodes = new HashSet<>();
        List<AssignmentExpression> defaultActionExpressions = guide.getDefinition().getDefaultActionExpressions();
        if (defaultActionExpressions != null) {
            for (ExpressionItem expressionItem : defaultActionExpressions) {
                addGTCodesInReads(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }

    private static void addGTCodesInReads(ExpressionItem expressionItem, Set<String> gtCodes) {
        if (expressionItem instanceof BinaryExpression) {
            BinaryExpression binaryExpression = (BinaryExpression) expressionItem;
            addGTCodesInReads(binaryExpression.getLeft(), gtCodes);
            addGTCodesInReads(binaryExpression.getRight(), gtCodes);
        } else if (expressionItem instanceof UnaryExpression) {
            UnaryExpression unaryExpression = (UnaryExpression) expressionItem;
            addGTCodesInReads(unaryExpression.getOperand(), gtCodes);
        } else if (expressionItem instanceof FunctionalExpression) {
            FunctionalExpression functionalExpression = (FunctionalExpression) expressionItem;
            for (ExpressionItem expressionItemAux : functionalExpression.getItems()) {
                addGTCodesInReads(expressionItemAux, gtCodes);
            }
        } else if (expressionItem instanceof AssignmentExpression) {
            AssignmentExpression assignmentExpression = (AssignmentExpression) expressionItem;
            addGTCodesInReads(assignmentExpression.getAssignment(), gtCodes);
        } else if (expressionItem instanceof MultipleAssignmentExpression) {
            MultipleAssignmentExpression multipleAssignmentExpression = (MultipleAssignmentExpression) expressionItem;
            for (AssignmentExpression assignmentExpression : multipleAssignmentExpression.getAssignmentExpressions()) {
                addGTCodesInReads(assignmentExpression, gtCodes);
            }
        } else if (expressionItem instanceof Variable) {
            Variable variable = (Variable) expressionItem;
            gtCodes.add(variable.getCode());
        } else if (expressionItem instanceof ConstantExpression) {
            //Do nothing
        } else {
            throw new RuntimeException("Unkown expression '" + expressionItem.getClass().getName() + "'");
        }
    }

    public static Set<String> getGTCodesInWrites(Guide guide) {
        Set<String> gtCodes = new HashSet<>();
        if (guide.getDefinition() == null || guide.getDefinition().getRules() == null) {
            return gtCodes;
        }
        //Rules
        for (Rule rule : guide.getDefinition().getRules().values()) {
            gtCodes.addAll(getGTCodesInWrites(rule));
        }
        return gtCodes;
    }

    public static Set<String> getGTCodesInWrites(Rule rule) {
        Set<String> gtCodes = new HashSet<>();
        if (rule.getThenStatements() != null) {
            for (ExpressionItem expressionItem : rule.getThenStatements()) {
                addGTCodesInWrites(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }

    private static void addGTCodesInWrites(ExpressionItem expressionItem, Set<String> gtCodes) {
        if (expressionItem instanceof CreateInstanceExpression) {
            MultipleAssignmentExpression multipleAssignmentExpression = ((CreateInstanceExpression) expressionItem).getAssignment();
            for (AssignmentExpression assignmentExpression : multipleAssignmentExpression.getAssignmentExpressions()) {
                addGTCodesInWrites(assignmentExpression, gtCodes);
            }
        } else if (expressionItem instanceof AssignmentExpression) {
            gtCodes.add(((AssignmentExpression) expressionItem).getVariable().getCode());
        } else {
            throw new RuntimeException("Unknown expression '" + expressionItem.getClass().getName() + "'");
        }
    }

    public static Map<String, String> getGtCodeElementIdMap(Guide guide) {
        return getGtCodeElementIdMap(guide, null);
    }

    public static Map<String, String> getGtCodeElementIdMap(Guide guide, String domainId) {
        Map<String, String> gtCodeElementIdMap = new HashMap<>();
        if (guide.getDefinition() == null || guide.getDefinition().getArchetypeBindings() == null) {
            return gtCodeElementIdMap;
        }
        for (ArchetypeBinding archetypeBinding : guide.getDefinition().getArchetypeBindings().values()) {
            if (domainId == null || archetypeBinding.getDomain() == null || domainId.equals(archetypeBinding.getDomain())) {
                for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                    gtCodeElementIdMap.put(elementBinding.getId(), archetypeBinding.getArchetypeId() + elementBinding.getPath());
                }
            }
        }
        return gtCodeElementIdMap;
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
package se.cambio.cds.controller.guide;

import org.apache.log4j.Logger;
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
import se.cambio.cds.util.ElementInstanceCollectionUtil;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.*;

public class GuideUtil {

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

    private static GeneratedArchetypeReference getGeneratedArchetypeReference(ArchetypeBinding archetypeBinding, String guideId, Guide guide, DateTime dateTime, boolean resolvePredicates) {
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

    public static void generatePredicateElements(ArchetypeBinding archetypeBinding, ArchetypeReference ar, String guideId, Guide guide, DateTime dateTime, boolean resolvePredicates) {
        if (archetypeBinding.getPredicateStatements() != null) {
            for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
                if (expressionItem instanceof BinaryExpression) {
                    BinaryExpression be = ((BinaryExpression) expressionItem);
                    ExpressionItem l = be.getLeft();
                    ExpressionItem r = be.getRight();
                    if (l instanceof Variable) {
                        String path = ((Variable) l).getPath();
                        if (r instanceof ConstantExpression) {
                            String idElement =
                                    archetypeBinding.getArchetypeId() + path;
                            ConstantExpression ce = (ConstantExpression) r;
                            DataValue dv = null;
                            if (!"null".equals(ce.getValue())) {
                                dv = getDataValue(ce);
                            }

                            PredicateGeneratedElementInstance ei = generateElementInstanceForPredicate(ar, be.getOperator(), idElement, dv, guide, dateTime, resolvePredicates);
                            String gtCode = getGTCodeForPredicate(archetypeBinding, path, dv);
                            if (gtCode != null) {
                                ei.getRuleReferences().add(new RuleReference(guideId, gtCode));
                            }
                        } else if (r instanceof ExpressionItem) {
                            String attribute = path.substring(path.lastIndexOf("/value/") + 7, path.length());
                            path = path.substring(0, path.length() - attribute.length() - 7);
                            String idElement = archetypeBinding.getArchetypeId() + path;
                            DataValue dv = new CurrentTimeExpressionDataValue(r, attribute);
                            generateElementInstanceForPredicate(ar, be.getOperator(), idElement, dv, guide, dateTime, resolvePredicates);
                            //TODO No rule references added (no gt codes)
                        }
                    }
                } else if (expressionItem instanceof UnaryExpression) {
                    UnaryExpression ue = ((UnaryExpression) expressionItem);
                    OperatorKind op = ue.getOperator();
                    ExpressionItem o = ue.getOperand();
                    if (o instanceof Variable) {
                        String idElement =
                                archetypeBinding.getArchetypeId() + ((Variable) o).getPath();
                        DataValue dv = null;
                        generateElementInstanceForPredicate(ar, op, idElement, dv, guide, dateTime, resolvePredicates);
                        //TODO No rule references added (no gt codes)
                    }
                }
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
        if (dvCodedText != null) {
            //TODO Will only work if the same code is used in predicate and definition
            if ("local".equals(dvCodedText.getTerminologyId()) &&
                    dvCodedText.getCode().startsWith("gt")) {
                return dvCodedText.getCode();
            }
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

    private static PredicateGeneratedElementInstance generateElementInstanceForPredicate(ArchetypeReference ar, OperatorKind op, String idElement, DataValue dv, Guide guide, DateTime dateTime, boolean resolvePredicates) {
        Collection<RuleReference> previousRuleReferences = new ArrayList<RuleReference>();
        ElementInstance elementInstance = ar.getElementInstancesMap().get(idElement);
        if (elementInstance instanceof GeneratedElementInstance) {
            GeneratedElementInstance generatedElementInstance = (GeneratedElementInstance) elementInstance;
            previousRuleReferences.addAll(generatedElementInstance.getRuleReferences());
        }
        if (dv != null && guide != null && dateTime != null && resolvePredicates) {
            dv = ElementInstanceCollectionUtil.resolvePredicate(dv, op, Collections.singleton(guide), dateTime.toCalendar(Locale.getDefault()));
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


    public static DataValue getDataValue(ConstantExpression e) {
        if (e instanceof CodedTextConstant) {
            return ((CodedTextConstant) e).getCodedText();
        } else if (e instanceof QuantityConstant) {
            return ((QuantityConstant) e).getQuantity();
        } else if (e instanceof StringConstant) {
            return new DvText(((StringConstant) e).getString());
        } else if (e instanceof OrdinalConstant) {
            return ((OrdinalConstant) e).getOrdinal();
        } else if (e instanceof DateTimeConstant) {
            return new DvDateTime(e.getValue());
            //TODO Use proper BooleanConstant (create) object
        } else if ("true".equals(e.getValue()) || "false".equals(e.getValue())) {
            return new DvBoolean(e.getValue());
        } else if (isParsableInteger(e.getValue())) {
            int count = Integer.parseInt(e.getValue());
            return new DvCount(count);
        } else {
            Logger.getLogger(GuideUtil.class).warn("Unknown data value for constant expression '" + e + "'");
            return null; //TODO Proportion, date, time, count, etc
        }
    }

    private static boolean isParsableInteger(String value) {
        try {
            Integer.parseInt(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static List<RuleReference> getRuleReferences(List<String> firedRules) {
        List<RuleReference> ruleReferences = new ArrayList<RuleReference>();
        if (firedRules != null) {
            for (String firedRule : firedRules) {
                if (!firedRule.endsWith("/default")) {
                    ruleReferences.add(new RuleReference(firedRule));
                }
            }
        }
        return ruleReferences;
    }

    public static String serializeGuide(Guide guide) throws Exception {
        StringBuffer sb = new StringBuffer();
        DADLSerializer serializer = new DADLSerializer();
        for (String line : serializer.toDADL(guide)) {
            sb.append(line + "\n");
        }
        return sb.toString();
    }

    public static Guide parseGuide(InputStream input) throws Exception {
        GDLParser parser = new GDLParser();
        return parser.parse(input);
    }

    //Get all element paths in the guideline that contains a read statement
    public static Set<String> getGTCodesInReads(Guide guide) throws InternalErrorException {
        Set<String> gtCodes = new HashSet<String>();
        if (guide.getDefinition() == null || guide.getDefinition().getRules() == null) {
            return gtCodes;
        }
        //Rules
        for (Rule rule : guide.getDefinition().getRules().values()) {
            gtCodes.addAll(getGTCodesInReads(rule));
        }
        //Preconditions
        gtCodes.addAll(getPreconditionGTCodesInReads(guide));

        //Default actions
        gtCodes.addAll(getDefaultActionGTCodesInReads(guide));
        return gtCodes;
    }

    public static Set<String> getGTCodesInReads(Rule rule) throws InternalErrorException {
        Set<String> gtCodes = new HashSet<String>();
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

    public static Set<String> getPreconditionGTCodesInReads(Guide guide) throws InternalErrorException {
        Set<String> gtCodes = new HashSet<String>();
        List<ExpressionItem> preConditionExpressions = guide.getDefinition().getPreConditionExpressions();
        if (preConditionExpressions != null) {
            for (ExpressionItem expressionItem : preConditionExpressions) {
                addGTCodesInReads(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }


    private static Collection<String> getDefaultActionGTCodesInReads(Guide guide) throws InternalErrorException {
        Set<String> gtCodes = new HashSet<String>();
        List<AssignmentExpression> defaultActionExpressions = guide.getDefinition().getDefaultActionExpressions();
        if (defaultActionExpressions != null) {
            for (ExpressionItem expressionItem : defaultActionExpressions) {
                addGTCodesInReads(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }

    private static void addGTCodesInReads(ExpressionItem expressionItem, Set<String> gtCodes) throws InternalErrorException {
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
            throw new InternalErrorException(new Exception("Unkown expression '" + expressionItem.getClass().getName() + "'"));
        }
    }

    //Get all element paths in the guideline that contains a set/create statement
    public static Set<String> getGTCodesInWrites(Guide guide) throws InternalErrorException {
        Set<String> gtCodes = new HashSet<String>();
        if (guide.getDefinition() == null || guide.getDefinition().getRules() == null) {
            return gtCodes;
        }
        //Rules
        for (Rule rule : guide.getDefinition().getRules().values()) {
            gtCodes.addAll(getGTCodesInWrites(rule));
        }
        return gtCodes;
    }

    public static Set<String> getGTCodesInWrites(Rule rule) throws InternalErrorException {
        Set<String> gtCodes = new HashSet<String>();
        if (rule.getThenStatements() != null) {
            for (ExpressionItem expressionItem : rule.getThenStatements()) {
                addGTCodesInWrites(expressionItem, gtCodes);
            }
        }
        return gtCodes;
    }

    private static void addGTCodesInWrites(ExpressionItem expressionItem, Set<String> gtCodes) throws InternalErrorException {
        if (expressionItem instanceof CreateInstanceExpression) {
            MultipleAssignmentExpression multipleAssignmentExpression = ((CreateInstanceExpression) expressionItem).getAssigment();
            for (AssignmentExpression assignmentExpression : multipleAssignmentExpression.getAssignmentExpressions()) {
                addGTCodesInWrites(assignmentExpression, gtCodes);
            }
        } else if (expressionItem instanceof AssignmentExpression) {
            gtCodes.add(((AssignmentExpression) expressionItem).getVariable().getCode());
        } else {
            throw new InternalErrorException(new Exception("Unknown expression '" + expressionItem.getClass().getName() + "'"));
        }
    }

    public static Map<String, String> getGtCodeElementIdMap(Guide guide) {
        return getGtCodeElementIdMap(guide, null);
    }

    public static Map<String, String> getGtCodeElementIdMap(Guide guide, String domainId) {
        Map<String, String> gtCodeElementIdMap = new HashMap<String, String>();
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
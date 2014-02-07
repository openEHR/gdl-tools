package se.cambio.cds.controller.guide;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.openehr.rm.support.terminology.TerminologyService;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.gdl.parser.DADLSerializer;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.CurrentTimeExpressionDataValue;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class GuideUtil {

    public static final DvCodedText NULL_FLAVOUR_CODE_NO_INFO = new DvCodedText(
            "no information", new CodePhrase(TerminologyService.OPENEHR, "271"));

    public static Guide parseGuide(
            byte[] guideSrc,
            GuideManager guideManager,
            GeneratedElementInstanceCollection elementInstanceCollection)
            throws InternalErrorException{
        try {
            Guide guide = parseGuide(new ByteArrayInputStream(guideSrc));
            fillElementInstanceCollection(guide, elementInstanceCollection);
            return guide;
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    public static void fillElementInstanceCollection(
            Guide guide,
            GeneratedElementInstanceCollection elementInstanceCollection){
        List<ArchetypeBinding> abs = guide.getDefinition().getArchetypeBindings();
        if (abs!=null){
            for (ArchetypeBinding archetypeBinding: abs) {
                ArchetypeReference ar = getGeneratedArchetypeReference(archetypeBinding,  guide.getId());
                elementInstanceCollection.add(ar);
            }
        }
    }

    public static GeneratedArchetypeReference getGeneratedArchetypeReference(ArchetypeBinding archetypeBinding, String guideId){
        GeneratedArchetypeReference ar =
                new GeneratedArchetypeReference(
                        archetypeBinding.getDomain(),
                        archetypeBinding.getArchetypeId(),
                        archetypeBinding.getTemplateId());
        if (archetypeBinding.getElements()!=null){
            for (ElementBinding elementBinding : archetypeBinding.getElements().values()) {
                String idElement =
                        archetypeBinding.getArchetypeId()+elementBinding.getPath();
                new GeneratedElementInstance(
                        idElement,
                        null,
                        ar,
                        null,
                        NULL_FLAVOUR_CODE_NO_INFO,
                        guideId,
                        elementBinding.getId());
            }
        }
        generatePredicateElements(archetypeBinding, ar, guideId);
        return ar;
    }
    public static void generatePredicateElements(ArchetypeBinding archetypeBinding, ArchetypeReference ar, String guideId){
        if (archetypeBinding.getPredicateStatements()!=null){
            for (ExpressionItem expressionItem : archetypeBinding.getPredicateStatements()) {
                if (expressionItem instanceof BinaryExpression){
                    BinaryExpression be = ((BinaryExpression)expressionItem);
                    ExpressionItem l = be.getLeft();
                    ExpressionItem r = be.getRight();
                    if (l instanceof Variable){
                        if (r instanceof ConstantExpression){
                            String idElement =
                                    archetypeBinding.getArchetypeId()+((Variable)l).getPath();
                            DataValue dv = getDataValue((ConstantExpression)r);
                            new PredicateGeneratedElementInstance(
                                    idElement,
                                    dv,
                                    ar,
                                    null,
                                    null,
                                    guideId,
                                    null,
                                    be.getOperator());
                        }else if (r instanceof ExpressionItem){
                            String path = ((Variable)l).getPath();
                            String attribute = path.substring(path.lastIndexOf("/value/")+7, path.length());
                            path = path.substring(0, path.length()-attribute.length()-7);
                            String idElement =
                                    archetypeBinding.getArchetypeId()+path;
                            DataValue dv = new CurrentTimeExpressionDataValue(r, attribute);
                            new PredicateGeneratedElementInstance(
                                    idElement,
                                    dv,
                                    ar,
                                    null,
                                    null,
                                    guideId,
                                    null,
                                    be.getOperator());
                        }
                    }
                }else if (expressionItem instanceof UnaryExpression){
                    UnaryExpression ue = ((UnaryExpression)expressionItem);
                    OperatorKind op = ue.getOperator();
                    ExpressionItem o = ue.getOperand();
                    if (o instanceof Variable){
                        String idElement =
                                archetypeBinding.getArchetypeId()+((Variable)o).getPath();
                        DataValue dv = null;
                        new PredicateGeneratedElementInstance(
                                idElement,
                                dv,
                                ar,
                                null,
                                null,
                                guideId,
                                null,
                                op);
                    }
                }
            }
        }
    }


    private static DataValue getDataValue(ConstantExpression e){
        if (e instanceof CodedTextConstant){
            return ((CodedTextConstant)e).getCodedText();
        } else if (e instanceof QuantityConstant){
            return ((QuantityConstant)e).getQuantity();
        } else if (e instanceof StringConstant){
            return new DvText(((StringConstant)e).getString());
        } else if (e instanceof OrdinalConstant){
            return ((OrdinalConstant)e).getOrdinal();
        } else {
            return null; //TODO Proportion, dates, count, etc
        }
    }

    public static List<RuleReference> getRuleReferences(List<String> firedRules){
        List<RuleReference> ruleReferences = new ArrayList<RuleReference>();
        if (firedRules!=null){
            for (String firedRule : firedRules) {
                ruleReferences.add(new RuleReference(firedRule));
            }
        }
        return ruleReferences;
    }

    public static String serializeGuide(Guide guide) throws Exception{
        StringBuffer sb = new StringBuffer();
        DADLSerializer serializer = new DADLSerializer();
        for (String line : serializer.toDADL(guide)) {
            sb.append(line+"\n");
        }
        return sb.toString();
    }

    public static Guide parseGuide(InputStream input) throws Exception{
        GDLParser parser = new GDLParser();
        return parser.parse(input);
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
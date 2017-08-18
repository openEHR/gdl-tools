package se.cambio.openehr.controller;

import lombok.extern.slf4j.Slf4j;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.constraintmodel.*;
import org.openehr.am.archetype.constraintmodel.primitive.CInteger;
import org.openehr.am.archetype.constraintmodel.primitive.CPrimitive;
import org.openehr.am.archetype.ontology.ArchetypeOntology;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.archetype.ontology.OntologyDefinitions;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvOrdinal;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantity;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantityItem;
import org.openehr.am.openehrprofile.datatypes.quantity.Ordinal;
import org.openehr.am.openehrprofile.datatypes.text.CCodePhrase;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.archetype.vo.*;
import se.cambio.cm.model.util.OpenEHRRMUtil;
import se.cambio.cm.util.TerminologyNodeVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.*;

@Slf4j
public class GenericObjectBundleADLManager {
    private static final String SECTION_NAME = "name/value='";
    protected String templateId = null;
    private String language = null;
    private Archetype ar = null;
    private Collection<ArchetypeElementVO> archetypeElementVOs;
    private Collection<ClusterVO> clusterVOs;
    private Collection<CodedTextVO> codedTextVOs;
    private Collection<OrdinalVO> ordinalVOs;
    private Collection<UnitVO> unitVOs;
    private Collection<ProportionTypeVO> proportionTypeVOs;
    private final Map<String, Archetype> archetypeMap;
    private TerminologyService terminologyService;
    private UserConfigurationManager userConfigurationManager;

    public GenericObjectBundleADLManager(
            Archetype ar, Map<String, Archetype> archetypeMap,
            TerminologyService terminologyService, UserConfigurationManager userConfigurationManager) {
        this.ar = ar;
        this.archetypeMap = archetypeMap;
        this.terminologyService = terminologyService;
        this.userConfigurationManager = userConfigurationManager;
    }

    public GenericObjectBundleADLManager(
            Archetype ar, String templateId, Map<String, Archetype> archetypeMap,
            TerminologyService terminologyService, UserConfigurationManager userConfigurationManager) {
        this.ar = ar;
        this.templateId = templateId;
        this.archetypeMap = archetypeMap;
        this.terminologyService = terminologyService;
        this.userConfigurationManager = userConfigurationManager;
    }

    public ArchetypeObjectBundleCustomVO generateObjectBundleCustomVO() {
        init();
        setDefaultLanguage();
        loadArchetypeObjects();
        Collection<ArchetypeTermVO> archetypeTermVOs = generateArchetypeTerms();
        return new ArchetypeObjectBundleCustomVO(
                archetypeElementVOs,
                clusterVOs,
                codedTextVOs,
                ordinalVOs,
                unitVOs,
                proportionTypeVOs,
                archetypeTermVOs);
    }

    private void setDefaultLanguage() {
        language = userConfigurationManager.getLanguage();
        if (!ar.getOriginalLanguage().getCodeString().equals(language)
                && (ar.getTranslations() == null || !ar.getTranslations().containsKey(language))) {
            language = ar.getOriginalLanguage().getCodeString();
        }
    }

    private void init() {
        archetypeElementVOs = new ArrayList<>();
        clusterVOs = new ArrayList<>();
        codedTextVOs = new ArrayList<>();
        ordinalVOs = new ArrayList<>();
        unitVOs = new ArrayList<>();
        proportionTypeVOs = new ArrayList<>();
    }

    public void loadArchetypeObjects() {
        String archId = ar.getArchetypeId().getValue();
        String rmEntry = ar.getArchetypeId().rmEntity();
        processCObject(ar.getDefinition());
        Collection<ArchetypeElementVO> rmArchetypeElements = OpenEHRRMUtil.getRMElements(archId, templateId, rmEntry);
        for (ClusterVO clusterVO : clusterVOs) {
            if (OpenEHRConst.isEntry(clusterVO.getType()) && !clusterVO.getPath().equals("/")) {
                rmArchetypeElements.addAll(OpenEHRRMUtil.getRMElements(archId, templateId, clusterVO.getType(), clusterVO.getPath()));
            }
        }
        clusterVOs.addAll(OpenEHRRMUtil.getRMClusters(archId, templateId, rmEntry));
        archetypeElementVOs.addAll(rmArchetypeElements);
    }

    private void processCObject(CObject constrainedObj) {
        String path = constrainedObj.path();
        String archetypeId = ar.getArchetypeId().getValue();
        if (!OpenEHRConst.PARSABLE_OPENEHR_RM_NAMES.contains(constrainedObj.getRmTypeName())) {
            return;
        }
        if (constrainedObj instanceof CComplexObject) {
            processComplexObject(constrainedObj, path, archetypeId);
        } else if (constrainedObj instanceof ArchetypeSlot) {
            //Skip
        }
    }

    private void processComplexObject(CObject constrainedObj, String path, String archetypeId) {
        CComplexObject complexConstrainedObject = ((CComplexObject) constrainedObj);
        CAttribute att = complexConstrainedObject.getAttribute("value");
        Archetype localAOM = getLocalAOM(ar, path);
        String text = getText(localAOM, constrainedObj, language);
        String desc = getDescription(localAOM, constrainedObj, language);

        //TODO ????
        if ("@ internal @".equals(desc) || text == null || text.startsWith("*")) {
            int firstIndex = path.lastIndexOf("/") + 1;
            int finalIndex = path.lastIndexOf("[");
            if (finalIndex > firstIndex) {
                text = path.substring(firstIndex, finalIndex);
            }
        }
        CObject childCObject = getCChildCObject(att);
        String type = getType(constrainedObj, att);
        if (hasCardinalityZero(complexConstrainedObject)) {
            return;
        }
        if (OpenEHRDataValues.isDataValue(type)) {
            processDataValue(constrainedObj, path, archetypeId, localAOM, text, desc, type, childCObject);
        } else {
            processSectionsAndClusters(constrainedObj, path, archetypeId, text, desc, type);
        }

        //Recursive lookup
        for (CAttribute constrainedAttribute : complexConstrainedObject.getAttributes()) {
            for (CObject constrainedObjAux : constrainedAttribute.getChildren()) {
                processCObject(constrainedObjAux);
            }
        }
    }

    private String getType(CObject constrainedObj, CAttribute att) {
        String type;
        if (att != null) {
            if (att.getChildren() != null && !att.getChildren().isEmpty()) {
                type = att.getChildren().get(0).getRmTypeName();
            } else {
                type = constrainedObj.getRmTypeName();
            }
        } else {
            type = constrainedObj.getRmTypeName();
        }
        type = convertTypeIfQuantityOrOrdinal(type);
        return type;
    }

    private CObject getCChildCObject(CAttribute att) {
        CObject childCObject = null;
        if ((att != null) && (att.getChildren() != null) && !att.getChildren().isEmpty()) {
            childCObject = att.getChildren().get(0);
        }
        return childCObject;
    }

    private boolean hasCardinalityZero(CComplexObject complexConstrainedObject) {
        return (complexConstrainedObject.getOccurrences() != null)
                && (complexConstrainedObject.getOccurrences().getUpper() != null)
                && (complexConstrainedObject.getOccurrences().getUpper() <= 0);
    }

    private String convertTypeIfQuantityOrOrdinal(String type) {
        if (type != null) {
            if (type.equals("DvOrdinal")) {
                type = OpenEHRDataValues.DV_ORDINAL;
            } else if (type.equals("DvQuantity")) {
                type = OpenEHRDataValues.DV_QUANTITY;
            }
        }
        return type;
    }

    private void processSectionsAndClusters(CObject constrainedObj, String path, String archetypeId, String text, String desc, String type) {
        if (type.equals(OpenEHRConst.SECTION)) {
            String auxText = getSectionName(path);
            //If the user specifies a special name on the template designer
            if (auxText != null) {
                text = auxText;
            }
        }
        ClusterVO clusterVO = ClusterVO.builder()
                .name(text)
                .description(desc)
                .type(type)
                .idArchetype(archetypeId)
                .idTemplate(templateId)
                .path(path)
                .build();
        setCardinalities(clusterVO, constrainedObj);
        clusterVOs.add(clusterVO);
    }

    private void processDataValue(
            CObject constrainedObj, String path, String archetypeId,
            Archetype localAOM, String text, String desc,
            String type, CObject childCObject) {
        ArchetypeElementVO archetypeElementVO = ArchetypeElementVO.builder()
                .name(text)
                .description(desc)
                .type(type)
                .idArchetype(archetypeId)
                .idTemplate(templateId)
                .path(path)
                .build();
        setCardinalities(archetypeElementVO, constrainedObj);
        archetypeElementVOs.add(archetypeElementVO);
        if (OpenEHRDataValues.DV_CODED_TEXT.equals(type)) {
            if (codedTextVOs != null) {
                loadCodedTexts(archetypeId, localAOM, templateId, path, childCObject, language, codedTextVOs);
            }
        } else if (OpenEHRDataValues.DV_ORDINAL.equals(type)) {
            if (ordinalVOs != null) {
                loadOrdinals(archetypeId, localAOM, templateId, path, childCObject, language, ordinalVOs);
            }
        } else if (OpenEHRDataValues.DV_QUANTITY.equals(type)) {
            if (unitVOs != null) {
                loadUnits(templateId, archetypeElementVO.getId(), childCObject, unitVOs);
            }
        } else if (OpenEHRDataValues.DV_PROPORTION.equals(type)) {
            if (proportionTypeVOs != null) {
                loadProportionTypes(templateId, archetypeElementVO.getId(), childCObject, proportionTypeVOs);
            }
        }
    }

    private static void setCardinalities(PathableVO pathableVO, CObject constrainedObj) {
        pathableVO.setLowerCardinality(constrainedObj.getOccurrences().getLower());
        pathableVO.setUpperCardinality(constrainedObj.getOccurrences().getUpper());
    }

    private void loadCodedTexts(
            String archetypeId,
            Archetype ar,
            String templateId,
            String path,
            CObject childCObject,
            String language,
            Collection<CodedTextVO> codedTextVOs) {
        boolean codedListFound = false;
        if (childCObject instanceof CComplexObject) {
            List<CAttribute> atts = ((CComplexObject) childCObject).getAttributes();
            if (atts != null) {
                int index = 0;
                Iterator<CAttribute> atIt = atts.iterator();
                while (atIt.hasNext() && !codedListFound) {
                    CAttribute att2 = atIt.next();
                    List<CObject> childAtts = att2.getChildren();
                    Iterator<CObject> childattsIt = childAtts.iterator();
                    while (childattsIt.hasNext() && !codedListFound) {
                        CObject constrainedObj = childattsIt.next();
                        if (constrainedObj instanceof CCodePhrase) {
                            CCodePhrase consCodePhrase = ((CCodePhrase) constrainedObj);
                            if (consCodePhrase.getCodeList() != null) {
                                for (String codedStr : consCodePhrase.getCodeList()) {
                                    String text = codedStr;
                                    String desc = codedStr;
                                    String terminologyId = consCodePhrase.getTerminologyId().getValue();
                                    CodedTextVO codedText = CodedTextVO.builder()
                                            .name(text)
                                            .description(desc)
                                            .type(consCodePhrase.getRmTypeName())
                                            .idArchetype(archetypeId)
                                            .idTemplate(templateId)
                                            .path(path)
                                            .terminology(terminologyId)
                                            .code(codedStr)
                                            .build();
                                    if (terminologyId.equals(OpenEHRConst.LOCAL)) {
                                        codedText.setName(getText(ar, codedStr, language));
                                        codedText.setDescription(getDescription(ar, codedStr, language));
                                    } else {
                                        addSubclassCodedTexts(codedText, codedTextVOs);
                                    }
                                    codedTextVOs.add(codedText);
                                    if (!"local".equals(terminologyId) && index++ > 15) {
                                        //No need to load the whole terminology for external references
                                        return;
                                    }
                                }
                                ;
                                codedListFound = true;
                            }
                        }
                    }
                }
            }
        }
    }

    private void addSubclassCodedTexts(CodedTextVO codedTextVO, Collection<CodedTextVO> codedTextVOs) {
        if (!OpenEHRConst.LOCAL.equals(codedTextVO.getTerminology())
                && !"*".equals(codedTextVO.getCode())) {
            TerminologyNodeVO node = terminologyService.retrieveAllSubclasses(
                    new CodePhrase(codedTextVO.getTerminology(), codedTextVO.getCode()),
                    OpenEHRDataValuesUI.getLanguageCodePhrase());
            if (node == null) {
                log.warn("Terminology code not found '" + codedTextVO.getCode() + "::" + codedTextVO.getTerminology() + "'.");
                return;
            }
            DvCodedText ct = node.getValue();
            codedTextVO.setName(getValidCodedTextName(ct.getValue()));
            codedTextVO.setDescription(getValidCodedTextName(ct.getValue()));
            if (codedTextVOs.size() > 15) { //No need to load the whole terminology
                return;
            }
            addCodedTextVOs(node, codedTextVO, codedTextVOs);
        }
    }

    private static void addCodedTextVOs(TerminologyNodeVO root, CodedTextVO rootCodedTextVO, Collection<CodedTextVO> codedTextVOs) {
        if (codedTextVOs.size() > 15) { //No need to load the whole terminology
            return;
        }
        for (TerminologyNodeVO node : root.getChildren()) {
            DvCodedText ct = node.getValue();
            CodedTextVO codedTextVO = CodedTextVO.builder()
                    .name(getValidCodedTextName(ct.getValue()))
                    .description(getValidCodedTextName(ct.getValue()))
                    .type(OpenEHRDataValues.DV_CODED_TEXT)
                    .idArchetype(rootCodedTextVO.getIdArchetype())
                    .idTemplate(rootCodedTextVO.getIdTemplate())
                    .path(rootCodedTextVO.getPath())
                    .terminology(ct.getDefiningCode().getTerminologyId().getValue())
                    .code(ct.getDefiningCode().getCodeString())
                    .build();
            codedTextVOs.add(codedTextVO);
            addCodedTextVOs(node, codedTextVO, codedTextVOs);
        }
    }

    /* Remove all parenthesis to avoid parsing problems */
    private static String getValidCodedTextName(String string) {
        return string.replaceAll("\\(", "[").replaceAll("\\)", "\\]");
    }

    private static void loadOrdinals(
            String archetypeId,
            Archetype ar,
            String templateId,
            String path,
            CObject childCObject,
            String language,
            Collection<OrdinalVO> ordinalVOs) {
        if (childCObject instanceof CDvOrdinal) {
            CDvOrdinal consDvOrdinal = (CDvOrdinal) childCObject;
            if (consDvOrdinal.getList() != null) {
                for (Ordinal ordinal : consDvOrdinal.getList()) {
                    String codedStr = ordinal.getSymbol().getCodeString();
                    String text = codedStr;
                    String desc = codedStr;
                    if (ordinal.getSymbol().getTerminologyId().getValue().equals("local")) {
                        text = getText(ar, codedStr, language);
                        desc = getDescription(ar, codedStr, language);
                    } else {
                        log.error("Unknown terminology: '" + ordinal.getSymbol().getTerminologyId().getValue() + "', skipping...");
                    }
                    ordinalVOs.add(
                            OrdinalVO.builder()
                                    .name(text)
                                    .description(desc)
                                    .type(consDvOrdinal.getRmTypeName())
                                    .idArchetype(archetypeId)
                                    .idTemplate(templateId)
                                    .path(path)
                                    .value(ordinal.getValue())
                                    .terminology(ordinal.getSymbol().getTerminologyId().getValue())
                                    .code(ordinal.getSymbol().getCodeString())
                                    .build());
                }
                ;
            }
        }
    }

    private static void loadUnits(
            String templateId,
            String idElement,
            CObject childCObject,
            Collection<UnitVO> unitVOs) {
        if (childCObject instanceof CDvQuantity) {
            CDvQuantity consDvQuantity = (CDvQuantity) childCObject;
            if (consDvQuantity.getList() != null) {
                for (CDvQuantityItem consDvQuantityItem : consDvQuantity.getList()) {
                    unitVOs.add(UnitVO.builder()
                            .idTemplate(templateId)
                            .idElement(idElement)
                            .unit(consDvQuantityItem.getUnits())
                            .build());
                }
                ;
            }
        }
    }

    private static void loadProportionTypes(
            String templateId,
            String idElement,
            CObject childCObject,
            Collection<ProportionTypeVO> proportionTypeVOs) {
        if (childCObject instanceof CComplexObject) {
            CComplexObject complexConstrainedObject = (CComplexObject) childCObject;
            CAttribute constrainedAttribute = complexConstrainedObject.getAttribute("type");
            if (constrainedAttribute != null) {
                for (CObject constrainedObj : constrainedAttribute.getChildren()) {
                    if (constrainedObj instanceof CPrimitiveObject) {
                        CPrimitive consPrimitive = ((CPrimitiveObject) constrainedObj).getItem();
                        if (consPrimitive instanceof CInteger) {
                            CInteger consInteger = (CInteger) consPrimitive;
                            for (Integer proportionType : consInteger.getList()) {
                                proportionTypeVOs.add(
                                        ProportionTypeVO.builder()
                                                .idTemplate(templateId)
                                                .idElement(idElement)
                                                .type(proportionType)
                                                .build());
                            }
                        }
                    }
                }
                ;
            }
        }
    }

    private String getText(Archetype ar, CObject constrainedObj, String language) {
        String text = getText(ar, constrainedObj.getNodeId(), language);
        if (text != null) {
            return text;
        } else {
            Archetype archetype = archetypeMap.get(constrainedObj.getNodeId());
            if (archetype != null) {
                text = archetype.getConceptName(language);
            } else {
                text = constrainedObj.getNodeId();
            }
            return text;
        }
    }

    private static String getText(Archetype ar, String codeStr, String language) {
        ArchetypeTerm term = getArchetypeTerm(ar, codeStr, language);
        if (term != null) {
            return term.getText();
        } else {
            return null;
        }
    }

    private String getDescription(Archetype ar, CObject constrainedObj, String language) {
        String desc = getDescription(ar, constrainedObj.getNodeId(), language);
        if (desc != null) {
            return desc;
        } else {
            Archetype archetype = archetypeMap.get(constrainedObj.getNodeId());
            if (archetype != null) {
                String conceptCode = archetype.getConcept();
                ArchetypeTerm term = archetype.getOntology().termDefinition(language, conceptCode);
                desc = term.getDescription();
            } else {
                desc = constrainedObj.getNodeId();
            }
            return desc;
        }
    }

    private static String getDescription(Archetype ar, String codeStr, String language) {
        ArchetypeTerm term = getArchetypeTerm(ar, codeStr, language);
        if (term != null) {
            return term.getDescription();
        } else {
            return null;
        }
    }

    private static ArchetypeTerm getArchetypeTerm(Archetype ar, String codeStr, String language) {
        ArchetypeTerm term = ar.getOntology().termDefinition(language, codeStr);
        if (term == null) {
            term = ar.getOntology().termDefinition(ar.getOriginalLanguage().getCodeString(), codeStr);
        }
        return term;
    }

    public Archetype getLocalAOM(Archetype currentAOM, String path) {
        int index = path.lastIndexOf("[");
        while (index > 0) {
            String idArchetype = path.substring(index + 1, path.lastIndexOf("]"));
            if (idArchetype.contains(" ")) {
                idArchetype = idArchetype.split(" ")[0];
            }
            Archetype archetype = null;
            if (idArchetype.startsWith("openEHR")) { //TODO remove dependency on openEHR archetypes
                archetype = archetypeMap.get(idArchetype);
            }
            if (archetype != null) {
                return archetype;
            } else {
                path = path.substring(0, index);
                index = path.lastIndexOf("[");
            }
        }
        ;
        return currentAOM;
    }

    private static String getSectionName(String path) {
        int i1 = path.lastIndexOf(SECTION_NAME) + SECTION_NAME.length();
        int i2 = path.indexOf("'", i1);
        if ((i1 > 0) && (i2 > 0)) {
            return path.substring(i1, i2);
        } else {
            return null;
        }
    }

    public Collection<ArchetypeTermVO> generateArchetypeTerms() {
        Collection<ArchetypeTermVO> archetypeTermVOs = new ArrayList<>();
        ArchetypeOntology ao = ar.getOntology();
        List<OntologyDefinitions> ods = ao.getTermDefinitionsList();
        for (OntologyDefinitions od : ods) {
            String lang = od.getLanguage();
            List<ArchetypeTerm> archetypeTerms = od.getDefinitions();
            for (ArchetypeTerm archetypeTerm : archetypeTerms) {
                archetypeTermVOs.add(
                        ArchetypeTermVO.builder()
                                .archetypeId(ar.getArchetypeId().getValue())
                                .code(archetypeTerm.getCode())
                                .language(lang)
                                .text(archetypeTerm.getText())
                                .description(archetypeTerm.getDescription())
                                .build());
            }
        }
        return archetypeTermVOs;
    }
}

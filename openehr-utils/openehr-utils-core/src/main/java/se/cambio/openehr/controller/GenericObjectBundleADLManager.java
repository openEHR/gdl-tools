package se.cambio.openehr.controller;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.constraintmodel.*;
import org.openehr.am.archetype.constraintmodel.primitive.CInteger;
import org.openehr.am.archetype.constraintmodel.primitive.CPrimitive;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvOrdinal;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantity;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantityItem;
import org.openehr.am.openehrprofile.datatypes.quantity.Ordinal;
import org.openehr.am.openehrprofile.datatypes.text.CCodePhrase;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.slf4j.LoggerFactory;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.archetype.vo.*;
import se.cambio.cm.model.util.OpenEHRRMUtil;
import se.cambio.cm.util.TerminologyNodeVO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.*;

import java.util.*;

public class GenericObjectBundleADLManager {
    private final static String SECTION_NAME = "name/value='";
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
        return new ArchetypeObjectBundleCustomVO(
                archetypeElementVOs,
                clusterVOs,
                codedTextVOs,
                ordinalVOs,
                unitVOs,
                proportionTypeVOs);
    }

    private void setDefaultLanguage() {
        language = userConfigurationManager.getLanguage();
        if (!ar.getOriginalLanguage().getCodeString().equals(language) &&
                (ar.getTranslations() == null || !ar.getTranslations().containsKey(language))) {
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
        proccessCObject(ar.getDefinition());
        Collection<ArchetypeElementVO> rmArchetypeElements = OpenEHRRMUtil.getRMElements(archId, templateId, rmEntry);
        for (ClusterVO clusterVO : clusterVOs) {
            if (OpenEHRConst.isEntry(clusterVO.getRMType()) && !clusterVO.getPath().equals("/")) {
                rmArchetypeElements.addAll(OpenEHRRMUtil.getRMElements(archId, templateId, clusterVO.getRMType(), clusterVO.getPath()));
            }
        }
        clusterVOs.addAll(OpenEHRRMUtil.getRMClusters(archId, templateId));

        archetypeElementVOs.addAll(rmArchetypeElements);
    }

    private void proccessCObject(CObject cObject) {
        String path = cObject.path();
        String archetypeId = ar.getArchetypeId().getValue();
        if (!OpenEHRConst.PARSABLE_OPENEHR_RM_NAMES.contains(cObject.getRmTypeName())) {
            return;
        }
        if (cObject instanceof CComplexObject) {
            processComplexObject(cObject, path, archetypeId);
        } else if (cObject instanceof ArchetypeSlot) {
            //Skip
        }
    }

    private void processComplexObject(CObject cObject, String path, String archetypeId) {
        CComplexObject cComplexObject = ((CComplexObject) cObject);
        CAttribute att = cComplexObject.getAttribute("value");
        Archetype localAOM = getLocalAOM(ar, path);
        String text = getText(localAOM, cObject, language);
        String desc = getDescription(localAOM, cObject, language);

        //TODO ????
        if ("@ internal @".equals(desc) || text == null || text.startsWith("*")) {
            int firstIndex = path.lastIndexOf("/") + 1;
            int finalIndex = path.lastIndexOf("[");
            if (finalIndex > firstIndex) {
                text = path.substring(firstIndex, finalIndex);
            }
        }
        CObject childCObject = getCChildCObject(att);
        String type = getType(cObject, att);
        if (hasCardinalityZero(cComplexObject)) {
            return;
        }
        if (OpenEHRDataValues.isDataValue(type)) {
            processDataValue(cObject, path, archetypeId, localAOM, text, desc, type, childCObject);
        } else {
            processSectionsAndClusters(cObject, path, archetypeId, text, desc, type);
        }

        //Recursive lookup
        for (CAttribute cAttribute : cComplexObject.getAttributes()) {
            for (CObject cObjectAux : cAttribute.getChildren()) {
                proccessCObject(cObjectAux);
            }
        }
    }

    private String getType(CObject cObject, CAttribute att) {
        String type;
        if (att != null) {
            if (att.getChildren() != null && !att.getChildren().isEmpty()) {
                type = att.getChildren().get(0).getRmTypeName();
            } else {
                type = cObject.getRmTypeName();
            }
        } else {
            type = cObject.getRmTypeName();
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

    private boolean hasCardinalityZero(CComplexObject cComplexObject) {
        return (cComplexObject.getOccurrences() != null) &&
                (cComplexObject.getOccurrences().getUpper() != null) &&
                (cComplexObject.getOccurrences().getUpper() <= 0);
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

    private void processSectionsAndClusters(CObject cObject, String path, String archetypeId, String text, String desc, String type) {
        if (type.equals(OpenEHRConst.SECTION)) {
            String auxText = getSectionName(path);
            //If the user specifies a special name on the template designer
            if (auxText != null) {
                text = auxText;
            }
        }
        ClusterVO clusterVO =
                new ClusterVOBuilder()
                        .setName(text)
                        .setDescription(desc)
                        .setType(type)
                        .setIdArchetype(archetypeId)
                        .setIdTemplate(templateId)
                        .setPath(path)
                        .createClusterVO();
        setCardinalities(clusterVO, cObject);
        clusterVOs.add(clusterVO);
    }

    private void processDataValue(CObject cObject, String path, String archetypeId, Archetype localAOM, String text, String desc, String type, CObject childCObject) {
        ArchetypeElementVO archetypeElementVO =
                new ArchetypeElementVOBuilder()
                        .setName(text)
                        .setDescription(desc)
                        .setType(type)
                        .setIdArchetype(archetypeId)
                        .setIdTemplate(templateId)
                        .setPath(path)
                        .createArchetypeElementVO();
        setCardinalities(archetypeElementVO, cObject);
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

    private static void setCardinalities(PathableVO pathableVO, CObject cObject) {
        //TODO
        pathableVO.setLowerCardinality(cObject.getOccurrences().getLower());
        pathableVO.setUpperCardinality(cObject.getOccurrences().getUpper());
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
                int i = 0;
                Iterator<CAttribute> atIt = atts.iterator();
                while (atIt.hasNext() && !codedListFound) {
                    CAttribute att2 = atIt.next();
                    List<CObject> childAtts = att2.getChildren();
                    Iterator<CObject> childattsIt = childAtts.iterator();
                    while (childattsIt.hasNext() && !codedListFound) {
                        CObject cObject = childattsIt.next();
                        if (cObject instanceof CCodePhrase) {
                            CCodePhrase cCodePhrase = ((CCodePhrase) cObject);
                            if (cCodePhrase.getCodeList() != null) {
                                for (String codedStr : cCodePhrase.getCodeList()) {
                                    String text = codedStr;
                                    String desc = codedStr;
                                    String terminologyId = cCodePhrase.getTerminologyId().getValue();
                                    CodedTextVO codedText =
                                            new CodedTextVOBuilder()
                                                    .setName(text)
                                                    .setDescription(desc)
                                                    .setType(cCodePhrase.getRmTypeName())
                                                    .setIdArchetype(archetypeId)
                                                    .setIdTemplate(templateId)
                                                    .setPath(path)
                                                    .setTerminology(terminologyId)
                                                    .setCode(codedStr)
                                                    .createCodedTextVO();
                                    if (terminologyId.equals(OpenEHRConst.LOCAL)) {
                                        codedText.setName(getText(ar, codedStr, language));
                                        codedText.setDescription(getDescription(ar, codedStr, language));
                                    } else {
                                        addSubclassCodedTexts(codedText, codedTextVOs);
                                    }
                                    codedTextVOs.add(codedText);
                                    if (!"local".equals(terminologyId) && i++ > 15) { //No need to load the whole terminology for external references
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
            try {
                TerminologyNodeVO node = terminologyService.retrieveAllSubclasses(
                        new CodePhrase(codedTextVO.getTerminology(), codedTextVO.getCode()),
                        OpenEHRDataValuesUI.getLanguageCodePhrase());
                if (node == null) {
                    LoggerFactory.getLogger(GenericObjectBundleADLManager.class).warn("Terminology code not found '" + codedTextVO.getCode() + "::" + codedTextVO.getTerminology() + "'.");
                    return;
                }
                DvCodedText ct = node.getValue();
                codedTextVO.setName(getValidCodedTextName(ct.getValue()));
                codedTextVO.setDescription(getValidCodedTextName(ct.getValue()));
                if (codedTextVOs.size() > 15) { //No need to load the whole terminology
                    return;
                }
                addCodedTextVOs(node, codedTextVO, codedTextVOs);
            } catch (Exception e) {
                ExceptionHandler.handle(e);
            }
        }
    }

    private static void addCodedTextVOs(TerminologyNodeVO root, CodedTextVO rootCodedTextVO, Collection<CodedTextVO> codedTextVOs) {
        if (codedTextVOs.size() > 15) { //No need to load the whole terminology
            return;
        }
        for (TerminologyNodeVO node : root.getChildren()) {
            DvCodedText ct = node.getValue();
            CodedTextVO codedTextVO =
                    new CodedTextVOBuilder()
                            .setName(getValidCodedTextName(ct.getValue()))
                            .setDescription(getValidCodedTextName(ct.getValue()))
                            .setType(OpenEHRDataValues.DV_CODED_TEXT)
                            .setIdArchetype(rootCodedTextVO.getIdArchetype())
                            .setIdTemplate(rootCodedTextVO.getIdTemplate())
                            .setPath(rootCodedTextVO.getPath())
                            .setTerminology(ct.getDefiningCode().getTerminologyId().getValue())
                            .setCode(ct.getDefiningCode().getCodeString())
                            .createCodedTextVO();
            codedTextVOs.add(codedTextVO);
            addCodedTextVOs(node, codedTextVO, codedTextVOs);
        }
    }

    /* Remove all parenthesis to avoid parsing problems */
    private static String getValidCodedTextName(String string) {
        return string.replaceAll("\\(", "[").replaceAll("\\)", "\\]");
    }

    private static void loadOrdinals(
            String archetypdId,
            Archetype ar,
            String templateId,
            String path,
            CObject childCObject,
            String language,
            Collection<OrdinalVO> ordinalVOs) {
        if (childCObject instanceof CDvOrdinal) {
            CDvOrdinal cDvOrdinal = (CDvOrdinal) childCObject;
            if (cDvOrdinal.getList() != null) {
                for (Ordinal ordinal : cDvOrdinal.getList()) {
                    String codedStr = ordinal.getSymbol().getCodeString();
                    String text = codedStr;
                    String desc = codedStr;
                    if (ordinal.getSymbol().getTerminologyId().getValue().equals("local")) {
                        text = getText(ar, codedStr, language);
                        desc = getDescription(ar, codedStr, language);
                    } else {
                        LoggerFactory.getLogger(GenericObjectBundleADLManager.class).error("Unkown terminology: '" + ordinal.getSymbol().getTerminologyId().getValue() + "', skipping...");
                        //TODO TERMINOLOGY SERVICE
                    }
                    ordinalVOs.add(
                            new OrdinalVOBuilder()
                                    .setName(text)
                                    .setDescription(desc)
                                    .setType(cDvOrdinal.getRmTypeName())
                                    .setIdArchetype(archetypdId)
                                    .setIdTemplate(templateId)
                                    .setPath(path)
                                    .setValue(ordinal.getValue())
                                    .setTerminology(ordinal.getSymbol().getTerminologyId().getValue())
                                    .setCode(ordinal.getSymbol().getCodeString())
                                    .createOrdinalVO());
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
            CDvQuantity cDvQuantity = (CDvQuantity) childCObject;
            if (cDvQuantity.getList() != null) {
                for (CDvQuantityItem cDvQuantityItem : cDvQuantity.getList()) {
                    unitVOs.add(new UnitVO(templateId, idElement, cDvQuantityItem.getUnits()));
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
            CComplexObject cComplexObject = (CComplexObject) childCObject;
            CAttribute cAttribute = cComplexObject.getAttribute("type");
            if (cAttribute != null) {
                for (CObject cObject : cAttribute.getChildren()) {
                    if (cObject instanceof CPrimitiveObject) {
                        CPrimitive cPrimitive = ((CPrimitiveObject) cObject).getItem();
                        if (cPrimitive instanceof CInteger) {
                            CInteger cInteger = (CInteger) cPrimitive;
                            for (Integer proportionType : cInteger.getList()) {
                                proportionTypeVOs.add(new ProportionTypeVO(templateId, idElement, proportionType));
                            }
                        }
                    }
                }
                ;
            }
        }
    }

    private String getText(Archetype ar, CObject cObject, String language) {
        String text = getText(ar, cObject.getNodeId(), language);
        if (text != null) {
            return text;
        } else {
            Archetype archetype = archetypeMap.get(cObject.getNodeId());
            if (archetype != null) {
                text = archetype.getConceptName(language);
            } else {
                text = cObject.getNodeId();
            }
            return text;
        }
    }

    private String getDescription(Archetype ar, CObject cObject, String language) {
        String desc = getDescription(ar, cObject.getNodeId(), language);
        if (desc != null) {
            return desc;
        } else {
            Archetype archetype = archetypeMap.get(cObject.getNodeId());
            if (archetype != null) {
                String conceptCode = archetype.getConcept();
                ArchetypeTerm term = archetype.getOntology().termDefinition(language, conceptCode);
                desc = term.getDescription();
            } else {
                desc = cObject.getNodeId();
            }
            return desc;
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
        int i = path.lastIndexOf("[");
        while (i > 0) {
            String idArchetype = path.substring(i + 1, path.lastIndexOf("]"));
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
                path = path.substring(0, i);
                i = path.lastIndexOf("[");
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
}

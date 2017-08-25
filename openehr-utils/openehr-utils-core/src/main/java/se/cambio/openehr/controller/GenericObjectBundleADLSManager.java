package se.cambio.openehr.controller;

import org.openehr.jaxb.am.*;
import org.openehr.jaxb.rm.CodePhrase;
import org.openehr.jaxb.rm.DvOrdinal;
import org.openehr.jaxb.rm.StringDictionaryItem;
import org.openehr.jaxb.rm.TranslationDetails;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.model.archetype.vo.*;
import se.cambio.cm.model.util.OpenEHRRMUtil;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.ArchetypeTermMapGenerator;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.exceptions.ArchetypeProcessingException;

import java.util.*;

public class GenericObjectBundleADLSManager {
    private final ArchetypeManager archetypeManager;
    private String language = null;
    private Archetype ar = null;
    private Collection<ArchetypeElementVO> archetypeElementVOs;
    private Collection<ClusterVO> clusterVOs;
    private Collection<CodedTextVO> codedTextVOs;
    private Collection<OrdinalVO> ordinalVOs;
    private Collection<UnitVO> unitVOs;
    private Collection<ProportionTypeVO> proportionTypeVOs;
    private Map<String, Map<String, String>> termDefinitionsArchetypeTermMap;
    private Logger logger = LoggerFactory.getLogger(GenericObjectBundleADLSManager.class);
    private Map<String, List<String>> valueSetsMap;

    public GenericObjectBundleADLSManager(Archetype ar, ArchetypeManager archetypeManager) {
        this.ar = ar;
        this.archetypeManager = archetypeManager;
    }

    public ArchetypeObjectBundleCustomVO generateObjectBundleCustomVO() throws ArchetypeProcessingException {
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
        language = archetypeManager.getUserConfigurationManager().getLanguage();
        if (!ar.getOriginalLanguage().getCodeString().equals(language)
                && (ar.getTranslations() == null || !containsLanguage(ar.getTranslations(), language))) {
            language = ar.getOriginalLanguage().getCodeString();
        }
    }

    private boolean containsLanguage(List<TranslationDetails> translationDetails, String language) {
        for (TranslationDetails translationDetail : translationDetails) {
            String translationLanguage = translationDetail.getLanguage().getCodeString();
            if (language.equals(translationLanguage)) {
                return true;
            }
        }
        return false;
    }

    private void init() {
        archetypeElementVOs = new ArrayList<>();
        clusterVOs = new ArrayList<>();
        codedTextVOs = new ArrayList<>();
        ordinalVOs = new ArrayList<>();
        unitVOs = new ArrayList<>();
        proportionTypeVOs = new ArrayList<>();
    }

    public void loadArchetypeObjects() throws ArchetypeProcessingException {
        String archId = getArchetypeId();
        String rmEntry = ar.getDefinition().getRmTypeName();
        String path = "";
        for (CAttribute consAttribute : ar.getDefinition().getAttributes()) {
            processAttribute(consAttribute, path);
        }
        Collection<ArchetypeElementVO> rmArchetypeElements = OpenEHRRMUtil.getRMElements(archId, null, rmEntry);
        //TODO Send Archetype (to create long RM paths (data/events/time))
        for (ClusterVO clusterVO : clusterVOs) {
            if (OpenEHRConst.isEntry(clusterVO.getType()) && !clusterVO.getPath().equals("/")) {
                rmArchetypeElements.addAll(OpenEHRRMUtil.getRMElements(archId, null, clusterVO.getType(), clusterVO.getPath()));
                //TODO Send Archetype (to create long RM paths (data/events/time))
            }
        }
        archetypeElementVOs.addAll(rmArchetypeElements);
    }

    private String getArchetypeId() {
        return ar.getArchetypeId().getValue();
    }


    private void processCObject(CObject constrainedObject, String path) throws ArchetypeProcessingException {
        if (constrainedObject instanceof CComplexObject) {
            processCComplexObject((CComplexObject) constrainedObject, path);
        }
    }

    private void processCComplexObject(CComplexObject constrainedComplexObject, String path) throws ArchetypeProcessingException {
        List<CAttribute> consAttributes = constrainedComplexObject.getAttributes();
        String currentPath = getCurrentPath(constrainedComplexObject, path);
        if (constrainedComplexObject.getRmTypeName().equals("ELEMENT")) {
            processElement(constrainedComplexObject, currentPath);
        } else if (constrainedComplexObject instanceof CArchetypeRoot) {
            String usedArchetypeId = ((CArchetypeRoot) constrainedComplexObject).getArchetypeRef();
            processArchetypeReference(usedArchetypeId, currentPath);
        } else {
            for (CAttribute consAttribute : consAttributes) {
                processAttribute(consAttribute, currentPath);
            }
            processClusters(constrainedComplexObject, currentPath);
        }
    }

    private void processArchetypeReference(String usedArchetypeId, String path) throws ArchetypeProcessingException {
        String currentPath = path + "[" + usedArchetypeId + "]";
        ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO =
                archetypeManager.getArchetypes().getArchetypeAOBCVOById(usedArchetypeId);
        processReferencedArcehtypeElements(currentPath, archetypeObjectBundleCustomVO);
        processReferencedClusters(currentPath, archetypeObjectBundleCustomVO);
        processReferencedCodedTexts(currentPath, archetypeObjectBundleCustomVO);
        processReferencedOrdinals(currentPath, archetypeObjectBundleCustomVO);
        processReferencedUnits(archetypeObjectBundleCustomVO);
        processReferencedProportionTypes(archetypeObjectBundleCustomVO);
    }

    private void processReferencedProportionTypes(ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for (ProportionTypeVO proportionTypeVO : archetypeObjectBundleCustomVO.getProportionTypes()) {
            ProportionTypeVO proportionTypeVO1 = proportionTypeVO.toBuilder().build();
            String elementId = proportionTypeVO1.getIdElement();
            int indexOfFirstSlash = elementId.indexOf("/");
            String elementPath = elementId.substring(indexOfFirstSlash, elementId.length());
            elementId = getArchetypeId() + elementPath;
            proportionTypeVO1.setIdElement(elementId);
            proportionTypeVOs.add(proportionTypeVO1);
        }
    }

    private void processReferencedUnits(ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for (UnitVO unitVO : archetypeObjectBundleCustomVO.getUnitVOs()) {
            UnitVO unitVO1 = unitVO.toBuilder().build();
            String elementId = unitVO.getIdElement();
            int indexOfFirstSlash = elementId.indexOf("/");
            String elementPath = elementId.substring(indexOfFirstSlash, elementId.length());
            elementId = getArchetypeId() + elementPath;
            unitVO1.setIdElement(elementId);
            unitVOs.add(unitVO1);
        }
    }

    private void processReferencedOrdinals(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for (OrdinalVO ordinalVO : archetypeObjectBundleCustomVO.getOrdinalVOs()) {
            OrdinalVO ordinalVO1 = ordinalVO.toBuilder().build();
            ordinalVO1.setIdArchetype(getArchetypeId());
            ordinalVO1.setPath(currentPath + ordinalVO.getPath());
            ordinalVOs.add(ordinalVO1);
        }
    }

    private void processReferencedCodedTexts(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for (CodedTextVO codedTextVO : archetypeObjectBundleCustomVO.getCodedTextVOs()) {
            CodedTextVO codedTextVO1 = codedTextVO.toBuilder().build();
            codedTextVO1.setIdArchetype(getArchetypeId());
            codedTextVO1.setPath(currentPath + codedTextVO.getPath());
            codedTextVOs.add(codedTextVO1);
        }
    }

    private void processReferencedClusters(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for (ClusterVO clusterVO : archetypeObjectBundleCustomVO.getClusterVOs()) {
            ClusterVO clusterVO1 = clusterVO.toBuilder().build();
            clusterVO1.setIdArchetype(getArchetypeId());
            clusterVO1.setPath(currentPath + clusterVO.getPath());
            clusterVOs.add(clusterVO1);
        }
    }

    private void processReferencedArcehtypeElements(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for (ArchetypeElementVO archetypeElementVO : archetypeObjectBundleCustomVO.getArchetypeElementVOs()) {
            ArchetypeElementVO archetypeElementVO1 = archetypeElementVO.toBuilder().build();
            archetypeElementVO1.setIdArchetype(getArchetypeId());
            archetypeElementVO1.setPath(currentPath + archetypeElementVO.getPath());
            archetypeElementVOs.add(archetypeElementVO1);
        }
    }

    private String getCurrentPath(CComplexObject constrainedComplexObject, String path) {
        if (constrainedComplexObject.getNodeId() != null) {
            return path + "[" + constrainedComplexObject.getNodeId() + "]";
        } else {
            return path;
        }
    }

    private void processElement(CComplexObject constrainedComplexObject, String currentPath) throws ArchetypeProcessingException {
        if (hasCardinalityZero(constrainedComplexObject)) {
            return;
        }
        String nodeId = constrainedComplexObject.getNodeId();
        Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(nodeId);
        if (termMap == null) {
            logger.warn("Archetype term not found for atCode '" + nodeId + "'");
            return;
        }
        String text = getTermDefinitionsArchetypeTermMap().get(nodeId).get("text");
        String description = termMap.get("description");
        CObject elementDefinitionCObject = getElementDefinitionCObject(constrainedComplexObject, currentPath, "value");
        if (elementDefinitionCObject != null) {
            String rmType = elementDefinitionCObject.getRmTypeName();
            rmType = translateCIMIRM(rmType);
            rmType = mapToKnowRMType(rmType);
            ArchetypeElementVO archetypeElementVO =
                    ArchetypeElementVO.builder()
                            .name(text)
                            .description(description)
                            .idArchetype(getArchetypeId())
                            .type(rmType)
                            .path(currentPath)
                            .build();
            setCardinalities(archetypeElementVO, constrainedComplexObject);
            archetypeElementVOs.add(archetypeElementVO);
            processAdditionalPathables(currentPath, elementDefinitionCObject, rmType);
        } else {
            logger.warn("Could not find CObject for path '{}'", currentPath);
        }
    }

    private String mapToKnowRMType(String rmType) {
        if ("C_TERMINOLOGY_CODE".equals(rmType)) {
            return OpenEHRDataValues.DV_CODED_TEXT;
        } else {
            return rmType;
        }
    }

    private void processAdditionalPathables(String currentPath, CObject elementDefinitionCObject, String rmType) throws ArchetypeProcessingException {
        if (OpenEHRDataValues.DV_QUANTITY.equals(rmType)) {
            loadUnits(elementDefinitionCObject, currentPath);
        } else if (elementDefinitionCObject instanceof CTerminologyCode) {
            processCTerminologyCode((CTerminologyCode) elementDefinitionCObject, currentPath);
        } else if (OpenEHRDataValues.DV_CODED_TEXT.equals(rmType)) {
            processCodedTexts(elementDefinitionCObject, currentPath);
        } else if (OpenEHRDataValues.DV_ORDINAL.equals(rmType)) {
            processOrdinals(elementDefinitionCObject, currentPath);
        } else if (OpenEHRDataValues.DV_PROPORTION.equals(rmType)) {
            processProportion(elementDefinitionCObject, currentPath);
        }
    }

    private void processCTerminologyCode(CTerminologyCode constrainedTerminologyCode, String currentPath) throws ArchetypeProcessingException {
        List<String> acCodes = constrainedTerminologyCode.getCodeList();
        for (String acCode : acCodes) {
            List<String> members = getValueSetsMap().get(acCode);
            if (members == null) {
                logger.warn("No members could be found for code '" + acCode + "'");
                return;
            }
            for (String memberCode : members) {
                processCodedTextItem("local", currentPath, memberCode);
            }
        }
    }

    private void processCodedTexts(CObject constrainedObject, String currentPath) throws ArchetypeProcessingException {
        if (constrainedObject instanceof CComplexObject) {
            CComplexObject constrainedComplexObject = (CComplexObject) constrainedObject;
            CObject constrainedObjectDefinedCode = getElementDefinitionCObject(constrainedComplexObject, currentPath, "defining_code");
            if (constrainedObjectDefinedCode instanceof CTerminologyCode) {
                CTerminologyCode constrainedTerminologyCode = (CTerminologyCode) constrainedObjectDefinedCode;
                processCTerminologyCode(constrainedTerminologyCode, currentPath);
            } else {
                processCodedTextsWithoutDefiningCode(constrainedComplexObject, currentPath);
            }
        }
    }

    private void processCodedTextsWithoutDefiningCode(CComplexObject constrainedComplexObject, String currentPath) throws ArchetypeProcessingException {
        Map<String, List<String>> terminologyCodesMap = getTerminologyCodesMap(constrainedComplexObject, currentPath);
        for (Map.Entry<String, List<String>> entrySet : terminologyCodesMap.entrySet()) {
            for (String code : entrySet.getValue()) {
                processCodedTextItem(entrySet.getKey(), currentPath, code);
            }
        }
    }

    private Map<String, List<String>> getTerminologyCodesMap(CComplexObject constrainedComplexObject, String currentPath)
            throws ArchetypeProcessingException {
        Map<String, List<String>> terminologyCodesMap = new HashMap<>();
        CObject terminologyIdCObject = getElementDefinitionCObject(constrainedComplexObject, currentPath, "terminology_id");
        CObject termIdCObject = getElementDefinitionCObject(constrainedComplexObject, currentPath, "term_id");
        if (terminologyIdCObject instanceof CComplexObject
                && termIdCObject instanceof CString) {
            CObject terminologyIdCString = getElementDefinitionCObject((CComplexObject) terminologyIdCObject, currentPath, "value");
            String terminologyId = null;
            if (terminologyIdCString instanceof CString) {
                terminologyId = ((CString) terminologyIdCString).getDefaultValue();
            }
            List<String> codes = ((CString) termIdCObject).getList();
            terminologyCodesMap.put(terminologyId, codes);
        }
        return terminologyCodesMap;
    }


    private void processCodedTextItem(String terminologyId, String currentPath, String code) throws ArchetypeProcessingException {
        Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(code);
        if (termMap == null) {
            logger.warn("Archetype term not found for atCode '" + code + "'");
            return;
        }
        CodedTextVO codedTextVO = CodedTextVO.builder()
                .name(termMap.get("text"))
                .description(termMap.get("description"))
                .type(OpenEHRDataValues.DV_CODED_TEXT)
                .idArchetype(getArchetypeId())
                .code(code)
                .terminology(terminologyId)
                .path(currentPath)
                .build();
        codedTextVOs.add(codedTextVO);
    }

    private void processOrdinals(CObject constrainedObject, String currentPath) throws ArchetypeProcessingException {
        if (constrainedObject instanceof CComplexObject) {
            CObject constrainedObjectDefinedCode = getElementDefinitionCObject((CComplexObject) constrainedObject, currentPath, "value");
            if (constrainedObjectDefinedCode instanceof CDvOrdinal) {
                CDvOrdinal consDvOrdinal = (CDvOrdinal) constrainedObjectDefinedCode;
                for (DvOrdinal dvOrdinal : consDvOrdinal.getList()) {
                    CodePhrase definingCode = dvOrdinal.getSymbol().getDefiningCode();
                    String code = definingCode.getCodeString();
                    Map<String, String> referenceTerm = getTermDefinitionsArchetypeTermMap().get(code);
                    if (referenceTerm == null) {
                        logger.warn("Archetype term not found for atCode '" + code + "'");
                        continue;
                    }
                    OrdinalVO ordinalVO = OrdinalVO.builder()
                            .name(referenceTerm.get("text"))
                            .description(referenceTerm.get("description"))
                            .type(constrainedObject.getRmTypeName())
                            .idArchetype(getArchetypeId())
                            .value(dvOrdinal.getValue())
                            .code(code)
                            .terminology(definingCode.getTerminologyId().getValue())
                            .path(currentPath)
                            .build();
                    ordinalVOs.add(ordinalVO);
                }
            }
        }
    }

    private void processClusters(CObject constrainedObject, String path) throws ArchetypeProcessingException {
        String nodeId = constrainedObject.getNodeId();
        Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(nodeId);
        if (termMap == null) {
            logger.warn("Archetype term not found for atCode '" + nodeId + "'");
            return;
        }
        ClusterVO clusterVO = ClusterVO.builder()
                .name(termMap.get("text"))
                .description(termMap.get("description"))
                .type(constrainedObject.getRmTypeName())
                .idArchetype(getArchetypeId())
                .path(path)
                .build();
        setCardinalities(clusterVO, constrainedObject);
        clusterVOs.add(clusterVO);
    }

    private static void setCardinalities(PathableVO pathableVO, CObject constrainedObject) {
        pathableVO.setLowerCardinality(constrainedObject.getOccurrences().getLower());
        pathableVO.setUpperCardinality(constrainedObject.getOccurrences().getUpper());
    }

    private void loadUnits(CObject constrainedObject, String path) throws ArchetypeProcessingException {
        if (constrainedObject instanceof CComplexObject) {
            CComplexObject constrainedComplexObject = (CComplexObject) constrainedObject;
            List<CAttributeTuple> attributeTuples = constrainedComplexObject.getAttributeTuples();
            for (CAttributeTuple consAttributeTuple : attributeTuples) {
                processUnits(path, consAttributeTuple);
            }
            CObject elementDefinitionCObject = getElementDefinitionCObject(constrainedComplexObject, path, "units");
            if (elementDefinitionCObject instanceof CTerminologyCode) {
                List<String> codes = ((CTerminologyCode) elementDefinitionCObject).getCodeList();
                List<String> units = new ArrayList<>();
                for (String code : codes) {
                    Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(code);
                    if (termMap == null) {
                        throw new ArchetypeProcessingException("Unknown identifier '" + code + "'");
                    }
                    units.add(termMap.get("text"));
                }
                String elementId = getArchetypeId() + path;
                processUnits(elementId, units);
            } else if (elementDefinitionCObject instanceof CComplexObject) {
                processUnitsWithoutCTerminologyCode((CComplexObject) elementDefinitionCObject, path);
            }
        }
    }

    private void processUnitsWithoutCTerminologyCode(CComplexObject constrainedComplexObject, String path) throws ArchetypeProcessingException {
        Map<String, List<String>> terminologyCodesMap = getTerminologyCodesMap(constrainedComplexObject, path);
        String elementId = getArchetypeId() + path;
        for (Map.Entry<String, List<String>> entrySet : terminologyCodesMap.entrySet()) {
            List<String> units = new ArrayList<>();
            for (String code : entrySet.getValue()) {
                Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(code);
                if (termMap == null) {
                    logger.warn("Archetype term not found for atCode '" + code + "'");
                    continue;
                }
                units.add(termMap.get("text"));
            }
            processUnits(elementId, units);
        }

    }

    private void processUnits(String path, CAttributeTuple consAttributeTuple) throws ArchetypeProcessingException {
        List<String> units = getUnitsCStrings(consAttributeTuple);
        String elementId = getArchetypeId() + path;
        processUnits(elementId, units);
    }

    private void processUnits(String elementId, List<String> units) {
        for (String unit : units) {
            UnitVO unitVO = UnitVO.builder()
                    .idElement(elementId)
                    .unit(unit)
                    .build();
            unitVOs.add(unitVO);
        }
    }

    private void processProportion(CObject constrainedObject, String currentPath) throws ArchetypeProcessingException {
        if (constrainedObject instanceof CComplexObject) {
            CObject elementDefinitionCObject = getElementDefinitionCObject((CComplexObject) constrainedObject, currentPath, "type");
            List<Integer> proportionTypes = getCIntegers(elementDefinitionCObject);
            for (Integer proportionType : proportionTypes) {
                proportionTypeVOs.add(
                        ProportionTypeVO.builder()
                                .idElement(getArchetypeId() + currentPath)
                                .type(proportionType)
                                .build());
            }
        }
    }

    private List<String> getUnitsCStrings(CAttributeTuple consAttributeTuple) throws ArchetypeProcessingException {
        int unitsIndex = getUnitsIndex(consAttributeTuple);
        List<CObject> constrainedObjects = getCObjectsFromTuple(consAttributeTuple, unitsIndex);
        return getCStrings(constrainedObjects);
    }

    private List<String> getCStrings(List<CObject> constrainedObjects) throws ArchetypeProcessingException {
        List<String> consStrings = new ArrayList<>();
        for (CObject constrainedObject : constrainedObjects) {
            if (!(constrainedObject instanceof CString)) {
                throw new ArchetypeProcessingException("Expecting C_STRING but found '" + constrainedObject.getRmTypeName() + "'");
            }
            consStrings.addAll(((CString) constrainedObject).getList());
        }
        return consStrings;
    }

    private List<Integer> getCIntegers(CObject constrainedObject) throws ArchetypeProcessingException {
        List<Integer> consIntegers = new ArrayList<>();
        if (!(constrainedObject instanceof CInteger)) {
            throw new ArchetypeProcessingException("Expecting C_INTEGER but found '" + constrainedObject.getRmTypeName() + "'");
        }
        consIntegers.addAll(((CInteger) constrainedObject).getList());
        return consIntegers;
    }

    private List<CObject> getCObjectsFromTuple(CAttributeTuple consAttributeTuple, int index) {
        List<CObject> constrainedObjects = new ArrayList<>();
        for (CObjectTuple constrainedObjectTuple : consAttributeTuple.getChildren()) {
            constrainedObjects.add(constrainedObjectTuple.getMembers().get(index));
        }
        return constrainedObjects;
    }

    private Integer getUnitsIndex(CAttributeTuple consAttributeTuple) {
        Integer index = 0;
        for (CAttribute consSingleAttribute : consAttributeTuple.getMembers()) {
            if (consSingleAttribute.getRmAttributeName().equals("units")) {
                return index;
            }
            index++;
        }
        return null;
    }

    private CObject getElementDefinitionCObject(CComplexObject constrainedComplexObject, String path, String rmName)
            throws ArchetypeProcessingException {
        for (CAttribute consAttribute : constrainedComplexObject.getAttributes()) {
            if (consAttribute.getRmAttributeName().equals(rmName)) {
                if (consAttribute.getChildren().isEmpty()) {
                    throw new ArchetypeProcessingException("Could not find constrainedComplexObject for element at path '" + path + "'");
                }
                return consAttribute.getChildren().iterator().next();
            }
        }
        return null;
    }

    private void processAttribute(CAttribute consAttribute, String path) throws ArchetypeProcessingException {
        for (CObject constrainedObject : consAttribute.getChildren()) {
            processCObject(constrainedObject, path + "/" + consAttribute.getRmAttributeName());
        }
    }

    private Map<String, Map<String, String>> getTermDefinitionsArchetypeTermMap() throws ArchetypeProcessingException {
        if (termDefinitionsArchetypeTermMap == null) {
            ArchetypeTermMapGenerator archetypeTermMapGenerator = new ArchetypeTermMapGenerator(ar.getTerminology().getTermDefinitions(), language);
            termDefinitionsArchetypeTermMap = archetypeTermMapGenerator.generateTermDefinitionsArchetypeTermMap();
        }
        return termDefinitionsArchetypeTermMap;
    }

    private Map<String, List<String>> getValueSetsMap() {
        if (valueSetsMap == null) {
            valueSetsMap = generateValueSetsMap();
        }
        return valueSetsMap;
    }

    private Map<String, List<String>> generateValueSetsMap() {
        Map valueSetMap = new HashMap<String, List<String>>();
        for (ValueSetItem valueSetItem : ar.getTerminology().getValueSets()) {
            valueSetMap.put(valueSetItem.getId(), valueSetItem.getMembers());
        }
        return valueSetMap;
    }

    private boolean hasCardinalityZero(CComplexObject constrainedComplexObject) {
        return (constrainedComplexObject.getOccurrences() != null)
                && (constrainedComplexObject.getOccurrences().getUpper() != null)
                && (constrainedComplexObject.getOccurrences().getUpper() <= 0);
    }

    private String translateCIMIRM(String rmName) {
        if ("PLAIN_TEXT".equals(rmName) || "TEXT".equals(rmName)) {
            return OpenEHRDataValues.DV_TEXT;
        } else if ("CODED_TEXT".equals(rmName)) {
            return OpenEHRDataValues.DV_CODED_TEXT;
        } else if ("ORDINAL".equals(rmName)) {
            return OpenEHRDataValues.DV_ORDINAL;
        } else if ("QUANTITY".equals(rmName)) {
            return OpenEHRDataValues.DV_QUANTITY;
        } else if ("YESNO".equals(rmName)) {
            return OpenEHRDataValues.DV_BOOLEAN;
        } else if ("COUNT".equals(rmName)) {
            return OpenEHRDataValues.DV_COUNT;
        } else if ("DURATION".equals(rmName)) {
            return OpenEHRDataValues.DV_DURATION;
        } else if ("DATE_TIME".equals(rmName)) {
            return OpenEHRDataValues.DV_DATE_TIME;
        } else if ("DATE".equals(rmName)) {
            return OpenEHRDataValues.DV_DATE;
        } else if ("TIME".equals(rmName)) {
            return OpenEHRDataValues.DV_TIME;
        } else if ("PROPORTION".equals(rmName)) {
            return OpenEHRDataValues.DV_PROPORTION;
        } else if ("IDENTIFIER".equals(rmName)) {
            return OpenEHRDataValues.DV_IDENTIFIER;
        } else {
            return rmName;
        }
    }

    public Collection<ArchetypeTermVO> generateArchetypeTerms() {
        Collection<ArchetypeTermVO> archetypeTermVOs = new ArrayList<>();
        ArchetypeTerminology ao = ar.getTerminology();
        List<CodeDefinitionSet> termDefinitions = ao.getTermDefinitions();
        for (CodeDefinitionSet codeDefinitionSet : termDefinitions) {
            String lang = codeDefinitionSet.getLanguage();
            for (ArchetypeTerm archetypeTerm : codeDefinitionSet.getItems()) {
                String text = getDictionaryItem("text", archetypeTerm);
                String description = getDictionaryItem("description", archetypeTerm);
                archetypeTermVOs.add(ArchetypeTermVO.builder()
                        .archetypeId(ar.getArchetypeId().getValue())
                        .language(lang)
                        .text(text)
                        .description(description)
                        .build());
            }
        }
        return archetypeTermVOs;
    }

    private String getDictionaryItem(String parameterName, ArchetypeTerm archetypeTerm) {
        for (StringDictionaryItem dictionaryItem : archetypeTerm.getItems()) {
            if (dictionaryItem.getId().equals(parameterName)) {
                return dictionaryItem.getValue();
            }
        }
        return null;
    }
}

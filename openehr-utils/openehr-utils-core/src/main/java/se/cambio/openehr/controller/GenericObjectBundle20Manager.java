package se.cambio.openehr.controller;

import org.apache.log4j.Logger;
import org.openehr.jaxb.am.*;
import org.openehr.jaxb.rm.CodePhrase;
import org.openehr.jaxb.rm.DvOrdinal;
import org.openehr.jaxb.rm.TranslationDetails;
import se.cambio.cm.model.archetype.vo.*;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.ArchetypeTermMapGenerator;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRRMUtil;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.ArchetypeProcessingException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GenericObjectBundle20Manager {
    private final ArchetypeManager archetypeManager;
    private String language = null;
    private FlatArchetype ar = null;
    private Collection<ArchetypeElementVO> archetypeElementVOs;
    private Collection<ClusterVO> clusterVOs;
    private Collection<CodedTextVO> codedTextVOs;
    private Collection<OrdinalVO> ordinalVOs;
    private Collection<UnitVO> unitVOs;
    private Collection<ProportionTypeVO> proportionTypeVOs;
    private Map<String, Map<String, String>> termDefinitionsArchetypeTermMap;
    private Logger logger = Logger.getLogger(GenericObjectBundle20Manager.class);
    private Map<String, List<String>> valueSetsMap;

    public GenericObjectBundle20Manager(FlatArchetype ar, ArchetypeManager archetypeManager){
        this.ar = ar;
        this.archetypeManager = archetypeManager;
    }

    public ArchetypeObjectBundleCustomVO generateObjectBundleCustomVO() throws ArchetypeProcessingException {
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
        language = UserConfigurationManager.getLanguage();
        if (!ar.getOriginalLanguage().getCodeString().equals(language) &&
                (ar.getTranslations() == null || !containsLanguage(ar.getTranslations(),language))){
            language = ar.getOriginalLanguage().getCodeString();
        }
    }

    private boolean containsLanguage(List<TranslationDetails> translationDetails, String language){
        for (TranslationDetails translationDetail: translationDetails){
            String translationLanguage = translationDetail.getLanguage().getCodeString();
            if (language.equals(translationLanguage)){
                return true;
            }
        }
        return false;
    }

    private void init() {
        archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
        clusterVOs = new ArrayList<ClusterVO>();
        codedTextVOs = new ArrayList<CodedTextVO>();
        ordinalVOs = new ArrayList<OrdinalVO>();
        unitVOs = new ArrayList<UnitVO>();
        proportionTypeVOs = new ArrayList<ProportionTypeVO>();
    }

    public void loadArchetypeObjects() throws ArchetypeProcessingException {
        String archId = getArchetypeId();
        String rmEntry = ar.getDefinition().getRmTypeName();
        String path = "";
        for(CAttribute cAttribute: ar.getDefinition().getAttributes()){
            processAttribute(cAttribute, path);
        }
        Collection<ArchetypeElementVO> rmArchetypeElements = OpenEHRRMUtil.getRMElements(archId, null, rmEntry);
        for (ClusterVO clusterVO: clusterVOs){
            if (OpenEHRConst.isEntry(clusterVO.getRMType()) && !clusterVO.getPath().equals("/")){
                rmArchetypeElements.addAll(OpenEHRRMUtil.getRMElements(archId, null, clusterVO.getRMType(), clusterVO.getPath()));
            }
        }
        archetypeElementVOs.addAll(rmArchetypeElements);
    }

    private String getArchetypeId() {
        return ar.getArchetypeId().getValue();
    }


    private void processCObject(CObject cObject, String path) throws ArchetypeProcessingException {
        if (cObject instanceof CComplexObject){
            processCComplexObject((CComplexObject) cObject, path);
        }
    }

    private void processCComplexObject(CComplexObject cComplexObject, String path) throws ArchetypeProcessingException {
        List<CAttribute> cAttributes = cComplexObject.getAttributes();
        String currentPath = getCurrentPath(cComplexObject, path);
        if (cComplexObject.getRmTypeName().equals("ELEMENT")) {
            processElement(cComplexObject, currentPath);
        } else if (cComplexObject instanceof CArchetypeRoot){
            String usedArchetypeId = ((CArchetypeRoot)cComplexObject).getArchetypeId().getValue();
            processArchetypeReference(usedArchetypeId, currentPath);
        } else {
            for(CAttribute cAttribute: cAttributes){
                processAttribute(cAttribute, currentPath);
            }
            processClusters(cComplexObject, currentPath);
        }
    }

    private void processArchetypeReference(String usedArchetypeId, String path) throws ArchetypeProcessingException {
        String currentPath = path + "[" + usedArchetypeId + "]";
        try {
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO =
                    archetypeManager.getArchetypes().getArchetypeAOBCVOById(usedArchetypeId);
            processReferencedArcehtypeElements(currentPath, archetypeObjectBundleCustomVO);
            processReferencedClusters(currentPath, archetypeObjectBundleCustomVO);
            processReferencedCodedTexts(currentPath, archetypeObjectBundleCustomVO);
            processReferencedOrdinals(currentPath, archetypeObjectBundleCustomVO);
            processReferencedUnits(archetypeObjectBundleCustomVO);
            processReferencedProportionTypes(archetypeObjectBundleCustomVO);
        } catch (InternalErrorException e) {
            throw new ArchetypeProcessingException(e.getMessage());
        } catch (InstanceNotFoundException e) {
            throw new ArchetypeProcessingException(e.getMessage());
        }
    }

    private void processReferencedProportionTypes(ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for(ProportionTypeVO proportionTypeVO: archetypeObjectBundleCustomVO.getProportionTypes()){
            ProportionTypeVO proportionTypeVO1 = proportionTypeVO.clone();
            String elementId = proportionTypeVO1.getIdElement();
            int indexOfFirstSlash = elementId.indexOf("/");
            String elementPath = elementId.substring(indexOfFirstSlash, elementId.length());
            elementId = getArchetypeId() + elementPath;
            proportionTypeVO1.setIdElement(elementId);
            proportionTypeVOs.add(proportionTypeVO1);
        }
    }

    private void processReferencedUnits(ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for(UnitVO unitVO: archetypeObjectBundleCustomVO.getUnitVOs()){
            UnitVO unitVO1 = unitVO.clone();
            String elementId = unitVO.getIdElement();
            int indexOfFirstSlash = elementId.indexOf("/");
            String elementPath = elementId.substring(indexOfFirstSlash, elementId.length());
            elementId = getArchetypeId() + elementPath;
            unitVO1.setIdElement(elementId);
            unitVOs.add(unitVO1);
        }
    }

    private void processReferencedOrdinals(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for(OrdinalVO ordinalVO: archetypeObjectBundleCustomVO.getOrdinalVOs()){
            OrdinalVO ordinalVO1 = ordinalVO.clone();
            ordinalVO1.setIdArchetype(getArchetypeId());
            ordinalVO1.setPath(currentPath + ordinalVO.getPath());
            ordinalVOs.add(ordinalVO1);
        }
    }

    private void processReferencedCodedTexts(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for(CodedTextVO codedTextVO: archetypeObjectBundleCustomVO.getCodedTextVOs()){
            CodedTextVO codedTextVO1 = codedTextVO.clone();
            codedTextVO1.setIdArchetype(getArchetypeId());
            codedTextVO1.setPath(currentPath + codedTextVO.getPath());
            codedTextVOs.add(codedTextVO1);
        }
    }

    private void processReferencedClusters(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for(ClusterVO clusterVO: archetypeObjectBundleCustomVO.getClusterVOs()){
            ClusterVO clusterVO1 = clusterVO.clone();
            clusterVO1.setIdArchetype(getArchetypeId());
            clusterVO1.setPath(currentPath + clusterVO.getPath());
            clusterVOs.add(clusterVO1);
        }
    }

    private void processReferencedArcehtypeElements(String currentPath, ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        for(ArchetypeElementVO archetypeElementVO: archetypeObjectBundleCustomVO.getArchetypeElementVOs()){
            ArchetypeElementVO archetypeElementVO1 = archetypeElementVO.clone();
            archetypeElementVO1.setIdArchetype(getArchetypeId());
            archetypeElementVO1.setPath(currentPath + archetypeElementVO.getPath());
            archetypeElementVOs.add(archetypeElementVO1);
        }
    }

    private String getCurrentPath(CComplexObject cComplexObject, String path) {
        if (cComplexObject.getNodeId() != null) {
            return path + "[" + cComplexObject.getNodeId() + "]";
        } else {
            return path;
        }
    }

    private void processElement(CComplexObject cComplexObject, String currentPath) throws ArchetypeProcessingException {
        if (hasCardinalityZero(cComplexObject)){
            return;
        }
        String nodeId = cComplexObject.getNodeId();
        Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(nodeId);
        if (termMap == null) {
            logger.warn("Archetype term not found for atCode '" + nodeId + "'");
            return;
        }
        String text = getTermDefinitionsArchetypeTermMap().get(nodeId).get("text");
        String description = termMap.get("description");
        CObject elementDefinitionCObject = getElementDefinitionCObject(cComplexObject, currentPath, "value");
        String rmType = elementDefinitionCObject.getRmTypeName();
        rmType = translateCIMIRM(rmType);
        rmType = mapToKnowRMType(rmType);
        ArchetypeElementVO archetypeElementVO =
                new ArchetypeElementVOBuilder()
                        .setName(text)
                        .setDescription(description)
                        .setIdArchetype(getArchetypeId())
                        .setType(rmType)
                        .setPath(currentPath)
                        .createArchetypeElementVO();
        setCardinalities(archetypeElementVO, cComplexObject);
        archetypeElementVOs.add(archetypeElementVO);
        processAdditionalPathables(currentPath, elementDefinitionCObject, rmType);
    }

    private String mapToKnowRMType(String rmType) {
        if ("C_TERMINOLOGY_CODE".equals(rmType)){
            return OpenEHRDataValues.DV_CODED_TEXT;
        } else {
            return rmType;
        }
    }

    private void processAdditionalPathables(String currentPath, CObject elementDefinitionCObject, String rmType) throws ArchetypeProcessingException {
        if (OpenEHRDataValues.DV_QUANTITY.equals(rmType)) {
            loadUnits(elementDefinitionCObject, currentPath);
        } else  if (elementDefinitionCObject instanceof CTerminologyCode) {
            processCTerminologyCode((CTerminologyCode) elementDefinitionCObject, currentPath);
        } else  if (OpenEHRDataValues.DV_CODED_TEXT.equals(rmType)) {
            processCodedTexts(elementDefinitionCObject, currentPath);
        } else  if (OpenEHRDataValues.DV_ORDINAL.equals(rmType)) {
            processOrdinals(elementDefinitionCObject, currentPath);
        } else  if (OpenEHRDataValues.DV_PROPORTION.equals(rmType)) {
            processProprotion(elementDefinitionCObject, currentPath);
        }
    }

    private void processCTerminologyCode(CTerminologyCode cTerminologyCode, String currentPath) throws ArchetypeProcessingException {
        List<String> acCodes = cTerminologyCode.getCodeList();
        for (String acCode: acCodes) {
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

    private void processCodedTexts(CObject cObject, String currentPath) throws ArchetypeProcessingException {
        if (cObject instanceof CComplexObject){
            CComplexObject cComplexObject = (CComplexObject) cObject;
            CObject cObjectDefinedCode = getElementDefinitionCObject(cComplexObject, currentPath, "defining_code");
            if (cObjectDefinedCode instanceof CTerminologyCode) {
                CTerminologyCode cTerminologyCode = (CTerminologyCode) cObjectDefinedCode;
                processCTerminologyCode(cTerminologyCode, currentPath);
            } else {
                processCodedTextsWithoutDefiningCode(cComplexObject, currentPath);
            }
        }
    }

    private void processCodedTextsWithoutDefiningCode(CComplexObject cComplexObject, String currentPath) throws ArchetypeProcessingException {
        Map<String, List<String>> terminologyCodesMap = getTerminologyCodesMap(cComplexObject, currentPath);
        for(Map.Entry<String, List<String>> entrySet: terminologyCodesMap.entrySet()){
            for(String code: entrySet.getValue()) {
                processCodedTextItem(entrySet.getKey(), currentPath, code);
            }
        }
    }

    private Map<String, List<String>> getTerminologyCodesMap(CComplexObject cComplexObject, String currentPath) throws ArchetypeProcessingException {
        Map<String, List<String>> terminologyCodesMap = new HashMap<String, List<String>>();
        CObject terminologyIdCObject = getElementDefinitionCObject(cComplexObject, currentPath, "terminology_id");
        CObject termIdCObject = getElementDefinitionCObject(cComplexObject, currentPath, "term_id");
        if (terminologyIdCObject instanceof CComplexObject &&
                termIdCObject instanceof CString) {
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
        CodedTextVO codedTextVO = new CodedTextVOBuilder()
                .setName(termMap.get("text"))
                .setDescription(termMap.get("description"))
                .setType(OpenEHRDataValues.DV_CODED_TEXT)
                .setIdArchetype(getArchetypeId())
                .setCode(code)
                .setTerminology(terminologyId)
                .setPath(currentPath)
                .createCodedTextVO();
        codedTextVOs.add(codedTextVO);
    }

    private void processOrdinals(CObject cObject, String currentPath) throws ArchetypeProcessingException {
        if (cObject instanceof CComplexObject){
            CObject cObjectDefinedCode = getElementDefinitionCObject((CComplexObject)cObject, currentPath, "value");
            if (cObjectDefinedCode instanceof CDvOrdinal) {
                CDvOrdinal cDvOrdinal = (CDvOrdinal) cObjectDefinedCode;
                for (DvOrdinal dvOrdinal: cDvOrdinal.getList()) {
                    CodePhrase definingCode = dvOrdinal.getSymbol().getDefiningCode();
                    String code = definingCode.getCodeString();
                    Map<String, String> referenceTerm = getTermDefinitionsArchetypeTermMap().get(code);
                    if (referenceTerm == null) {
                        logger.warn("Archetype term not found for atCode '" + code + "'");
                        continue;
                    }
                    OrdinalVO ordinalVO = new OrdinalVOBuilder()
                            .setName(referenceTerm.get("text"))
                            .setDescription(referenceTerm.get("description"))
                            .setType(cObject.getRmTypeName())
                            .setIdArchetype(getArchetypeId())
                            .setValue(dvOrdinal.getValue())
                            .setCode(code)
                            .setTerminology(definingCode.getTerminologyId().getValue())
                            .setPath(currentPath)
                            .createOrdinalVO();
                    ordinalVOs.add(ordinalVO);
                }
            }
        }
    }

    private void processClusters(CObject cObject, String path) throws ArchetypeProcessingException {
        String nodeId = cObject.getNodeId();
        Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(nodeId);
        if (termMap == null) {
            logger.warn("Archetype term not found for atCode '" + nodeId + "'");
            return;
        }
        ClusterVO clusterVO =
                new ClusterVOBuilder()
                        .setName(termMap.get("text"))
                        .setDescription(termMap.get("description"))
                        .setType(cObject.getRmTypeName())
                        .setIdArchetype(getArchetypeId())
                        .setPath(path)
                        .createClusterVO();
        setCardinalities(clusterVO, cObject);
        clusterVOs.add(clusterVO);
    }

    private static void setCardinalities(PathableVO pathableVO, CObject cObject){
        pathableVO.setLowerCardinality(cObject.getOccurrences().getLower());
        pathableVO.setUpperCardinality(cObject.getOccurrences().getUpper());
    }

    private void loadUnits(CObject cObject, String path) throws ArchetypeProcessingException {
        if (cObject instanceof CComplexObject){
            CComplexObject cComplexObject = (CComplexObject) cObject;
            List<CAttributeTuple> attributeTuples = cComplexObject.getAttributeTuples();
            for(CAttributeTuple cAttributeTuple: attributeTuples) {
                processUnits(path, cAttributeTuple);
            }
            CObject elementDefinitionCObject = getElementDefinitionCObject(cComplexObject, path, "units");
            if (elementDefinitionCObject instanceof CTerminologyCode) {
                List<String> codes = ((CTerminologyCode) elementDefinitionCObject).getCodeList();
                List<String> units = new ArrayList<String>();
                for(String code: codes){
                    Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(code);
                    if (termMap == null){
                        throw new ArchetypeProcessingException("Unknown identifier '" + code + "'");
                    }
                    units.add(termMap.get("text"));
                }
                String elementId = getArchetypeId() + path;
                processUnits(elementId, units);
            } else if (elementDefinitionCObject instanceof CComplexObject) {
                processUnitsWithoutCTerminologyCode((CComplexObject)elementDefinitionCObject, path);
            }
        }
    }

    private void processUnitsWithoutCTerminologyCode(CComplexObject cComplexObject, String path) throws ArchetypeProcessingException {
        Map<String, List<String>> terminologyCodesMap = getTerminologyCodesMap(cComplexObject, path);
        String elementId = getArchetypeId() + path;
        for(Map.Entry<String, List<String>> entrySet: terminologyCodesMap.entrySet()){
            List<String> units = new ArrayList<String>();
            for (String code: entrySet.getValue()) {
                Map<String, String> termMap = getTermDefinitionsArchetypeTermMap().get(code);
                if (termMap == null){
                    logger.warn("Archetype term not found for atCode '" + code + "'");
                    continue;
                }
                units.add(termMap.get("text"));
            }
            processUnits(elementId, units);
        }

    }

    private void processUnits(String path, CAttributeTuple cAttributeTuple) throws ArchetypeProcessingException {
        List<String> units = getUnitsCStrings(cAttributeTuple);
        String elementId = getArchetypeId() + path;
        processUnits(elementId, units);
    }

    private void processUnits( String elementId, List<String> units) {
        for (String unit: units){
            UnitVO unitVO = new UnitVO(null, elementId, unit);
            unitVOs.add(unitVO);
        }
    }

    private void processProprotion(CObject cObject, String currentPath) throws ArchetypeProcessingException {
        if (cObject instanceof CComplexObject) {
            CObject elementDefinitionCObject = getElementDefinitionCObject((CComplexObject)cObject, currentPath, "type");
            List<Integer> proportionTypes = getCIntegers(elementDefinitionCObject);
            for (Integer proportionType: proportionTypes) {
                proportionTypeVOs.add(new ProportionTypeVO(null, getArchetypeId() + currentPath, proportionType));
            }
        }
    }

    private List<String> getUnitsCStrings(CAttributeTuple cAttributeTuple) throws ArchetypeProcessingException {
        int unitsIndex = getAttributeIndex(cAttributeTuple, "units");
        List<CObject> cObjects = getCObjectsFromTuple(cAttributeTuple, unitsIndex);
        return getCStrings(cObjects);
    }

    private List<String> getCStrings(List<CObject> cObjects) throws ArchetypeProcessingException {
        List<String> cStrings = new ArrayList<String>();
        for(CObject cObject: cObjects){
            if (!(cObject instanceof CString)) {
                throw new ArchetypeProcessingException("Expecting C_STRING but found '" + cObject.getRmTypeName() + "'");
            }
            cStrings.addAll(((CString)cObject).getList());
        }
        return cStrings;
    }

    private List<Integer> getCIntegers(CObject cObject) throws ArchetypeProcessingException {
        List<Integer> cIntegers = new ArrayList<Integer>();
        if (!(cObject instanceof CInteger)) {
            throw new ArchetypeProcessingException("Expecting C_INTEGER but found '" + cObject.getRmTypeName() + "'");
        }
        cIntegers.addAll(((CInteger) cObject).getList());
        return cIntegers;
    }

    private List<CObject> getCObjectsFromTuple(CAttributeTuple cAttributeTuple, int index) {
        List<CObject> cObjects = new ArrayList<CObject>();
        for (CObjectTuple cObjectTuple: cAttributeTuple.getChildren()){
            cObjects.add(cObjectTuple.getMembers().get(index));
        }
        return cObjects;
    }

    private Integer getAttributeIndex(CAttributeTuple cAttributeTuple, String attributeRMName) {
        Integer i = 0;
        for(CAttribute cSingleAttribute: cAttributeTuple.getMembers()){
            if (cSingleAttribute.getRmAttributeName().equals(attributeRMName)){
                return i;
            }
            i++;
        }
        return null;
    }

    private CObject getElementDefinitionCObject(CComplexObject cComplexObject, String path, String rmName) throws ArchetypeProcessingException {
        for(CAttribute cAttribute: cComplexObject.getAttributes()) {
            if (cAttribute.getRmAttributeName().equals(rmName)) {
                if (cAttribute.getChildren().isEmpty()) {
                    throw new ArchetypeProcessingException("Could not find cComplexObject for element at path '" + path + "'");
                }
                return cAttribute.getChildren().iterator().next();
            }
        }
        return null;
    }

    private void processAttribute(CAttribute cAttribute, String path) throws ArchetypeProcessingException {
        for(CObject cObject: cAttribute.getChildren()) {
            processCObject(cObject, path + "/" + cAttribute.getRmAttributeName());
        }
    }

    private Map<String, Map<String, String>> getTermDefinitionsArchetypeTermMap() throws ArchetypeProcessingException {
        if (termDefinitionsArchetypeTermMap == null) {
            ArchetypeTermMapGenerator archetypeTermMapGenerator = new ArchetypeTermMapGenerator(ar.getOntology().getTermDefinitions(), language);
            termDefinitionsArchetypeTermMap = archetypeTermMapGenerator.generateTermDefinitionsArchetypeTermMap();
        }
        return termDefinitionsArchetypeTermMap;
    }

    private Map<String, List<String>> getValueSetsMap(){
        if (valueSetsMap == null) {
            valueSetsMap = generateValueSetsMap();
        }
        return valueSetsMap;
    }

    private Map<String, List<String>> generateValueSetsMap() {
        Map valueSetMap = new HashMap<String, List<String>>();
        for(ValueSetItem valueSetItem : ar.getOntology().getValueSets()){
            valueSetMap.put(valueSetItem.getId(), valueSetItem.getMembers());
        }
        return valueSetMap;
    }

    private boolean hasCardinalityZero(CComplexObject cComplexObject) {
        return (cComplexObject.getOccurrences() != null) &&
                (cComplexObject.getOccurrences().getUpper() != null) &&
                (cComplexObject.getOccurrences().getUpper() <= 0);
    }

    private String translateCIMIRM(String rmName){
        if ("PLAIN_TEXT".equals(rmName) || "TEXT".equals(rmName)){
            return OpenEHRDataValues.DV_TEXT;
        } else if ("CODED_TEXT".equals(rmName)){
            return OpenEHRDataValues.DV_CODED_TEXT;
        } else if ("ORDINAL".equals(rmName)){
            return OpenEHRDataValues.DV_ORDINAL;
        } else if ("QUANTITY".equals(rmName)){
            return OpenEHRDataValues.DV_QUANTITY;
        } else if ("YESNO".equals(rmName)){
            return OpenEHRDataValues.DV_BOOLEAN;
        } else if ("COUNT".equals(rmName)){
            return OpenEHRDataValues.DV_COUNT;
        } else if ("DURATION".equals(rmName)){
            return OpenEHRDataValues.DV_DURATION;
        } else if ("DATE_TIME".equals(rmName)){
            return OpenEHRDataValues.DV_DATE_TIME;
        } else if ("DATE".equals(rmName)){
            return OpenEHRDataValues.DV_DATE;
        } else if ("TIME".equals(rmName)){
            return OpenEHRDataValues.DV_TIME;
        } else if ("PROPORTION".equals(rmName)){
            return OpenEHRDataValues.DV_PROPORTION;
        } else if ("IDENTIFIER".equals(rmName)){
            return OpenEHRDataValues.DV_IDENTIFIER;
        } else {
            return rmName;
        }

    }
}

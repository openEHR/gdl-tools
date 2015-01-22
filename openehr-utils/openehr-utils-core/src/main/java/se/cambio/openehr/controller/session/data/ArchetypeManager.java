package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.archetype.vo.CodedTextVO;
import se.cambio.cm.model.archetype.vo.OrdinalVO;
import se.cambio.cm.model.util.TemplateAttributeMap;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.PathUtils;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class ArchetypeManager {

    private static ArchetypeManager instance;
    private ArchetypeElements archetypeElements = null;
    private Clusters clusters = null;
    private CodedTexts codedTexts = null;
    private Ordinals ordinals = null;
    private ProportionTypes proportionTypes = null;
    private Units units = null;
    private ArchetypeTerms archetypeTerms = null;
    private Archetypes archetypes;
    private Templates templates;

    public ArchetypeManager(){
    }

    public void registerArchetypeObjectBundle(
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO,
            Archetype archetype){
        getArchetypeElements().loadArchetypeElements(archetypeObjectBundleCustomVO.getArchetypeElementVOs());
        getClusters().loadClusters(archetypeObjectBundleCustomVO.getClusterVOs());
        getCodedTexts().loadCodedTexts(archetypeObjectBundleCustomVO.getCodedTextVOs());
        getOrdinals().loadOrdinals(archetypeObjectBundleCustomVO.getOrdinalVOs());
        getUnits().loadUnits(archetypeObjectBundleCustomVO.getUnitVOs());
        getProportionTypes().loadProportionTypes(archetypeObjectBundleCustomVO.getProportionTypes());
        if (archetype != null) {
            getArchetypeTerms().loadArchetype(archetype);
        }
    }

    public Archetypes getArchetypes() {
        if (archetypes == null) {
            archetypes = new Archetypes(this);
        }
        return archetypes;
    }

    public Templates getTemplates() {
        if (templates == null) {
            templates = new Templates(this);
        }
        return templates;
    }

    public ArchetypeElements getArchetypeElements() {
        if (archetypeElements == null) {
            archetypeElements = new ArchetypeElements(this);
        }
        return archetypeElements;
    }

    public Clusters getClusters() {
        if (clusters == null) {
            clusters = new Clusters(this);
        }
        return clusters;
    }

    public CodedTexts getCodedTexts() {
        if (codedTexts == null) {
            codedTexts = new CodedTexts(this);
        }
        return codedTexts;
    }

    public Ordinals getOrdinals() {
        if (ordinals == null) {
            ordinals = new Ordinals(this);
        }
        return ordinals;
    }

    public Units getUnits() {
        if (units == null) {
            units = new Units();
        }
        return units;
    }

    public ProportionTypes getProportionTypes() {
        if (proportionTypes == null) {
            proportionTypes = new ProportionTypes();
        }
        return proportionTypes;
    }

    public ArchetypeTerms getArchetypeTerms() {
        if (archetypeTerms == null) {
            archetypeTerms = new ArchetypeTerms();
        }
        return archetypeTerms;
    }

    protected ArchetypeTerm getArchetypeTerm(String idTemplate, String idElement, String lang) {
        loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        String atCode = idElement.substring(idElement.lastIndexOf("[")+1, idElement.length()-1);
        ArchetypeTerm archetypeTem = null;
        if (idTemplate==null){
            String archetypeId = idElement.substring(0, idElement.indexOf("/"));
            archetypeTem = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
        }else{
            Collection<String> archetypeIds = new ArrayList<String>();
            try {
                archetypeIds.addAll(this.getArchetypes().getAllIdsInCache());
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
            String archetypeId = PathUtils.getLastArchetypeIdInPath(idElement, archetypeIds);
            if (archetypeId==null){
                archetypeTem =  getArchetypeTerms().getArchetypeTerm(idTemplate, lang, atCode);
            }else{
                archetypeTem = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
            }
        }
        return archetypeTem;
    }


    protected ArchetypeTerm getArchetypeTerm(String archetypeId, String idTemplate, String idElement, String atCode, String lang) {
        loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        ArchetypeTerm archetypeTem = null;
        if (idTemplate==null){
            archetypeTem = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
        }else{
            Collection<String> archetypeIds = new ArrayList<String>();
            try {
                archetypeIds.addAll(this.getArchetypes().getAllIdsInCache());
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
            archetypeId = PathUtils.getLastArchetypeIdInPath(idElement, archetypeIds);
            if (archetypeId==null){
                archetypeTem = getArchetypeTerms().getArchetypeTerm(idTemplate, lang, atCode);
            }else{
                archetypeTem = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
            }
        }
        return archetypeTem;
    }


    public void loadArchetypesAndTemplatesIfNeeded(String templateId, String idElement) {
        String archetypeId = idElement.substring(0, idElement.indexOf("/"));
        loadArchetypesIfNeeded(archetypeId);
        loadTemplateIfNeeded(templateId);
    }

    public void loadArchetypesIfNeeded(String archetypeId) {
        try {
            getArchetypes().getCMElement(archetypeId);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }

    public void loadTemplateIfNeeded(String templateId) {
        try {
            if (templateId != null) {
                getTemplates().getCMElement(templateId);
            }
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }

    public TemplateElementMap getTemplateElementMap(ArchetypeElementVO archetypeElementVO, Collection<String> elementMapIds) {
        String elementMapId = getElementMapId(archetypeElementVO.getName(), elementMapIds);
        Map<String, TemplateAttributeMap> templateAttributeMaps = getTemplateAttributeMaps(archetypeElementVO);
        return new TemplateElementMap(archetypeElementVO.getType(), archetypeElementVO.getPath(), elementMapId, templateAttributeMaps);
    }

    private Map<String, TemplateAttributeMap> getTemplateAttributeMaps(ArchetypeElementVO archetypeElementVO) {
        if (OpenEHRDataValues.DV_CODED_TEXT.equals(archetypeElementVO.getRMType())) {
            return getCodedTextsAttributeMaps(archetypeElementVO);
        }
        if (OpenEHRDataValues.DV_ORDINAL.equals(archetypeElementVO.getRMType())) {
            return getOrdinalsAttributeMaps(archetypeElementVO);
        }
        return null;
    }

    private Map<String, TemplateAttributeMap> getCodedTextsAttributeMaps(ArchetypeElementVO archetypeElementVO) {
        Collection<CodedTextVO> codedTextVOs = getCodedTexts().getCodedTextVOs(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId());
        Map<String, TemplateAttributeMap> templateAttributeMaps = new HashMap<String, TemplateAttributeMap>();
        for(CodedTextVO codedTextVO: codedTextVOs) {
            if (!"local".equals(codedTextVO.getTerminology())) {
                return null;
            }
            String attributeId = getIdentifier(codedTextVO.getName(), 0);
            TemplateAttributeMap templateAttributeMap = getCodedTextAttributeMap(attributeId, codedTextVO);
            templateAttributeMaps.put(attributeId, templateAttributeMap);
        }
        return templateAttributeMaps;
    }

    private TemplateAttributeMap getCodedTextAttributeMap(String attributeId, CodedTextVO codedTextVO) {
        return new TemplateAttributeMap(attributeId, codedTextVO.getTerminology(), codedTextVO.getCode(), null, codedTextVO.getName());
    }

    private Map<String, TemplateAttributeMap> getOrdinalsAttributeMaps(ArchetypeElementVO archetypeElementVO) {
        Collection<OrdinalVO> ordinalVOs = getOrdinals().getOrdinalVOs(archetypeElementVO.getIdTemplate(), archetypeElementVO.getId());
        Map<String, TemplateAttributeMap> templateAttributeMaps = new HashMap<String, TemplateAttributeMap>();
        for(OrdinalVO ordinalVO: ordinalVOs) {
            if (!"local".equals(ordinalVO.getTerminology())) {
                return null;
            }
            String attributeId = getIdentifier(ordinalVO.getName(), 0);
            TemplateAttributeMap templateAttributeMap = getOrdinalAttributeMap(attributeId, ordinalVO);
            templateAttributeMaps.put(attributeId, templateAttributeMap);
        }
        return templateAttributeMaps;
    }

    private TemplateAttributeMap getOrdinalAttributeMap(String attributeId, OrdinalVO ordinalVO) {
        return new TemplateAttributeMap(attributeId, ordinalVO.getTerminology(), ordinalVO.getCode(), ordinalVO.getValue(), ordinalVO.getName());
    }

    public static String getElementMapId(String name, Collection<String> elementMapIds) {
        String elementMapId;
        int i = 0;
        do {
            elementMapId = getIdentifier(name, i++);
        } while (elementMapIds.contains(elementMapId));
        elementMapIds.add(elementMapId);
        return elementMapId;
    }

    public static String getIdentifier(String str, int i) {
        StringBuilder sb = new StringBuilder();
        if(!Character.isJavaIdentifierStart(str.charAt(0))) {
            sb.append("_");
        }
        for (char c : str.toCharArray()) {
            if(!Character.isJavaIdentifierPart(c)) {
                sb.append("_");
            } else {
                sb.append(Character.toLowerCase(c));
            }
        }
        if (i>0){
            sb.append(i);
        }
        return sb.toString();
    }

    public static ArchetypeManager getInstance(){
        if (instance == null){
            instance = new ArchetypeManager();
        }
        return instance;
    }
}

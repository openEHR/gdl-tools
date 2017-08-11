package se.cambio.openehr.controller.session.data;

import org.apache.commons.lang.StringUtils;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.archetype.vo.*;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.util.TemplateAttributeMap;
import se.cambio.cm.model.util.TemplateElementMap;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.PathUtils;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.*;
import java.util.concurrent.ExecutorService;

public class ArchetypeManager {

    private ArchetypeElements archetypeElements = null;
    private Clusters clusters = null;
    private CodedTexts codedTexts = null;
    private Ordinals ordinals = null;
    private ProportionTypes proportionTypes = null;
    private Units units = null;
    private ArchetypeTerms archetypeTerms = null;
    private Archetypes archetypes;
    private Templates templates;
    private ClinicalModelsService clinicalModelsService;
    private TerminologyService terminologyService;
    private UserConfigurationManager userConfigurationManager;
    private ExecutorService executorService;

    public ArchetypeManager(
            ClinicalModelsService clinicalModelsService,
            TerminologyService terminologyService,
            UserConfigurationManager userConfigurationManager,
            ExecutorService executorService) {
        this.clinicalModelsService = clinicalModelsService;
        this.terminologyService = terminologyService;
        this.userConfigurationManager = userConfigurationManager;
        this.executorService = executorService;
    }

    public ClinicalModelsService getClinicalModelsService() {
        return clinicalModelsService;
    }

    public TerminologyService getTerminologyService() {
        return terminologyService;
    }

    public UserConfigurationManager getUserConfigurationManager() {
        return userConfigurationManager;
    }

    void registerArchetypeObjectBundle(
            String archetypeId,
            String templateId,
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO) {
        getArchetypeElements().loadArchetypeElements(archetypeId, templateId, archetypeObjectBundleCustomVO.getArchetypeElementVOs());
        getClusters().loadClusters(archetypeId, templateId, archetypeObjectBundleCustomVO.getClusterVOs());
        getCodedTexts().loadCodedTexts(archetypeId, templateId, archetypeObjectBundleCustomVO.getCodedTextVOs());
        getOrdinals().loadOrdinals(archetypeId, templateId, archetypeObjectBundleCustomVO.getOrdinalVOs());
        getUnits().loadUnits(archetypeId, templateId, archetypeObjectBundleCustomVO.getUnitVOs());
        getProportionTypes().loadProportionTypes(archetypeId, templateId, archetypeObjectBundleCustomVO.getProportionTypes());
        getArchetypeTerms().loadArchetypeTerms(archetypeId, archetypeObjectBundleCustomVO.getArchetypeTermVOs());
    }

    public Archetypes getArchetypes() {
        if (archetypes == null) {
            archetypes = new Archetypes(this, executorService);
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

    protected ArchetypeTermVO getArchetypeTerm(String idTemplate, String idElement, String lang) {
        loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        int bracketIndex = idElement.lastIndexOf("[");
        String atCode = null;
        if (bracketIndex > 0) {
            atCode = idElement.substring(bracketIndex + 1, idElement.length() - 1);
        }
        ArchetypeTermVO archetypeTerm = null;
        if (idTemplate == null) {
            int slashIndex = idElement.indexOf("/");
            if (slashIndex > 0) {
                String archetypeId = idElement.substring(0, slashIndex);
                archetypeTerm = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
            }
        } else {
            String archetypeId = PathUtils.getLastEntryArchetypeInPath(idElement);
            if (archetypeId == null) {
                archetypeId = StringUtils.substringBefore(idElement, "/");
            }
            archetypeTerm = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
        }
        return archetypeTerm;
    }


    protected ArchetypeTermVO getArchetypeTerm(String archetypeId, String idTemplate, String idElement, String atCode, String lang) {
        loadArchetypesAndTemplatesIfNeeded(idTemplate, idElement);
        ArchetypeTermVO archetypeTerm;
        if (idTemplate == null) {
            archetypeTerm = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
        } else {
            archetypeId = PathUtils.getLastEntryArchetypeInPath(idElement);
            if (archetypeId == null) {
                archetypeId = StringUtils.substringBefore(idElement, "/");
            }
            archetypeTerm = getArchetypeTerms().getArchetypeTerm(archetypeId, lang, atCode);
        }
        return archetypeTerm;
    }


    public void loadArchetypesAndTemplatesIfNeeded(String templateId, String idElement) {
        int slashIndex = idElement.indexOf("/");
        if (slashIndex > 0) {
            String archetypeId = idElement.substring(0, slashIndex);
            loadArchetypesIfNeeded(archetypeId);
            List<String> archetypeIds = PathUtils.getEntryArchetypesInPath(idElement);
            for (String archetypeIdAux : archetypeIds) {
                loadArchetypesIfNeeded(archetypeIdAux);
            }
            loadTemplateIfNeeded(templateId);
        }
    }

    public void loadArchetypesIfNeeded(String archetypeId) {
        getArchetypes().getCMElement(archetypeId);
    }

    public void loadTemplateIfNeeded(String templateId) {
        if (templateId != null) {
            getTemplates().getCMElement(templateId);
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
        Collection<String> previousHandles = new ArrayList<>();
        for (CodedTextVO codedTextVO : codedTextVOs) {
            if (!"local".equals(codedTextVO.getTerminology())) {
                return null;
            }
            String name = codedTextVO.getName();
            String attributeId = getIdentifier(name, previousHandles);
            previousHandles.add(attributeId);
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
        Collection<String> previousHandles = new ArrayList<>();
        for (OrdinalVO ordinalVO : ordinalVOs) {
            if (!"local".equals(ordinalVO.getTerminology())) {
                return null;
            }
            String name = ordinalVO.getName();
            String attributeId = getIdentifier(name, previousHandles);
            previousHandles.add(attributeId);
            TemplateAttributeMap templateAttributeMap = getOrdinalAttributeMap(attributeId, ordinalVO);
            templateAttributeMaps.put(attributeId, templateAttributeMap);
        }
        return templateAttributeMaps;
    }

    private TemplateAttributeMap getOrdinalAttributeMap(String attributeId, OrdinalVO ordinalVO) {
        return new TemplateAttributeMap(attributeId, ordinalVO.getTerminology(), ordinalVO.getCode(), ordinalVO.getValue(), ordinalVO.getName());
    }

    public static String getElementMapId(String name, Collection<String> elementMapIds) {
        return getIdentifier(name, elementMapIds);
    }

    public static String getIdentifier(String name, Collection<String> previousIds) {
        String elementMapId;
        int index = 0;
        do {
            elementMapId = getIdentifier(name, index++);
        } while (previousIds.contains(elementMapId));
        previousIds.add(elementMapId);
        return elementMapId;
    }

    public static String getIdentifier(String str, int index) {
        StringBuilder sb = new StringBuilder();
        if (!Character.isJavaIdentifierStart(str.charAt(0))) {
            sb.append("_");
        }
        for (char c : str.toCharArray()) {
            if (!Character.isJavaIdentifierPart(c)) {
                sb.append("_");
            } else {
                sb.append(Character.toLowerCase(c));
            }
        }
        if (index > 0) {
            sb.append(index);
        }
        return sb.toString();
    }
}

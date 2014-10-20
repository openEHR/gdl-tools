package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.PathUtils;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;

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
        getArchetypeElements().loadArchetypeElements(archetypeObjectBundleCustomVO.getElementVOs());
        getClusters().loadClusters(archetypeObjectBundleCustomVO.getClusterVOs());
        getCodedTexts().loadCodedTexts(archetypeObjectBundleCustomVO.getCodedTextVOs());
        getOrdinals().loadOrdinals(archetypeObjectBundleCustomVO.getOrdinalVOs());
        getUnits().loadUnits(archetypeObjectBundleCustomVO.getUnitVOs());
        getProportionTypes().loadProportionTypes(archetypeObjectBundleCustomVO.getProportionTypes());
        getArchetypeTerms().loadArchetype(archetype);
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

    public static ArchetypeManager getInstance(){
        if (instance == null){
            instance = new ArchetypeManager();
        }
        return instance;
    }
}

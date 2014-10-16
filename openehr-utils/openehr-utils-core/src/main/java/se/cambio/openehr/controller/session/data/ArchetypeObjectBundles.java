package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.PathUtils;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;

public class ArchetypeObjectBundles {

    private static ArchetypeObjectBundles instance;
    private ArchetypeElements archetypeElements = null;
    private Clusters clusters = null;
    private CodedTexts codedTexts = null;
    private Ordinals ordinals = null;
    private ProportionTypesUI proportionTypesUI = null;
    private Units units = null;
    private ArchetypeTerms archetypeTerms = null;
    private final Archetypes archetypes;

    public ArchetypeObjectBundles(Archetypes archetypes){
        this.archetypes = archetypes;
    }

    public void registerArchetypeObjectBundle(
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO,
            Archetype archetype){
        getArchetypeElements().loadArchetypeElements(archetypeObjectBundleCustomVO.getElementVOs());
        getClusters().loadClusters(archetypeObjectBundleCustomVO.getClusterVOs());
        getCodedTexts().loadCodedTexts(archetypeObjectBundleCustomVO.getCodedTextVOs());
        getOrdinals().loadOrdinals(archetypeObjectBundleCustomVO.getOrdinalVOs());
        getUnits().loadUnits(archetypeObjectBundleCustomVO.getUnitVOs());
        getProportionTypesUI().loadProportionTypes(archetypeObjectBundleCustomVO.getProportionTypes());
        getArchetypeTerms().loadArchetype(archetype);
    }

    public Archetypes getArchetypes() {
        return archetypes;
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

    public ProportionTypesUI getProportionTypesUI() {
        if (proportionTypesUI == null) {
            proportionTypesUI = new ProportionTypesUI();
        }
        return proportionTypesUI;
    }

    public ArchetypeTerms getArchetypeTerms() {
        if (archetypeTerms == null) {
            archetypeTerms = new ArchetypeTerms();
        }
        return archetypeTerms;
    }

    protected ArchetypeTerm getArchetypeTerm(String idTemplate, String idElement, String lang) {
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

}

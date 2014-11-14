package se.cambio.openehr.controller.session.data;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeOntology;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.archetype.ontology.OntologyDefinitions;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ArchetypeTerms {

    private Map<String, Map<String, Map<String, ArchetypeTerm>>> archetypeTermsMap = null;

    public ArchetypeTerms() {
    }

    public void loadArchetype(Archetype archetype){
        ArchetypeOntology ao = archetype.getOntology();
        List<OntologyDefinitions> ods = ao.getTermDefinitionsList();
        for (OntologyDefinitions od : ods){
            String lang = od.getLanguage();
            List<ArchetypeTerm> archetypeTerms = od.getDefinitions();
            for(ArchetypeTerm archetypeTerm: archetypeTerms){
                getArchetypeTermsMap(archetype.getArchetypeId().getValue(), lang).put(archetypeTerm.getCode(), archetypeTerm);
            }
        }
    }

    public ArchetypeTerm getArchetypeTerm(String archetypeId, String lang, String atCode){
        return getArchetypeTermsMap(archetypeId, lang).get(atCode);
    }

    private Map<String, ArchetypeTerm> getArchetypeTermsMap(String archetypeId, String lang){
        Map<String, ArchetypeTerm> archetypeTermMap = getArchetypeTermsMap(archetypeId).get(lang);
        if(archetypeTermMap==null){
            archetypeTermMap = new HashMap<String, ArchetypeTerm>();
            getArchetypeTermsMap(archetypeId).put(lang, archetypeTermMap);
        }
        return archetypeTermMap;
    }

    private Map<String, Map<String, ArchetypeTerm>> getArchetypeTermsMap(String archetypeId){
        Map<String, Map<String, ArchetypeTerm>> archetypeTermMap = getArchetypeTermsMap().get(archetypeId);
        if(archetypeTermMap==null){
            archetypeTermMap = new HashMap<String, Map<String, ArchetypeTerm>>();
            getArchetypeTermsMap().put(archetypeId, archetypeTermMap);
        }
        return archetypeTermMap;
    }

    private Map<String, Map<String, Map<String, ArchetypeTerm>>> getArchetypeTermsMap(){
        if(archetypeTermsMap==null){
            archetypeTermsMap = new HashMap<String, Map<String, Map<String, ArchetypeTerm>>>();
        }
        return archetypeTermsMap;
    }
}

package se.cambio.openehr.util;

import org.openehr.jaxb.am.ArchetypeTerm;
import org.openehr.jaxb.am.CodeDefinitionSet;
import org.openehr.jaxb.rm.StringDictionaryItem;
import se.cambio.openehr.util.exceptions.ArchetypeProcessingException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ArchetypeTermMapGenerator {

    private final String language;
    private List<CodeDefinitionSet> termDefinitions;

    public ArchetypeTermMapGenerator(List<CodeDefinitionSet> termDefinitions, String language) {
        super();
        this.termDefinitions = termDefinitions;
        this.language = language;
    }

    public Map<String, Map<String, String>> generateTermDefinitionsArchetypeTermMap() throws ArchetypeProcessingException {
        Map<String, Map<String, String>> archetypeTermMap = new HashMap<String, Map<String, String>>();
        List<ArchetypeTerm> archetypeTerms = getTermDefinitionsArchetypeTerms(language);
        for (ArchetypeTerm archetypeTerm : archetypeTerms) {
            Map<String, String> dictionaryItemsMap = generateStringDictionaryMap(archetypeTerm);
            archetypeTermMap.put(archetypeTerm.getCode(), dictionaryItemsMap);
        }
        return archetypeTermMap;
    }

    private Map<String, String> generateStringDictionaryMap(ArchetypeTerm archetypeTerm) {
        Map<String, String> dictionaryItemsMap = new HashMap<String, String>();
        for (StringDictionaryItem stringDictionaryItem : archetypeTerm.getItems()) {
            dictionaryItemsMap.put(stringDictionaryItem.getId(), stringDictionaryItem.getValue());
        }
        return dictionaryItemsMap;
    }

    private List<ArchetypeTerm> getTermDefinitionsArchetypeTerms(String language) throws ArchetypeProcessingException {
        for (CodeDefinitionSet termDefinition : termDefinitions) {
            if (language.equals(termDefinition.getLanguage())) {
                return termDefinition.getItems();
            }
        }
        throw new ArchetypeProcessingException("No term definitions found for language '" + language + "'");
    }
}

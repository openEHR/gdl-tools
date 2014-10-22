package se.cambio.cds.util.export.html;

import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.archetype.ontology.OntologyDefinitions;
import org.openehr.rm.common.resource.ResourceDescriptionItem;
import se.cambio.cds.util.export.html.util.ArchetypeDefinitionHTMLRenderer;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ArchetypeHTMLExporter extends ClinicalModelHTMLExporter<Archetype> {

    private HashMap<String, ArchetypeTerm> archetypeTermMap;
    private ArchetypeDefinitionHTMLRenderer archetypeDefinitionHTMLRenderer;
    private String templateId;

    public ArchetypeHTMLExporter(Archetype entity, String templateId, String lang, ArchetypeManager archetypeManager) {
        super(entity, lang);
        this.templateId = templateId;
        this.archetypeDefinitionHTMLRenderer = new ArchetypeDefinitionHTMLRenderer(archetypeManager);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException, InstanceNotFoundException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("archetype", getEntity());
        objectMap.put("archetype_description_item", getResourceDescriptionItem());
        objectMap.put("archetype_ontology_definitions", getOntologyDefinitions());
        objectMap.put("getATCodeText", new GetATCodeText());
        objectMap.put("archetype_definition", archetypeDefinitionHTMLRenderer.generateHTML(getEntity(), templateId, getLanguage()));
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "ArchetypeDetails");
        addText(textsMap, "ArchetypeDefinition");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return ArchetypeHTMLExporter.class.getClassLoader().getResourceAsStream("archetype.ftl");
    }

    private ResourceDescriptionItem getResourceDescriptionItem() {
        if (getEntity().getDescription()==null){
            return null; //Template
        }
        List<ResourceDescriptionItem> resourceDescriptionItems = getEntity().getDescription().getDetails();
        for (ResourceDescriptionItem resourceDescriptionItem: resourceDescriptionItems){
            if (getLanguage().equals(resourceDescriptionItem.getLanguage().getCodeString())){
                return resourceDescriptionItem;
            }
        }
        Logger.getLogger(ArchetypeHTMLExporter.class).warn("ResourceDescriptionItem not found for language '"+getLanguage()+"' using default.");
        return resourceDescriptionItems.get(0);
    }

    private OntologyDefinitions getOntologyDefinitions() {
        List<OntologyDefinitions> termDefinitions = getEntity().getOntology().getTermDefinitionsList();
        for (OntologyDefinitions ontologyDefinition: termDefinitions){
            if (getLanguage().equals(ontologyDefinition.getLanguage())){
                return ontologyDefinition;
            }
        }
        Logger.getLogger(ArchetypeHTMLExporter.class).warn("OntologyDefinitions not found for language '"+getLanguage()+"' using default.");
        return termDefinitions.get(0);
    }

    public class GetATCodeText implements TemplateMethodModelEx {
        public TemplateModel exec(List args) throws TemplateModelException {
            if (args.size() != 1 || !(args.get(0) instanceof SimpleScalar)) {
                throw new TemplateModelException("Wrong arguments");
            }
            String atCode = ((SimpleScalar)args.get(0)).getAsString();
            try {
                return new SimpleScalar(getATCodeText(atCode));
            } catch (InstanceNotFoundException e) {
                ExceptionHandler.handle(e);
                return null;
            }
        }
    }

    private ArchetypeTerm getATCodeArchetypeTerm(String atCode) throws InstanceNotFoundException {
        ArchetypeTerm archetypeTerm = getArchetypeTermMap().get(atCode);
        if (archetypeTerm==null){
            throw new InstanceNotFoundException(atCode, ArchetypeTerm.class.getName());
        }
        return archetypeTerm;
    }

    private String getATCodeText(String atCode) throws InstanceNotFoundException {
        ArchetypeTerm archetypeTerm = getATCodeArchetypeTerm(atCode);
        return archetypeTerm.getText();
    }

    private String getATCodeDescription(String atCode) throws InstanceNotFoundException {
        ArchetypeTerm archetypeTerm = getATCodeArchetypeTerm(atCode);
        return archetypeTerm.getDescription();
    }

    private ArchetypeDefinitionHTMLRenderer getArchetypeDefinitionHTMLRenderer() {
        return archetypeDefinitionHTMLRenderer;
    }

    private Map<String, ArchetypeTerm> getArchetypeTermMap(){
        if (archetypeTermMap==null){
            archetypeTermMap = new HashMap<String, ArchetypeTerm>();
            for(ArchetypeTerm archetypeTerm: getOntologyDefinitions().getDefinitions()){
                archetypeTermMap.put(archetypeTerm.getCode(), archetypeTerm);
            }
        }
        return archetypeTermMap;
    }

    public String getTemplateId() {
        return templateId;
    }

    public String getIconPath() {
        return getArchetypeDefinitionHTMLRenderer().getIconPath();
    }

    public void setIconPath(String iconPath) {
        getArchetypeDefinitionHTMLRenderer().setIconPath(iconPath);
    }
}

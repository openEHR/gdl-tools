package se.cambio.cds.util.export.html;

import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.util.export.html.util.ArchetypeDataDefinitionHTMLRenderer;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-06
 * Time: 11:30
 */
public class KBInstanceHTMLExporter extends ClinicalModelHTMLExporter<KBInstance> {

    private ArchetypeDataDefinitionHTMLRenderer archetypeDataDefinitionHTMLRenderer;

    public KBInstanceHTMLExporter(KBInstance entity, String lang) {
        super(entity, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("kbInstance", getEntity());
        objectMap.put("kbInstance_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("kbInstance_definitions", getEntity().getKbInstanceDefinitions().get(getLanguage()));
        if (getEntity().getLocatable()!=null) {
            String archetypeId = getEntity().getLocatable().getArchetypeDetails().getArchetypeId().getValue();
            String templateId = null;
            if (getEntity().getLocatable().getArchetypeDetails().getTemplateId()!=null) {
                templateId = getEntity().getLocatable().getArchetypeDetails().getTemplateId().getValue();
            }
            objectMap.put("kbInstance_data", getArchetypeDataDefinitionHTMLRenderer().generateHTML(archetypeId, templateId, getEntity().getLocatable(), getLanguage()));
        }
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "KBInstanceDetails");
        addText(textsMap, "KBInstanceDefinition");
        addText(textsMap, "Data");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return KBInstanceHTMLExporter.class.getClassLoader().getResourceAsStream("kbi.ftl");
    }
    private ArchetypeDataDefinitionHTMLRenderer getArchetypeDataDefinitionHTMLRenderer() {
        if (archetypeDataDefinitionHTMLRenderer == null) {
            archetypeDataDefinitionHTMLRenderer = new ArchetypeDataDefinitionHTMLRenderer();
        }
        return archetypeDataDefinitionHTMLRenderer;
    }

    public String getIconPath() {
        return getArchetypeDataDefinitionHTMLRenderer().getIconPath();
    }

    public void setIconPath(String iconPath) {
        getArchetypeDataDefinitionHTMLRenderer().setIconPath(iconPath);
    }
}

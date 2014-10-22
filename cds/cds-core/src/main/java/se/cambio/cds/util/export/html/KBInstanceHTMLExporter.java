package se.cambio.cds.util.export.html;

import org.openehr.rm.common.archetyped.Archetyped;
import org.openehr.rm.common.archetyped.Locatable;
import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.util.export.html.util.ArchetypeDataDefinitionHTMLRenderer;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class KBInstanceHTMLExporter extends ClinicalModelHTMLExporter<KBInstance> {

    private ArchetypeDataDefinitionHTMLRenderer archetypeDataDefinitionHTMLRenderer;
    private ArchetypeManager archetypeManager;

    public KBInstanceHTMLExporter(KBInstance entity, String lang, ArchetypeManager archetypeManager) {
        super(entity, lang);
        this.archetypeManager = archetypeManager;
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException, InstanceNotFoundException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("kbInstance", getEntity());
        objectMap.put("kbInstance_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("kbInstance_definitions", getEntity().getKbInstanceDefinitions().get(getLanguage()));
        Locatable locatable = getEntity().getLocatable();
        if (locatable != null) {
            Archetyped archetypeDetails = locatable.getArchetypeDetails();
            String archetypeId = archetypeDetails.getArchetypeId().getValue();
            String templateId = null;
            if (archetypeDetails.getTemplateId()!=null) {
                templateId = archetypeDetails.getTemplateId().getValue();
            }
            objectMap.put("kbInstance_data", getArchetypeDataDefinitionHTMLRenderer().generateHTML(archetypeId, templateId, locatable, getLanguage()));
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
            archetypeDataDefinitionHTMLRenderer = new ArchetypeDataDefinitionHTMLRenderer(archetypeManager);
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

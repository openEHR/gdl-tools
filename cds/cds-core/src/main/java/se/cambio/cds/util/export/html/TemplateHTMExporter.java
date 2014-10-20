package se.cambio.cds.util.export.html;

import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class TemplateHTMExporter extends ArchetypeHTMLExporter {


    public TemplateHTMExporter(Archetype entity, String templateId, String lang, ArchetypeManager archetypeManager) {
        super(entity, templateId, lang, archetypeManager);
    }


    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException, InstanceNotFoundException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.putAll(super.getEntityObjectsMap());
        objectMap.put("templateId", getTemplateId());
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "TemplateDefinition");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return ArchetypeHTMLExporter.class.getClassLoader().getResourceAsStream("template.ftl");
    }
}

package se.cambio.cds.util.export.html;

import org.openehr.am.archetype.Archetype;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-15
 * Time: 17:06
 */
public class TemplateHTMExporter extends ArchetypeHTMLExporter {


    public TemplateHTMExporter(Archetype entity, String templateId, String lang) {
        super(entity, templateId, lang);
    }


    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
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

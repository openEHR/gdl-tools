package se.cambio.cds.util.export.html;

import se.cambio.cds.model.kb.instance.KBInstance;
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

    public KBInstanceHTMLExporter(KBInstance entity, String lang) {
        super(entity, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("kbInstance", getEntity());
        objectMap.put("kbInstance_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("kbInstance_definitions", getEntity().getKbInstanceDefinitions().get(getLanguage()));
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
}

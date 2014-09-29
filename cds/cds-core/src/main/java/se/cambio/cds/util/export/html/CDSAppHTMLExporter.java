package se.cambio.cds.util.export.html;

import se.cambio.cds.model.app.CDSApp;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class CDSAppHTMLExporter extends ClinicalModelHTMLExporter<CDSApp> {

    public CDSAppHTMLExporter(CDSApp entity, String lang) {
        super(entity, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("app", getEntity());
        objectMap.put("app_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("app_definitions", getEntity().getCdsAppDefinitions().get(getLanguage()));
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "AppDetails");
        addText(textsMap, "AppDefinition");
        addText(textsMap, "Views");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return CDSAppHTMLExporter.class.getClassLoader().getResourceAsStream("app.ftl");
    }
}

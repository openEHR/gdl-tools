package se.cambio.cds.util.export.html;

import se.cambio.cds.model.view.DecisionSupportView;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class DSViewHTMLExporter extends ClinicalModelHTMLExporter<DecisionSupportView> {

    public DSViewHTMLExporter(DecisionSupportView entity, String lang) {
        super(entity, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("dsv", getEntity());
        objectMap.put("dsv_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("dsv_definitions", getEntity().getDecisionSupportViewDefinitions().get(getLanguage()));
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "DSVDefinition");
        addText(textsMap, "AlertGuidelines");
        addText(textsMap, "ExecutionGuidelines");
        addText(textsMap, "DSViewDetails");
        addText(textsMap, "Alerts");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return DSViewHTMLExporter.class.getClassLoader().getResourceAsStream("dsview.ftl");
    }
}

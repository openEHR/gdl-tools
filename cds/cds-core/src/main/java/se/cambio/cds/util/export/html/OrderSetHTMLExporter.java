package se.cambio.cds.util.export.html;

import se.cambio.cds.model.orderset.OrderSet;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class OrderSetHTMLExporter extends ClinicalModelHTMLExporter<OrderSet> {

    public OrderSetHTMLExporter(OrderSet entity, String lang) {
        super(entity, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("orderSet", getEntity());
        objectMap.put("orderSet_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("orderSet_definitions", getEntity().getOrderSetDefinitions().get(getLanguage()));
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "OrderSetDetails");
        addText(textsMap, "OrderSetDefinition");
        addText(textsMap, "Guidelines");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return OrderSetHTMLExporter.class.getClassLoader().getResourceAsStream("orderSet.ftl");
    }
}

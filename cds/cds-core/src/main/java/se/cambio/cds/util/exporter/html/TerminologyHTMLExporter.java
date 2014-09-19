package se.cambio.cds.util.exporter.html;

import se.cambio.cds.util.exporter.html.util.TerminologyDefinitionHTMLRenderer;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-06
 * Time: 11:30
 */
public class TerminologyHTMLExporter extends ClinicalModelHTMLExporter<String> {

    private TerminologyDefinitionHTMLRenderer terminologyDefinitionHTMLRenderer;

    public TerminologyHTMLExporter(String terminologyId, String lang) {
        super(terminologyId, lang);
        this.terminologyDefinitionHTMLRenderer = new TerminologyDefinitionHTMLRenderer(terminologyId, getLanguage());
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("terminologyId", getEntity());
        objectMap.put("terminology_definition", terminologyDefinitionHTMLRenderer.generateHTML());
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "TerminologyDefinition");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return TerminologyHTMLExporter.class.getClassLoader().getResourceAsStream("terminology.ftl");
    }
}

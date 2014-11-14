package se.cambio.cds.util.export.html;

import se.cambio.cds.util.export.html.util.TerminologyDefinitionHTMLRenderer;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class TerminologyHTMLExporter extends ClinicalModelHTMLExporter<String> {

    public TerminologyHTMLExporter(ArchetypeManager archetypeManager) {
        super(archetypeManager);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        TerminologyDefinitionHTMLRenderer terminologyDefinitionHTMLRenderer =
                new TerminologyDefinitionHTMLRenderer(getEntity(), getLanguage());
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

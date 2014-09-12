package se.cambio.cds.util.exporter.html;

import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-12
 * Time: 13:31
 */
public class GuideHTMLExporter extends ClinicalModelHTMLExporter<Guide> {

    public GuideHTMLExporter(Guide guide, String lang) {
        super(guide, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Guide guide = getEntity();
        String lang = getLanguage();
        ReadableGuide readableGuide = GuideImporter.importGuide(guide, lang);
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("guide", guide);
        objectMap.put("guide_details", guide.getDescription().getDetails().get(lang));
        objectMap.put("guide_definitions", readableGuide);
        objectMap.put("guide_terms", guide.getOntology().getTermDefinitions().get(lang).getTerms());
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        HashMap<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "GuideDetails");
        addText(textsMap, "Preconditions");
        addText(textsMap, "RuleList");
        addText(textsMap, "Bindings");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return GuideHTMLExporter.class.getClassLoader().getResourceAsStream("gdl.ftl");
    }
}

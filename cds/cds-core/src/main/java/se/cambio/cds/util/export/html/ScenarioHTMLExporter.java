package se.cambio.cds.util.export.html;

import se.cambio.cds.model.scenario.Scenario;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;

public class ScenarioHTMLExporter extends ClinicalModelHTMLExporter<Scenario> {

    public ScenarioHTMLExporter(ArchetypeManager archetypeManager) {
        super(archetypeManager);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("scenario", getEntity());
        objectMap.put("scenario_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("scenario_definitions", getEntity().getScenarioDefinitions().get(getLanguage()));
        if (getEntity().getExecutionDate()!=null) {
            objectMap.put("execution_date", SimpleDateFormat.getDateTimeInstance().format(getEntity().getExecutionDate()));
        }
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "ScenarioDetails");
        addText(textsMap, "ScenarioDefinition");
        addText(textsMap, "ExecutionDate");
        addText(textsMap, "Guidelines");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return ScenarioHTMLExporter.class.getClassLoader().getResourceAsStream("scenario.ftl");
    }
}

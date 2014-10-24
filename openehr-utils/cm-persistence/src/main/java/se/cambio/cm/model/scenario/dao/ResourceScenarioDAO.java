package se.cambio.cm.model.scenario.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.scenario.dto.ScenarioDTO;

import java.util.Collections;

public class ResourceScenarioDAO extends ResourceGenericCMElementDAO<ScenarioDTO> {

    public ResourceScenarioDAO() {
        super(ScenarioDTO.class, Collections.singleton("scn"));
    }
}

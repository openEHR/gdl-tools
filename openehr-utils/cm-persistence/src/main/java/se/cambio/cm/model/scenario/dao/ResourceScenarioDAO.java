package se.cambio.cm.model.scenario.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.scenario.dto.ScenarioDTO;

public class ResourceScenarioDAO extends ResourceGenericCMElementDAO<ScenarioDTO> {

    public ResourceScenarioDAO() {
        super(ScenarioDTO.class);
    }
}

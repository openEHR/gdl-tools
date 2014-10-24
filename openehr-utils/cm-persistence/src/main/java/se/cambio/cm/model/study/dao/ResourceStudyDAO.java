package se.cambio.cm.model.study.dao;

import se.cambio.cm.model.cm.element.dao.ResourceGenericCMElementDAO;
import se.cambio.cm.model.study.dto.StudyDTO;

import java.util.Collections;

public class ResourceStudyDAO extends ResourceGenericCMElementDAO<StudyDTO> {

    public ResourceStudyDAO() {
        super(StudyDTO.class, Collections.singleton("std"));
    }
}

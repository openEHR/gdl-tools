package se.cambio.cm.model.util;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.HashMap;
import java.util.Map;

@Component
public class CMElementDAOFactory {

    private static String DAO_POSTFIX = "DAO";
    private Map<String, GenericCMElementDAO> genericCMElementDAOMap;

    @Autowired
    private ApplicationContext applicationContext;

    public <DTO extends CMElement> GenericCMElementDAO getDAO(Class<DTO> cmElementClass)
            throws InternalErrorException {
        String daoName = getDaoName(cmElementClass);
        return applicationContext.getBean(daoName, GenericCMElementDAO.class);
    }

    private Map<String, GenericCMElementDAO> getGenericCMElementDAOMap() {
        if (genericCMElementDAOMap == null) {
            genericCMElementDAOMap = new HashMap<String, GenericCMElementDAO>();
        }
        return genericCMElementDAOMap;
    }

    private String getDaoName(Class cmElementClass) {
        String simpleName = cmElementClass.getSimpleName();
        if (simpleName.endsWith("DTO")) {
            simpleName = simpleName.substring(0, simpleName.length() - 3);
        }
        return simpleName + DAO_POSTFIX;
    }
}

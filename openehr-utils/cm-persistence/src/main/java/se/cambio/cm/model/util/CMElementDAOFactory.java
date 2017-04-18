package se.cambio.cm.model.util;

import org.springframework.context.ApplicationContext;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class CMElementDAOFactory {

    private final static String DAO_POSTFIX = "DAO";

    private ApplicationContext applicationContext;

    public CMElementDAOFactory(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    public <DTO extends CMElement> GenericCMElementDAO getDAO(Class<DTO> cmElementClass)
            throws InternalErrorException {
        String daoName = getDaoName(cmElementClass);
        return applicationContext.getBean(daoName, GenericCMElementDAO.class);
    }

    private String getDaoName(Class cmElementClass) {
        String simpleName = cmElementClass.getSimpleName();
        if (simpleName.endsWith("DTO")) {
            simpleName = simpleName.substring(0, simpleName.length() - 3);
        }
        return simpleName + DAO_POSTFIX;
    }
}

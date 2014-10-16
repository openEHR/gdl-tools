package se.cambio.openehr.model.util;

import se.cambio.openehr.model.cm.element.dao.GenericCMElementDAO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.CDSConfigurationParametersManager;

import java.util.HashMap;
import java.util.Map;

public class CMElementDAOFactory {

    private static CMElementDAOFactory instance;
    private static String DAO_CLASS_PATH = "DAO/Class";
    private Map<String, GenericCMElementDAO> genericCMElementDAOMap;

    private static <DTO extends CMElement, DAO extends GenericCMElementDAO<DTO>> Class<DAO> getDAOClass(Class<DTO> cmElementClass) throws InternalErrorException {
        Class<DAO> theClass = null;
        try {
            String delegateClassName = CDSConfigurationParametersManager.getParameter(getConfigName(cmElementClass));
            theClass = (Class<DAO>)Class.forName(delegateClassName);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
        return theClass;
    }

    public <DTO extends CMElement> GenericCMElementDAO getDAO(Class<DTO> cmElementClass)
            throws InternalErrorException {
        String configName = getConfigName(cmElementClass);
        GenericCMElementDAO genericCMElementDAO = getGenericCMElementDAOMap().get(configName);
        if (genericCMElementDAO==null) {
            try {
                genericCMElementDAO = getDAOClass(cmElementClass).newInstance();
                getGenericCMElementDAOMap().put(configName, genericCMElementDAO);
            } catch (InternalErrorException e) {
                throw e;
            } catch (Exception e) {
                throw new InternalErrorException(e);
            }
        }
        return genericCMElementDAO;
    }

    private Map<String, GenericCMElementDAO> getGenericCMElementDAOMap(){
        if (genericCMElementDAOMap == null) {
            genericCMElementDAOMap = new HashMap<String, GenericCMElementDAO>();
        }
        return genericCMElementDAOMap;
    }

    private static String getConfigName(Class cmElementClass){
        String simpleName = cmElementClass.getSimpleName();
        if (simpleName.endsWith("DTO")){
            simpleName = simpleName.substring(0, simpleName.length()-3);
        }
        return "Generic"+simpleName+DAO_CLASS_PATH;
    }

    public static CMElementDAOFactory getInstance() {
        if (instance == null) {
            instance = new CMElementDAOFactory();
        }
        return instance;
    }
}

package se.cambio.cds.model.util;

import se.cambio.cds.model.cm.element.dao.GenericCMElementDAO;
import se.cambio.cds.util.misc.CDSConfigurationParametersManager;
import se.cambio.openehr.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.HashMap;
import java.util.Map;

public class CMElementDAOFactory {

    private static CMElementDAOFactory instance;
    private static String DAO_CLASS_PATH = "/DAO/Class";
    private Map<String, GenericCMElementDAO> genericCMElementDAOMap;

    private static <DTO extends CMElement, DAO extends GenericCMElementDAO<DTO>> Class<DAO> getDAOClass(Class<DTO> cmElementClass) throws InternalErrorException {
        Class<DAO> theClass = null;
        try {
            String delegateClassName = CDSConfigurationParametersManager.getParameter(cmElementClass.getName()+DAO_CLASS_PATH);
            theClass = (Class<DAO>)Class.forName(delegateClassName);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
        return theClass;
    }

    public <DTO extends CMElement>GenericCMElementDAO getDAO(Class<DTO> cmElementClass)
            throws InternalErrorException {
        GenericCMElementDAO genericCMElementDAO = getGenericCMElementDAOMap().get(cmElementClass.getName());
        if (genericCMElementDAO==null) {
            try {
                genericCMElementDAO = getDAOClass(cmElementClass).newInstance();
                getGenericCMElementDAOMap().put(cmElementClass.getName(), genericCMElementDAO);
            } catch (InternalErrorException e) {
                throw e;
            } catch (Exception e) {
                throw new InternalErrorException(e);
            }
        }
        return genericCMElementDAO;
    }

    public Map<String, GenericCMElementDAO> getGenericCMElementDAOMap(){
        if (genericCMElementDAOMap == null) {
            genericCMElementDAOMap = new HashMap<String, GenericCMElementDAO>();
        }
        return genericCMElementDAOMap;
    }

    public static CMElementDAOFactory getInstance() {
        if (instance == null) {
            instance = new CMElementDAOFactory();
        }
        return instance;
    }
}

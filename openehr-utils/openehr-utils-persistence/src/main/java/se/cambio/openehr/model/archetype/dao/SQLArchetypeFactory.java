package se.cambio.openehr.model.archetype.dao;

import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * @author icorram
 *


 */
public class SQLArchetypeFactory {

    public static SQLArchetypeDAO getDAO() throws InternalErrorException {

	try {
	    return new StandardSQLArchetypeDAO();
	} catch (Exception e) {
	    throw new InternalErrorException(e);
	}

    }
}

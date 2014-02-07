package se.cambio.openehr.model.terminology.dao;

import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * @author icorram
 *


 */
public class SQLTerminologyFactory {

    public static SQLTerminologyDAO getDAO() throws InternalErrorException {

	try {
	    return new StandardSQLTerminologyDAO();
	} catch (Exception e) {
	    throw new InternalErrorException(e);
	}

    }
}

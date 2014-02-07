package se.cambio.openehr.model.template.dao;

import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * @author icorram
 *


 */
public class SQLTemplateFactory {

	public static SQLTemplateDAO getDAO() throws InternalErrorException {
		try {
			return new StandardSQLTemplateDAO();
		} catch (Exception e) {
			throw new InternalErrorException(e);
		}
	}
}

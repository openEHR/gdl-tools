package se.cambio.cds.model.recommendation.dao;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collection;

import javax.sql.DataSource;

import se.cambio.cds.model.recommendation.dto.RecommendationDTO;
import se.cambio.cds.model.util.GlobalNames;
import se.cambio.cds.model.util.sql.DataSourceLocator;
import se.cambio.cds.model.util.sql.GeneralOperations;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.ModelException;

/**
 * @author iago.corbal
 */
public class SQLGenericRecommendationDAO implements GenericRecommendationDAO {


	private SQLRecommendationDAO dao;
	private DataSource dataSource;
	private DataSource dataSourceRR;

	public SQLGenericRecommendationDAO() throws InternalErrorException {
		dao = SQLRecommendationFactory.getDAO();
		dataSource = DataSourceLocator.getDataSource(GlobalNames.CDSS_DATA_SOURCE);
		dataSourceRR = DataSourceLocator.getDataSource(GlobalNames.CDSS_DATA_SOURCE_RR);
	}

	public RecommendationDTO search(Long idRecommendation)
	throws InternalErrorException, ModelException {
		Connection conexion = null;
		try {
			conexion = dataSource.getConnection();
			return dao.search(conexion, idRecommendation);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(conexion);
		}
	}

	public Collection<RecommendationDTO> searchUnresolved()
	throws InternalErrorException, ModelException {
		Connection conexion = null;
		try {
			conexion = dataSource.getConnection();
			return dao.searchUnresolved(conexion);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(conexion);
		}
	}

	public RecommendationDTO add(RecommendationDTO RecommendationDTO)
	throws InternalErrorException, ModelException {
		Connection conexion = null;
		try {
			conexion = dataSourceRR.getConnection();
			return dao.add(conexion,RecommendationDTO);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(conexion);
		}
	}

	public void update(RecommendationDTO RecommendationDTO)
	throws InternalErrorException, ModelException {
		Connection conexion = null;
		try {
			conexion = dataSource.getConnection();
			dao.update(conexion,RecommendationDTO);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(conexion);
		}
	}

	public void remove(Long idRecommendation)
	throws InternalErrorException, ModelException {
		Connection conexion = null;
		try {
			conexion = dataSource.getConnection();
			dao.remove(conexion, idRecommendation);
		} catch (SQLException e) {
			throw new InternalErrorException(e);
		} finally {
			GeneralOperations.closeConnection(conexion);
		}
	}
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */
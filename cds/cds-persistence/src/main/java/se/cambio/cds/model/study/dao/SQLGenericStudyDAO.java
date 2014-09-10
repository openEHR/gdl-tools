package se.cambio.cds.model.study.dao;

import se.cambio.cds.model.CDSGlobalNames;
import se.cambio.cds.model.study.dto.StudyDTO;
import se.cambio.openehr.model.util.sql.DataSourceLocator;
import se.cambio.openehr.model.util.sql.GeneralOperations;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Date;

/**
 * @author iago.corbal
 */
public class SQLGenericStudyDAO implements GenericStudyDAO {


    private SQLStudyDAO dao;
    private DataSource dataSource;

    public SQLGenericStudyDAO() throws InternalErrorException {
        dao = new StandardSQLStudyDAO();
        dataSource = DataSourceLocator.getDataSource(CDSGlobalNames.CDSS_DATA_SOURCE);
    }

    public StudyDTO searchByStudyId(String studyId)
            throws InternalErrorException, InstanceNotFoundException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            return dao.searchByStudyId(connection, studyId);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public Collection<StudyDTO> searchAll() throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            return dao.searchAll(connection);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void insert(StudyDTO studyDTO)
            throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            dao.insert(connection, studyDTO);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void update(StudyDTO studyDTO)
            throws InternalErrorException, InstanceNotFoundException  {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            dao.update(connection, studyDTO);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    public void remove(String studyId)
            throws InternalErrorException, InstanceNotFoundException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            dao.remove(connection,studyId);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
        }
    }

    @Override
    public Date getLastUpdateDate() throws InternalErrorException {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            return dao.getLastUpdateDate(connection);
        } catch (SQLException e) {
            throw new InternalErrorException(e);
        } finally {
            GeneralOperations.closeConnection(connection);
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
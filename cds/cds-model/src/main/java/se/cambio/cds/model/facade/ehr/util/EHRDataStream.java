package se.cambio.cds.model.facade.ehr.util;

import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.List;

/**
 * User: iago.corbal
 * Date: 2014-06-17
 * Time: 15:51
 */
public interface EHRDataStream {
    public EHRDataStreamStatus getStatus() throws InternalErrorException;
    public boolean hasNext(long waitTimeInMillis) throws InternalErrorException;
    public List<Object> next() throws InternalErrorException;
}

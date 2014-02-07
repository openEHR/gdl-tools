package se.cambio.openehr.util;

import org.openehr.rm.datatypes.basic.DataValue;

import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2013-11-14
 * Time: 20:46
 */
public class DataValuesGroupVO {
    private String path;
    private Map<String, DataValue> dataValueMap;

    public DataValuesGroupVO(Map<String, DataValue> dataValueMap, String path) {
        this.dataValueMap = dataValueMap;
        this.path = path;
    }

    public Map<String, DataValue> getDataValueMap() {
        return dataValueMap;
    }

    public void setDataValueMap(Map<String, DataValue> dataValueMap) {
        this.dataValueMap = dataValueMap;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }
}

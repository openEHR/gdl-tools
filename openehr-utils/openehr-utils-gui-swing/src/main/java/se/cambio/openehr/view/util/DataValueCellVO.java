package se.cambio.openehr.view.util;

import org.openehr.rm.datatypes.basic.DataValue;

/**
 * User: iago.corbal
 * Date: 2013-11-22
 * Time: 18:49
 */
public class DataValueCellVO {
    private boolean mandatory = false;
    private DataValue dv;

    public DataValueCellVO(DataValue dv, boolean mandatory) {
        this.dv = dv;
        this.mandatory = mandatory;
    }

    public DataValue getDv() {
        return dv;
    }

    public void setDv(DataValue dv) {
        this.dv = dv;
    }

    public boolean isMandatory() {
        return mandatory;
    }

    public void setMandatory(boolean mandatory) {
        this.mandatory = mandatory;
    }
}

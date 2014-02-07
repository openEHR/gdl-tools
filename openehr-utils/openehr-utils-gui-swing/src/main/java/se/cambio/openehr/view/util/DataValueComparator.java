package se.cambio.openehr.view.util;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvText;

import java.util.Comparator;

/**
 * User: Iago.Corbal
 * Date: 2013-11-17
 * Time: 13:03
 */
public class DataValueComparator implements Comparator<DataValue> {

    @Override
    public int compare(DataValue dv1, DataValue dv2) {
        if (dv1 instanceof DvText){
            return dv1.equals(dv2)?0:-1;
        }else{
            if (dv1 instanceof Comparable<?>){
                return ((Comparable)dv1).compareTo(dv2);
            }else{
                return -1;
            }
        }
    }
}

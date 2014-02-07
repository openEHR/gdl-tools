package se.cambio.openehr.view.renderers;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.view.util.FormatConverter;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-17
 * Time: 12:50
 */
public class DataValueRenderer  extends DefaultTableCellRenderer {

    private boolean _mandatory = false;

    public DataValueRenderer(boolean mandatory){
        _mandatory = mandatory;
    }

    @Override
    public void setValue(Object value) {
        if (value instanceof DataValue){
            DataValue dv = (DataValue)value;
            String dvStr = FormatConverter.getReadableValue(dv);
            setText(dvStr);
            setToolTipText(dvStr);
            //setIcon(null);
        }else{
            setText(null);
            setToolTipText(null);
            setIcon(null);
            if (_mandatory){
                setBorder(BorderFactory.createLineBorder(Color.RED));
            }
        }
    }
}

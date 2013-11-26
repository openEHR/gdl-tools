package se.cambio.openehr.view.renderers;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.view.util.DataValueCellVO;
import se.cambio.openehr.view.util.FormatConverter;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-17
 * Time: 12:50
 */
public class DataValueCellVORenderer extends DefaultTableCellRenderer {

    @Override
    public void setValue(Object value) {
        DataValueCellVO dvCellVO = null;
        DataValue dv = null;
        if (value instanceof DataValueCellVO){
            dvCellVO = (DataValueCellVO)value;
            dv = dvCellVO.getDv();
        }
        if (dv!=null){
            String dvStr = FormatConverter.getReadableValue(dv);
            setText(dvStr);
            setToolTipText(dvStr);
            //setIcon(null);
        }else{
            setText(null);
            setToolTipText(null);
            setIcon(null);
        }
        if (dvCellVO!=null && dvCellVO.isMandatory() && dv==null){
            setBorder(BorderFactory.createLineBorder(Color.RED));
        }
    }
}

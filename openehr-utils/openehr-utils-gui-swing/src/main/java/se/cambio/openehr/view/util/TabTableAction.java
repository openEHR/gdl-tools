package se.cambio.openehr.view.util;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 20:02
 */
public class TabTableAction extends AbstractAction {

    private static final long serialVersionUID = 0L;
    private Action _oldTabAction = null;
    private JTable _table = null;

    public TabTableAction(JTable table, Action oldTabAction){
        _oldTabAction = oldTabAction;
        _table = table;
    }

    public void actionPerformed(ActionEvent e){
        int row = _table.getSelectedRow();
        int col = _table.getSelectedColumn();
        if (_oldTabAction!=null){
            _oldTabAction.actionPerformed(e);
        }
        int rowCount = _table.getRowCount();
        if (row<rowCount-1){
            row++;
        }
        _table.changeSelection(row, col, false, false);
        _table.editCellAt(row, col);
    }
}

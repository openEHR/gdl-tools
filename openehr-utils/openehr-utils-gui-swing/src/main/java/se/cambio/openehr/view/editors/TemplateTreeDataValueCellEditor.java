package se.cambio.openehr.view.editors;

import se.cambio.openehr.model.archetype.vo.PathableVO;
import se.cambio.openehr.view.panels.DVGenericPanel;
import se.cambio.openehr.view.treetables.TemplateTreeTable;
import se.cambio.openehr.view.util.DVPanelFactory;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 14:14
 */
public class TemplateTreeDataValueCellEditor extends AbstractCellEditor implements TableCellEditor {

    private String _templateId = null;
    private DVGenericPanel _dvGenericPanel = null;

    public TemplateTreeDataValueCellEditor(String templateId){
        _templateId = templateId;
    }

    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        TemplateTreeTable templateTreeTable = ((TemplateTreeTable) table);
        PathableVO pathableVO = templateTreeTable.getElementAt(row);
        String elementId = pathableVO.getId();
        _dvGenericPanel = DVPanelFactory.createDVPanel(elementId, _templateId, pathableVO.getRMType(), true, true, true);
        _dvGenericPanel.setDataValue(templateTreeTable.getDataValue(elementId));
        return _dvGenericPanel;
    }

    public DVGenericPanel getDVGenericPanel(){
        return _dvGenericPanel;
    }

    public String getTemplateId(){
        return _templateId;
    }

    @Override
    public Object getCellEditorValue() {
        return null;
    }
}

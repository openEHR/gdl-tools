package se.cambio.openehr.view.editors;

import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.view.panels.DVGenericPanel;
import se.cambio.openehr.view.tables.TemplateListTable;
import se.cambio.openehr.view.util.DVPanelFactory;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 14:14
 */
public class TemplateListDataValueCellEditor extends AbstractCellEditor implements TableCellEditor {

    private String _templateId = null;
    private DVGenericPanel _dvGenericPanel = null;
    private Integer _editRow = null;

    public TemplateListDataValueCellEditor(String templateId){
        _templateId = templateId;
    }

    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        TemplateListTable templateListTable = ((TemplateListTable) table);
        ArchetypeElementVO archetypeElementVO = templateListTable.getArchetypeElementForColumn(column);
        _editRow = templateListTable.convertRowIndexToModel(row);
        _dvGenericPanel = DVPanelFactory.createDVPanel(archetypeElementVO.getId(), _templateId, archetypeElementVO.getRMType(), true, true, true);
        _dvGenericPanel.setDataValue(templateListTable.getDataValueAt(_editRow, archetypeElementVO.getId()));
        return _dvGenericPanel;
    }

    public DVGenericPanel getDVGenericPanel(){
        return _dvGenericPanel;
    }

    public String getTemplateId(){
        return _templateId;
    }

    public Integer getEditedRow(){
        return _editRow;
    }

    @Override
    public Object getCellEditorValue() {
        return null;
    }
}

package se.cambio.openehr.view.tables;

import com.rits.cloning.Cloner;
import org.jdesktop.swingx.JXTable;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.TemplateTableChangeManager;
import se.cambio.openehr.view.editors.TemplateListDataValueCellEditor;
import se.cambio.openehr.view.listeners.TemplateListTableCellEditorListener;
import se.cambio.openehr.view.renderers.DataValueRenderer;
import se.cambio.openehr.view.tables.models.TemplateListTableModel;
import se.cambio.openehr.view.util.DataValueComparator;

import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.util.Enumeration;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 20:43
 */
public class TemplateListTable extends JXTable{

    public static Color EVEN_ROW_COLOR = new Color(200, 230, 255);
    public static Color ODD_ROW_COLOR = new Color(240, 250, 255);
    public static Color SELECTED_ROW_COLOR = new Color(60, 110, 220);
    private List<DataValuesGroupVO> _originalDataValueGroups = null;


    public TemplateListTable(String templateId, List<ArchetypeElementVO> archetypeElements,  List<DataValuesGroupVO> dataValuesGroupVOs, TemplateTableChangeManager ttcm){
        super(new TemplateListTableModel(templateId, archetypeElements, dataValuesGroupVOs));
        _originalDataValueGroups = new Cloner().deepClone(dataValuesGroupVOs);
        TableCellRenderer tcr = new DataValueRenderer();
        DataValueComparator dvComparator = new DataValueComparator();
        Enumeration <TableColumn> e = getColumnModel().getColumns();
        int cCount = 0;
        while(e.hasMoreElements()){
            TemplateListDataValueCellEditor dvCellEditor = new TemplateListDataValueCellEditor(templateId);
            dvCellEditor.addCellEditorListener(new TemplateListTableCellEditorListener(dataValuesGroupVOs, ttcm));
            TableColumn tc = e.nextElement();
            tc.setCellEditor(dvCellEditor);
            tc.setCellRenderer(tcr);
            getColumnExt(cCount++).setComparator(dvComparator);
        }
        setBackground(null);
        //setBorder(new EtchedBorder(EtchedBorder.LOWERED));
    }

    public Component prepareRenderer(TableCellRenderer renderer, int row, int column){
        Component c = null;
        if (row>=getRowCount()){
            return null;
        }
        try{
            c = super.prepareRenderer(renderer, row, column);
        }catch(ArrayIndexOutOfBoundsException e){
            ExceptionHandler.handle(e);
            return null;
        }catch(IndexOutOfBoundsException e){
            ExceptionHandler.handle(e);
            return null;
        }

        int count = this.getSelectedRowCount();
        int i=0;
        int [] selectedRows = this.getSelectedRows();
        while(i<count && selectedRows[i]!=row) i++;

        if (i<count){
            c.setBackground(SELECTED_ROW_COLOR);
        } else{
            c.setBackground(row%2==0 ? EVEN_ROW_COLOR : ODD_ROW_COLOR);
        }

        /* Modified values rendering */
        String elementId = getTemplateListTableModel().getArchetypeElementForColumn(column).getId();
        if (hasChanged(row, elementId)){
            c.setFont(c.getFont().deriveFont(Font.BOLD));
        }else{
            c.setFont(c.getFont().deriveFont(Font.PLAIN));
        }

        return c;
    }

    private TemplateListTableModel getTemplateListTableModel(){
        return (TemplateListTableModel)getModel();
    }

    public DataValue getDataValueAt(int row, String elementId){
        return getTemplateListTableModel().getValueAt(row, elementId);
    }

    public ArchetypeElementVO getArchetypeElementForColumn(int columnIndex){
        return getTemplateListTableModel().getArchetypeElementForColumn(columnIndex);
    }

    public boolean hasChanged(int row, String elementId){
        int modelRow = convertRowIndexToModel(row);
        DataValuesGroupVO originalDataValuesGroup = null;
        if (_originalDataValueGroups.size()>modelRow){
            originalDataValuesGroup = _originalDataValueGroups.get(modelRow);
        }
        DataValue originalDataValue = originalDataValuesGroup!=null?originalDataValuesGroup.getDataValueMap().get(elementId):null;
        return !equals(getDataValueAt(modelRow, elementId), originalDataValue);
    }

    private static boolean equals(Object a, Object b) {
        return a == b || (a != null && a.equals(b));
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return true;
    }

    public void deleteRow(int row){
        if (_originalDataValueGroups!=null){
            _originalDataValueGroups.remove(row);
        }
        fireTableChanged();
    }

    public void fireTableChanged(){
        getTemplateListTableModel().fireTableDataChanged();
    }
}

package se.cambio.openehr.view.treetables;

import com.rits.cloning.Cloner;
import org.apache.log4j.Logger;
import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.renderer.DefaultTreeRenderer;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.model.archetype.vo.PathableVO;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.TemplateTableChangeManager;
import se.cambio.openehr.view.editors.TemplateTreeDataValueCellEditor;
import se.cambio.openehr.view.listeners.TemplateTableListener;
import se.cambio.openehr.view.listeners.TemplateTreeTableCellEditorListener;
import se.cambio.openehr.view.treetables.models.TemplateTreeTableModel;
import se.cambio.openehr.view.util.OpenEHRIconValue;
import se.cambio.openehr.view.util.OpenEHRStringValue;
import se.cambio.openehr.view.util.TabTableAction;

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 11:13
 */
public class TemplateTreeTable extends JXTreeTable implements TemplateTableChangeManager {

    public static Color EVEN_ROW_COLOR = new Color(200, 230, 255);
    public static Color ODD_ROW_COLOR = new Color(240, 250, 255);
    public static Color SELECTED_ROW_COLOR = new Color(60, 110, 220);
    private DataValuesGroupVO _originalDataValueGroups = null;
    private Collection<TemplateTableListener> templateTableListeners;

    public TemplateTreeTable(
            String archetypeId,
            String templateId,
            ClusterVO rootClusterVO,
            Map<ClusterVO, List<PathableVO>> pathableMap,
            DataValuesGroupVO dataValuesGroupVO){
        super(new TemplateTreeTableModel(archetypeId, templateId, rootClusterVO, pathableMap, dataValuesGroupVO));
        TreeCellRenderer renderer = new DefaultTreeRenderer(new OpenEHRIconValue(), new OpenEHRStringValue(), false);
        this.setTreeCellRenderer(renderer);
        this.getColumnModel().getColumn(TemplateTreeTableModel.ELEMENT_COLUMN).setPreferredWidth(150);
        this.getColumnModel().getColumn(TemplateTreeTableModel.VALUE_COLUMN).setPreferredWidth(80);
        //this.getColumnModel().getColumn(TemplateTreeTableModel.COMMENT_COLUMN).setPreferredWidth(200);

        TableColumn tableColumn = this.getColumnModel().getColumn(TemplateTreeTableModel.VALUE_COLUMN);
        TemplateTreeDataValueCellEditor dvCellEditor = new TemplateTreeDataValueCellEditor(templateId);
        dvCellEditor.addCellEditorListener(new TemplateTreeTableCellEditorListener(dataValuesGroupVO, this));
        tableColumn.setCellEditor(dvCellEditor);

        InputMap im = this.getInputMap(JTable.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        KeyStroke tab = KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0);
        // Disable the right arrow key
        KeyStroke right = KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0);
        im.put(right, "none");

        // Override the default tab behaviour
        // Tab to the next row.
        final Action oldTabAction = this.getActionMap().get(im.get(tab));
        Action tabAction = new TabTableAction(this, oldTabAction);
        this.getActionMap().put(im.get(tab), tabAction);
        this.expandAll();
        this.setBackground(null);
        //setBorder(new EtchedBorder(EtchedBorder.LOWERED));
        _originalDataValueGroups = new Cloner().deepClone(dataValuesGroupVO);
    }

    public PathableVO getElementAt(int row){
        Object obj = getValueAt(row, TemplateTreeTableModel.ELEMENT_COLUMN);
        if(obj instanceof PathableVO){
            return (PathableVO)obj;
        }else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown element of class '"+obj.getClass().getName()+"' in table!");
            return null;
        }
    }

    public DataValue getDataValueAt(int row){
        Object obj = getValueAt(row, TemplateTreeTableModel.VALUE_COLUMN);
        if (obj==null){
            return null;
        }else if(obj instanceof DataValue){
            return (DataValue)obj;
        }else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown element of class '"+obj.getClass().getName()+"' in table!");
            return null;
        }
    }

    private TemplateTreeTableModel getTemplateTreeTableModel(){
        return (TemplateTreeTableModel)getTreeTableModel();
    }

    public DataValue getDataValue(String elementId){
        return getTemplateTreeTableModel().getDataValue(elementId);
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
        PathableVO pathableVO = getElementAt(row);
        if (hasChanged(pathableVO.getId())){
            c.setFont(c.getFont().deriveFont(Font.BOLD));
        }else{
            c.setFont(c.getFont().deriveFont(Font.PLAIN));
        }

        return c;
    }

    public boolean hasChanged(String elementId){
        return !equals(getDataValue(elementId), _originalDataValueGroups.getDataValueMap().get(elementId));
    }

    private static boolean equals(Object a, Object b) {
        return a == b || (a != null && a.equals(b));
    }

    public void addTemplateTableListener(TemplateTableListener ttl){
        getTemplateTableListener().add(ttl);
    }

    private Collection<TemplateTableListener> getTemplateTableListener(){
        if(templateTableListeners==null){
            templateTableListeners= new ArrayList<TemplateTableListener>();
        }
        return templateTableListeners;
    }

    @Override
    public void notifyChange(DataValuesGroupVO dataValuesGroupVO, String elementId) {
        for (TemplateTableListener templateTableListener: getTemplateTableListener()){
            templateTableListener.dataValueChanged(dataValuesGroupVO, elementId);
        }
    }
}

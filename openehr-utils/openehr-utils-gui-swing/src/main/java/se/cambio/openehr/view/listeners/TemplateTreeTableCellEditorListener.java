package se.cambio.openehr.view.listeners;

import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.TemplateTableChangeManager;
import se.cambio.openehr.view.editors.TemplateTreeDataValueCellEditor;
import se.cambio.openehr.view.panels.DVGenericPanel;

import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 15:46
 */
public class TemplateTreeTableCellEditorListener implements CellEditorListener{

    private DataValuesGroupVO _dataValuesGroup = null;
    private TemplateTableChangeManager _ttcm;

    public TemplateTreeTableCellEditorListener(DataValuesGroupVO clusterInstance, TemplateTableChangeManager ttcm){
        _dataValuesGroup = clusterInstance;
        _ttcm = ttcm;
    }

    @Override
    public void editingStopped(ChangeEvent e) {
        Object obj = e.getSource();
        if (obj instanceof TemplateTreeDataValueCellEditor){
            DVGenericPanel dvGenericPanel = ((TemplateTreeDataValueCellEditor)obj).getDVGenericPanel();
            if (dvGenericPanel.isValidDV()){
                _dataValuesGroup.getDataValueMap().put(dvGenericPanel.getIdElement(), dvGenericPanel.getDataValue());
                _ttcm.notifyChange(_dataValuesGroup, dvGenericPanel.getIdElement());
            }
        }
    }

    @Override
    public void editingCanceled(ChangeEvent e) {
    }
}

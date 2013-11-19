package se.cambio.openehr.view.listeners;

import org.apache.log4j.Logger;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.TemplateTableChangeManager;
import se.cambio.openehr.view.editors.TemplateListDataValueCellEditor;
import se.cambio.openehr.view.panels.DVGenericPanel;

import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 15:46
 */
public class TemplateListTableCellEditorListener implements CellEditorListener{

    private List<DataValuesGroupVO> _dataValuesGroupVOs = null;
    private TemplateTableChangeManager _ttcm = null;

    public TemplateListTableCellEditorListener(List<DataValuesGroupVO> clusterInstances,TemplateTableChangeManager ttcm){
        _dataValuesGroupVOs = clusterInstances;
        _ttcm = ttcm;
    }

    @Override
    public void editingStopped(ChangeEvent e) {
        Object obj = e.getSource();
        if (obj instanceof TemplateListDataValueCellEditor){
            TemplateListDataValueCellEditor tldvce = ((TemplateListDataValueCellEditor)obj);
            DVGenericPanel dvGenericPanel = tldvce.getDVGenericPanel();
            int row = tldvce.getEditedRow();
            if (dvGenericPanel.isValidDV() && _dataValuesGroupVOs.size()>row){
                _dataValuesGroupVOs.get(row).getDataValueMap().put(dvGenericPanel.getIdElement(), dvGenericPanel.getDataValue());
                _ttcm.notifyChange(_dataValuesGroupVOs.get(row), dvGenericPanel.getIdElement());
            }
        }
    }

    @Override
    public void editingCanceled(ChangeEvent e) {
        Logger.getLogger(TemplateListTableCellEditorListener.class).warn("Edit canceled");
    }
}

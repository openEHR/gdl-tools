package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.TemplateTableChangeManager;
import se.cambio.openehr.view.listeners.TemplateTableListener;
import se.cambio.openehr.view.tables.TemplateListTable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 20:05
 */
public class TemplateListTableFormPanel extends JPanel implements TemplateTableChangeManager {

    private JPanel mainPanel;
    private String _templateId;
    private List<ArchetypeElementVO> _archetypeElements = null;
    private List<DataValuesGroupVO> _dataValuesGroupVOs = null;
    private TemplateListTable jTable;
    private JPanel buttonPanel;
    private JButton addButton;
    private JButton deleteButton;
    private String _path = null;
    private Collection<TemplateTableListener> templateTableListeners;

    public TemplateListTableFormPanel(String templateId, String path, List<ArchetypeElementVO> archetypeElements,  List<DataValuesGroupVO> clusterInstances){
        _templateId = templateId;
        _archetypeElements = archetypeElements;
        _dataValuesGroupVOs = clusterInstances;
        _path = path;
        this.setLayout(new BorderLayout());
        this.add(getButtonPanel(), BorderLayout.WEST);
        this.add(getMainPanel(), BorderLayout.CENTER);
    }

    private JPanel getButtonPanel(){
        if (buttonPanel==null){
            buttonPanel = new JPanel();
            buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
            buttonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            buttonPanel.add(getAddButton());
            buttonPanel.add(getDeleteButton());
        }
        return buttonPanel;
    }

    private JButton getAddButton() {
        if (addButton == null) {
            addButton = new JButton();
            addButton.setIcon(OpenEHRImageUtil.ADD_ICON);
            addButton.setToolTipText(OpenEHRLanguageManager.getMessage("Add"));
            addButton.setContentAreaFilled(false);
            addButton.setPreferredSize(new Dimension(16,16));
            addButton.setBorderPainted(false);
            addButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    DataValuesGroupVO dataValuesGroupVO = new DataValuesGroupVO(new HashMap<String, DataValue>(), _path);
                    _dataValuesGroupVOs.add(dataValuesGroupVO);
                    getTemplateListTable().fireTableChanged();
                    notifyAdd(dataValuesGroupVO);
                }
            });
        }
        return addButton;
    }

    private JButton getDeleteButton(){
        if(deleteButton==null){
            deleteButton = new JButton();
            deleteButton.setIcon(OpenEHRImageUtil.DELETE_ICON);
            deleteButton.setToolTipText(OpenEHRLanguageManager.getMessage("Remove"));
            deleteButton.setContentAreaFilled(false);
            deleteButton.setPreferredSize(new Dimension(16,16));
            deleteButton.setBorderPainted(false);
            deleteButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    int row = getTemplateListTable().convertRowIndexToModel(getTemplateListTable().getSelectedRow());
                    if (_dataValuesGroupVOs.size()>row){
                        DataValuesGroupVO dataValuesGroupVO = _dataValuesGroupVOs.get(row);
                        _dataValuesGroupVOs.remove(row);
                        getTemplateListTable().fireTableChanged();
                        notifyDelete(dataValuesGroupVO);
                    }
                }
            });
        }
        return deleteButton;
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(new JScrollPane(getTemplateListTable()));
        }
        return mainPanel;
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


    private void notifyDelete(DataValuesGroupVO dataValuesGroupVO){
        for (TemplateTableListener templateTableListener: getTemplateTableListener()){
            templateTableListener.dataValuesGroupRemoved(dataValuesGroupVO);
        }
    }

    private void notifyAdd(DataValuesGroupVO dataValuesGroupVO){
        for (TemplateTableListener templateTableListener: getTemplateTableListener()){
            templateTableListener.dataValuesGroupAdded(dataValuesGroupVO);
        }
    }

    public void notifyChange(DataValuesGroupVO dataValuesGroupVO, String elementId){
        for (TemplateTableListener templateTableListener: getTemplateTableListener()){
            templateTableListener.dataValueChanged(dataValuesGroupVO, elementId);
        }
    }

    public void stopEditing(){
        CellEditor ce = getTemplateListTable().getCellEditor();
        if (ce!=null){
            ce.stopCellEditing();
        }
    }

    private TemplateListTable getTemplateListTable(){
        if(jTable==null){
            jTable = new TemplateListTable(_templateId, _archetypeElements, _dataValuesGroupVOs, this);
        }
        return jTable;
    }
}

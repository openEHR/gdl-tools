package se.cambio.openehr.view.tables.models;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.DataValuesGroupVO;

import javax.swing.table.AbstractTableModel;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 20:44
 */
public class TemplateListTableModel extends AbstractTableModel{

    private String _templateId;
    private List<DataValuesGroupVO> _clusterInstances = null;
    private List<ArchetypeElementVO> _archetypeElements = null;

    public TemplateListTableModel(String templateId, List<ArchetypeElementVO> archetypeElements,  List<DataValuesGroupVO> dataValuesGroupVOs){
        super();
        _templateId = templateId;
        _clusterInstances = dataValuesGroupVOs;
        _archetypeElements = archetypeElements;
    }

    @Override
    public String getColumnName(int column) {
        return _archetypeElements.get(column).getName();
    }

    @Override
    public int getRowCount() {
        return _clusterInstances.size();
    }

    @Override
    public int getColumnCount() {
        return _archetypeElements.size();
    }

    public ArchetypeElementVO getArchetypeElementForColumn(int columnIndex){
        return _archetypeElements.get(columnIndex);
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        return getValueAt(rowIndex, getArchetypeElementForColumn(columnIndex).getId());
    }

    public DataValue getValueAt(int rowIndex, String elementId) {
        if (_clusterInstances.size()<=rowIndex){
            return null;
        } else {
            return _clusterInstances.get(rowIndex).getDataValueMap().get(elementId);
        }
    }
}

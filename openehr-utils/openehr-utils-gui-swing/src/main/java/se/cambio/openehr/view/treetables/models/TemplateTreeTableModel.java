package se.cambio.openehr.view.treetables.models;

import org.apache.log4j.Logger;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.model.archetype.vo.PathableVO;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.view.util.FormatConverter;

import java.util.List;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 11:15
 */
public class TemplateTreeTableModel extends AbstractTreeTableModel {

    private String _archetypeId;
    private String _templateId;
    private Map<ClusterVO, List<PathableVO>> _pathablesMap = null;
    private DataValuesGroupVO _dataValuesGroup = null;
    //TODO Internationalize
    public static int ELEMENT_COLUMN = 0;
    public static int VALUE_COLUMN = 1;
    public static int COMMENT_COLUMN = 2;
    private static String[] COLUMN_NAMES = new String[]{"Element", "Value", "Comment"};

    public TemplateTreeTableModel(String archetypeId, String templateId, ClusterVO rootCluster, Map<ClusterVO, List<PathableVO>> pathablesMap, DataValuesGroupVO dataValuesGroupVO){
        super(rootCluster);
        _archetypeId = archetypeId;
        _templateId = templateId;
        _pathablesMap = pathablesMap;
        _dataValuesGroup = dataValuesGroupVO;
    }

    @Override
    public int getColumnCount() {
        return COLUMN_NAMES.length; //Generated
    }

    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }

    @Override
    public Object getValueAt(Object o, int i) {
        if (o instanceof PathableVO){
            PathableVO pathableVO = (PathableVO)o;
            if (i==TemplateTreeTableModel.ELEMENT_COLUMN){
                return pathableVO;
            }else if (i==TemplateTreeTableModel.VALUE_COLUMN){
                DataValue dv = _dataValuesGroup.getDataValueMap().get(pathableVO.getId());
                if (dv!=null){
                    return FormatConverter.getReadableValue(dv);
                }else{
                    return null;
                }
            }
            return null;
        }else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown element in table!");
            return null;
        }
    }

    @Override
    public Object getChild(Object parent, int index) {
        if (parent instanceof ClusterVO){
            List<PathableVO> pathableVOs = _pathablesMap.get(parent);
            if (pathableVOs==null){
                //Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown cluster '"+((ClusterVO)parent).getId()+"' in table!");
                return null;
            }else{
                return pathableVOs.get(index);
            }
        }else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown element of class '"+parent.getClass().getName()+"' in table!");
            return null;
        }
    }

    @Override
    public int getChildCount(Object parent) {
        if (parent instanceof ClusterVO){
            List<PathableVO> pathableVOs = _pathablesMap.get(parent);
            if (pathableVOs==null){
                Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown cluster '"+((ClusterVO)parent).getId()+"' in table!");
                return 0;
            }
            return pathableVOs.size();
        }else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown element of class '"+parent.getClass().getName()+"' in table!");
            return 0;
        }
    }

    @Override
    public int getIndexOfChild(Object parent, Object child) {
        if (parent instanceof ClusterVO){
            List<PathableVO> pathableVOs = _pathablesMap.get(parent);
            if (pathableVOs==null){
                Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown cluster '"+((ClusterVO)parent).getId()+"' in table!");
                return 0;
            }
            return pathableVOs.indexOf(child);
        }else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown parent element of class '"+parent.getClass().getName()+"' in table!");
            return 0;
        }
    }

    @Override
    public boolean isLeaf(Object node) {
        if (node instanceof ClusterVO){
            return false;
        }else if (node instanceof ArchetypeElementVO){
            return true;
        } else{
            Logger.getLogger(TemplateTreeTableModel.class).warn("Unknown element in table!");
            return false;
        }
    }

    @Override
    public boolean isCellEditable(Object node, int column) {
        return node instanceof ArchetypeElementVO &&
                (column==TemplateTreeTableModel.VALUE_COLUMN || column==TemplateTreeTableModel.COMMENT_COLUMN);
    }

    public DataValue getDataValue(String elementId){
        return _dataValuesGroup.getDataValueMap().get(elementId);
    }
}
package se.cambio.openehr.view.panels;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.model.archetype.vo.PathableVO;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.view.listeners.TemplateTableListener;
import se.cambio.openehr.view.treetables.TemplateTreeTable;
import se.cambio.openehr.view.util.TemplateTableUtil;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 11:03
 */
public class TemplateTableFormPanel extends JPanel {

    private JPanel mainPanel;
    private String _archetypeId;
    private String _templateId;
    private Map<String, List<DataValuesGroupVO>> _clusterInstanceMap = null;
    boolean _showStructure = true;
    private JTabbedPane tabbedPane;
    private Collection<TemplateListTableFormPanel> _templateListTableFormPanels;
    private ArrayList<TemplateTreeTable> _templateTreeTables;


    public TemplateTableFormPanel(String archetypeId, String templateId, Map<String, List<DataValuesGroupVO>> clusterInstanceMap, boolean showStructure){
        _archetypeId = archetypeId;
        _templateId = templateId;
        _clusterInstanceMap = clusterInstanceMap;
        _showStructure = showStructure;
        this.setLayout(new BorderLayout());
        this.add(getMainPanel());
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(getJTabbedPane());
        }
        return mainPanel;
    }

    public JTabbedPane getJTabbedPane(){
        if (tabbedPane==null){
            tabbedPane = new JTabbedPane();
            Map<ClusterVO, List<PathableVO>> pathablesMap = TemplateTableUtil.generatePathableMap(_archetypeId, _templateId, _showStructure);
            List<ClusterVO> sectionClusterVOs = TemplateTableUtil.getSectionClusters(pathablesMap);
            if (!sectionClusterVOs.isEmpty()){
                for (ClusterVO sectionClusterVO : sectionClusterVOs){
                    List<DataValuesGroupVO> clusterInstances = _clusterInstanceMap.get(sectionClusterVO.getId());
                    if (clusterInstances==null){
                        clusterInstances = new ArrayList<DataValuesGroupVO>();
                        _clusterInstanceMap.put(sectionClusterVO.getId(), clusterInstances);
                    }
                    //TODO Only used for testing, use cardinality instead
                    if (sectionClusterVO.getName().toLowerCase().startsWith("list")){
                        List<ArchetypeElementVO> archetypeElementVOs = TemplateTableUtil.getArchetypeElementsInCluster(_archetypeId, _templateId, sectionClusterVO);
                        TemplateListTableFormPanel templateListTableFormPanel = new TemplateListTableFormPanel(_templateId, sectionClusterVO.getId(), archetypeElementVOs, clusterInstances);
                        getTemplateListTableFormPanels().add(templateListTableFormPanel);
                        tabbedPane.addTab(
                                sectionClusterVO.getName(),
                                OpenEHRConstUI.getIcon(OpenEHRConst.SECTION),
                                templateListTableFormPanel,
                                sectionClusterVO.getDescription());
                    }else{
                        if (clusterInstances.isEmpty()){
                            clusterInstances.add(new DataValuesGroupVO(new HashMap<String, DataValue>(), sectionClusterVO.getId()));
                        }
                        TemplateTreeTable templateTreeTable = new TemplateTreeTable(_archetypeId, _templateId, sectionClusterVO, pathablesMap, clusterInstances.get(0));
                        getTemplateTreeTables().add(templateTreeTable);
                        tabbedPane.addTab(
                                sectionClusterVO.getName(),
                                OpenEHRConstUI.getIcon(OpenEHRConst.SECTION),
                                new JScrollPane(templateTreeTable),
                                sectionClusterVO.getDescription());
                    }
                }
            }else{
                ClusterVO rootClusterVO = TemplateTableUtil.getRootCluster(pathablesMap);
                List<DataValuesGroupVO> clusterInstances = _clusterInstanceMap.get(rootClusterVO.getId());
                if (clusterInstances==null){
                    clusterInstances = new ArrayList<DataValuesGroupVO>();
                    _clusterInstanceMap.put(rootClusterVO.getId(), clusterInstances);
                }
                if (clusterInstances.isEmpty()){
                    clusterInstances.add(new DataValuesGroupVO(new HashMap<String, DataValue>(), rootClusterVO.getId()));
                }
                TemplateTreeTable templateTreeTable = new TemplateTreeTable(_archetypeId, _templateId, rootClusterVO, pathablesMap, clusterInstances.get(0));
                getTemplateTreeTables().add(templateTreeTable);
                tabbedPane.addTab(
                        rootClusterVO.getName(),
                        OpenEHRConstUI.getIcon(OpenEHRConst.SECTION),
                        new JScrollPane(templateTreeTable),
                        rootClusterVO.getDescription());
            }
        }
        return tabbedPane;
    }


    private Collection<TemplateListTableFormPanel> getTemplateListTableFormPanels(){
        if (_templateListTableFormPanels ==null){
            _templateListTableFormPanels = new ArrayList<TemplateListTableFormPanel>();
        }
        return _templateListTableFormPanels;
    }

    private Collection<TemplateTreeTable> getTemplateTreeTables(){
        if (_templateTreeTables ==null){
            _templateTreeTables = new ArrayList<TemplateTreeTable>();
        }
        return _templateTreeTables;
    }

    public void addTemplateTableListener(TemplateTableListener ttl){
        for (TemplateListTableFormPanel tltfp : getTemplateListTableFormPanels()){
            tltfp.addTemplateTableListener(ttl);
        }
        for (TemplateTreeTable ttt : getTemplateTreeTables()){
            ttt.addTemplateTableListener(ttl);
        }
    }

    public void stopEditing(){
        for (TemplateListTableFormPanel tltfp : getTemplateListTableFormPanels()){
            tltfp.stopEditing();
        }
        for (TemplateTreeTable ttt : getTemplateTreeTables()){
            CellEditor ce = ttt.getCellEditor();
            if (ce!=null){
                ce.stopCellEditing();
            }
        }
    }
}

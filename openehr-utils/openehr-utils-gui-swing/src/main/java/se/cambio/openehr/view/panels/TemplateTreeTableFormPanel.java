package se.cambio.openehr.view.panels;

import org.jdesktop.swingx.JXTreeTable;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 11:03
 */
public class TemplateTreeTableFormPanel extends JPanel{

    private JPanel mainPanel;
    private String _archetypeId;
    private String _templateId;
    private DataValuesGroupVO _clusterInstance = null;
    boolean _showTabs = true;
    boolean _showStructure = true;
    private Component visutalizationComponent = null;
    private ArrayList<TemplateTreeTable> _templateTreeTables;

    public TemplateTreeTableFormPanel(String archetypeId, String templateId, DataValuesGroupVO clusterInstance, boolean showTabs, boolean showStructure){
        _archetypeId = archetypeId;
        _templateId = templateId;
        _clusterInstance = clusterInstance;
        _showTabs = showTabs;
        _showStructure = showStructure;
        this.setLayout(new BorderLayout());
        this.add(getMainPanel());
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            mainPanel.add(getVisualizationComponent());
        }
        return mainPanel;
    }

    private Component getVisualizationComponent(){
        if (visutalizationComponent==null){
            visutalizationComponent = generateVisualizationComponent();
        }
        return visutalizationComponent;

    }

    private Component generateVisualizationComponent(){
        Map<ClusterVO, List<PathableVO>> pathablesMap = TemplateTableUtil.generatePathableMap(_archetypeId, _templateId, _showStructure);
        List<ClusterVO> sectionClusterVOs = TemplateTableUtil.getSectionClusters(pathablesMap);
        if (_showTabs && !sectionClusterVOs.isEmpty()){
            JTabbedPane tabbedPane = new JTabbedPane();
            for (ClusterVO sectionClusterVO : sectionClusterVOs){
                JXTreeTable treeTable = new TemplateTreeTable(_archetypeId, _templateId, sectionClusterVO, pathablesMap, _clusterInstance);
                tabbedPane.addTab(
                        sectionClusterVO.getName(),
                        OpenEHRConstUI.getIcon(OpenEHRConst.SECTION),
                        new JScrollPane(treeTable),
                        sectionClusterVO.getDescription());
            }
            return tabbedPane;
        }else{
            ClusterVO rootClusterVO = TemplateTableUtil.getRootCluster(pathablesMap);
            TemplateTreeTable treeTable = new TemplateTreeTable(_archetypeId, _templateId, rootClusterVO, pathablesMap, _clusterInstance);
            return new JScrollPane(treeTable);
        }
    }

    private Collection<TemplateTreeTable> getTemplateTreeTables(){
        if (_templateTreeTables ==null){
            _templateTreeTables = new ArrayList<TemplateTreeTable>();
        }
        return _templateTreeTables;
    }

    public void addTemplateTableListener(TemplateTableListener ttl){
        for (TemplateTreeTable ttt : getTemplateTreeTables()){
            ttt.addTemplateTableListener(ttl);
        }
    }
}

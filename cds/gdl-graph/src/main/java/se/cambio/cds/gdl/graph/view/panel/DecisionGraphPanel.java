package se.cambio.cds.gdl.graph.view.panel;

import org.apache.commons.lang.StringUtils;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.graph.controller.GDLSimpleDecisionGraph;
import se.cambio.cds.gdl.graph.controller.NodeExploder;
import se.cambio.cds.gdl.graph.controller.renderer.GraphRenderingException;
import se.cambio.cds.gdl.graph.model.GraphGranularity;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.util.misc.CDSLanguageManager;
import se.cambio.cds.view.swing.CDSImageUtil;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.util.Collection;

public class DecisionGraphPanel extends JPanel implements NodeExploder {

    private final String language;
    private final GDLSimpleDecisionGraph gdlSimpleDecisionGraph;
    private JPanel toolbarPanel;
    private JComboBox<GraphGranularity> granularityComboBox;
    private JPanel graphPanel;
    private JCheckBox viewArchetypeDependenciesCB;
    private JButton exportToPNGButton;

    public DecisionGraphPanel(
            Collection<Guide> guides,
            boolean showToolbar,
            String language,
            ArchetypeManager archetypeManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ElementInstanceCollectionManager elementInstanceCollectionManager) throws GraphRenderingException {
        this.gdlSimpleDecisionGraph =
                new GDLSimpleDecisionGraph(
                        guides,
                        archetypeManager,
                        GraphGranularity.GUIDE,
                        this,
                        archetypeReferencesManager,
                        elementInstanceCollectionManager);
        this.language = language;
        init(showToolbar);

    }

    private void init(boolean showToolbar) {
        this.setLayout(new BorderLayout());
        if (showToolbar) {
            this.add(getToolbarPanel(), BorderLayout.NORTH);
        }
        JScrollPane scrollPane = new JScrollPane(getGraphPanel());
        this.add(scrollPane, BorderLayout.CENTER);
        refresh();
    }


    private JPanel getToolbarPanel() {
        if (toolbarPanel == null) {
            toolbarPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            toolbarPanel.add(new JLabel(CDSLanguageManager.getMessage("Granularity") + ":"));
            toolbarPanel.add(getGranularityComboBox());
            toolbarPanel.add(getViewArchetypeDependenciesCB());
            toolbarPanel.add(getExportToPNGButton());
        }
        return toolbarPanel;
    }

    private JComboBox getGranularityComboBox() {
        if (granularityComboBox == null) {
            granularityComboBox = new JComboBox<>();
            granularityComboBox.addItem(GraphGranularity.GUIDE);
            granularityComboBox.addItem(GraphGranularity.RULE);
            granularityComboBox.addItem(GraphGranularity.RULELINE);
            if (gdlSimpleDecisionGraph.getGraphGranularity() != null) {
                granularityComboBox.setSelectedItem(gdlSimpleDecisionGraph.getGraphGranularity());
            }
            granularityComboBox.addActionListener(e -> {
                GraphGranularity granularity = (GraphGranularity) getGranularityComboBox().getSelectedItem();
                gdlSimpleDecisionGraph.setGraphGranularity(granularity);
                refresh();
            });
        }
        return granularityComboBox;
    }

    private JCheckBox getViewArchetypeDependenciesCB() {
        if (viewArchetypeDependenciesCB == null) {
            viewArchetypeDependenciesCB = new JCheckBox("Show archetype dependency"); //TODO i18n
            viewArchetypeDependenciesCB.addActionListener(e -> {
                gdlSimpleDecisionGraph.setViewArchetypeDependency(getViewArchetypeDependenciesCB().isSelected());
                refresh();
            });
        }
        return viewArchetypeDependenciesCB;
    }

    private JPanel getGraphPanel() {
        if (graphPanel == null) {
            graphPanel = new JPanel(new BorderLayout());
        }
        return graphPanel;
    }

    private void refresh() {
        getGraphPanel().removeAll();
        JComponent graphComponent = gdlSimpleDecisionGraph.generateGraphComponent(this.language);
        getGraphPanel().add(graphComponent);
        this.repaint();
        this.revalidate();
    }

    @Override
    public void explode(String label) {
        boolean isGuidelineLabel = isGuidelineLabel(label);
        if (isGuidelineLabel) {
            gdlSimpleDecisionGraph.getCustomGranularityMap().put(label, GraphGranularity.RULE);
        } else {
            if (isRuleLabel(label)) {
                gdlSimpleDecisionGraph.getCustomGranularityMap().put(label, GraphGranularity.RULELINE);
            } else {
                //Implode
                String ruleNodeLabel = getRuleNodeLabel(label);
                gdlSimpleDecisionGraph.getCustomGranularityMap().remove(ruleNodeLabel);
            }
        }
        refresh();
    }

    private String getRuleNodeLabel(String label) {
        String guideId = StringUtils.substringBefore(label, "<br/>");
        String restRuleLine = StringUtils.substringAfter(label, "<br/>");
        String ruleLabel = StringUtils.substringBefore(restRuleLine, "<br/>");
        return guideId + "<br/>" + ruleLabel;
    }

    private boolean isGuidelineLabel(String label) {
        return !label.contains("<br/>");
    }

    private boolean isRuleLabel(String label) {
        return label.indexOf("<br/>") == label.lastIndexOf("<br/>");
    }

    private JButton getExportToPNGButton() {
        if (exportToPNGButton == null) {
            exportToPNGButton = new JButton(CDSLanguageManager.getMessage("Export"), CDSImageUtil.EXPORT_ICON);
            exportToPNGButton.addActionListener(e -> {
                JFileChooser fileChooser = new JFileChooser();
                fileChooser.setFileFilter(new FileNameExtensionFilter("PNG file", "png"));
                fileChooser.setDialogTitle(CDSLanguageManager.getMessage("Export"));
                int answer = fileChooser.showSaveDialog((Component) e.getSource());
                if (answer == JFileChooser.APPROVE_OPTION) {
                    gdlSimpleDecisionGraph.generatePngGraphImage(fileChooser.getSelectedFile(), language);
                }
            });
        }
        return exportToPNGButton;
    }
}

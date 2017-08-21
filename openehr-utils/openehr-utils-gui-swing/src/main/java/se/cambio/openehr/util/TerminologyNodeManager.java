package se.cambio.openehr.util;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.util.TerminologyNodeVO;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;

import java.util.Collection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;

public class TerminologyNodeManager {

    private TerminologyService terminologyService;

    public TerminologyNodeManager(TerminologyService terminologyService) {
        this.terminologyService = terminologyService;
    }

    SelectableNode<Object> getNodeAllTerminologyCodes(String terminologyId, SelectableNode.SelectionMode selectionMode) {
        SelectableNode<Object> root =
                new SelectableNodeBuilder<>()
                        .setName(terminologyId)
                        .setSelectionMode(selectionMode)
                        .setIcon(OpenEHRImageUtil.TERMSET)
                        .createSelectableNode();
        List<TerminologyNodeVO> nodes = this.terminologyService.retrieveAll(terminologyId, OpenEHRDataValuesUI.getLanguageCodePhrase());
        if (nodes != null) {
            nodes.sort(Comparator.comparing(o -> o.getValue().getDefiningCode().getCodeString()));
            for (TerminologyNodeVO node : nodes) {
                root.add(getSelectableNodeTerminologyCodes(node, null, selectionMode));
            }
        }
        return root;
    }

    private static SelectableNode<Object> getSelectableNodeTerminologyCodes(
            TerminologyNodeVO node,
            Collection<String> selectedCodes,
            SelectableNode.SelectionMode selectionMode) {
        String code = node.getValue().getDefiningCode().getCodeString();
        String name = node.getValue().getValue() + " (" + code + ")";
        boolean selected = selectedCodes != null && selectedCodes.contains(code);
        SelectableNode.SelectionPropagationMode propagationMode =
                selectionMode.equals(SelectableNode.SelectionMode.MULTIPLE)
                        ? SelectableNode.SelectionPropagationMode.NONE : SelectableNode.SelectionPropagationMode.HIERARCHICAL;
        SelectableNode<Object> selectableNode =
                new SelectableNodeBuilder<>()
                        .setName(name)
                        .setIcon(OpenEHRImageUtil.TERMSET)
                        .setObject(node.getValue())
                        .setSelectionMode(selectionMode)
                        .setSelectionPropagationMode(propagationMode)
                        .setSelected(selected)
                        .createSelectableNode();
        for (TerminologyNodeVO nodeAux : node.getChildren()) {
            selectableNode.add(getSelectableNodeTerminologyCodes(nodeAux, selectedCodes, selectionMode));
        }
        return selectableNode;
    }

    static boolean selectCodesWith(SelectableNode<?> node, Object object, boolean multiple) {
        if (node.getObject() instanceof DvCodedText) {
            if (object.equals(((DvCodedText) node.getObject()).getDefiningCode().getCodeString())) {
                node.setSelected(true);
                node.stateChange(node);
                if (!multiple) {
                    return true;
                }
            }
        }
        Enumeration<?> list = node.getAllchildren();
        while (list.hasMoreElements()) {
            Object nodeObj = list.nextElement();
            if (nodeObj instanceof SelectableNode) {
                boolean found = selectCodesWith((SelectableNode) nodeObj, object, multiple);
                if (found && !multiple) {
                    return true;
                }
            }
        }
        return false;
    }
}

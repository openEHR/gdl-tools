package se.cambio.openehr.util;

import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.util.NodeConversor;
import se.cambio.openehr.view.util.WindowManager;

import java.awt.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


public class TerminologyDialogManager {

    private Map<String, DialogSelection> terminologySelectableNodes = null;
    private WindowManager windowManager;
    private TerminologyNodeManager terminologyNodeManager;

    public TerminologyDialogManager(WindowManager windowManager, TerminologyNodeManager terminologyNodeManager) {
        this.windowManager = windowManager;
        this.terminologyNodeManager = terminologyNodeManager;
        terminologySelectableNodes = new HashMap<>();
    }

    public DialogSelection getTerminologyDialog(Window owner, String terminologyId, SelectableNode.SelectionMode selectionMode, Collection<String> selectedCodes) {
        DialogSelection dialog = terminologySelectableNodes.get(terminologyId);
        SelectableNode<?> rootNode;
        if (dialog == null || (owner != dialog.getOwner())) {
            rootNode = terminologyNodeManager.getNodeAllTerminologyCodes(terminologyId, selectionMode);
            dialog = new DialogSelection(
                    owner,
                    terminologyId,
                    rootNode,
                    false,
                    new Dimension(500, 600), windowManager);
            dialog.setResizable(true);
            terminologySelectableNodes.put(terminologyId, dialog);
        }
        rootNode = dialog.getNode();
        NodeConversor.setAllVisible(rootNode);
        rootNode.setAllSelected(false, true); //Force cleaning all selection
        if (selectedCodes != null) {
            for (String selectedCode : selectedCodes) {
                TerminologyNodeManager.selectCodesWith(rootNode, selectedCode, false);
            }
        }
        dialog.setRootNode(rootNode);
        return dialog;
    }
}

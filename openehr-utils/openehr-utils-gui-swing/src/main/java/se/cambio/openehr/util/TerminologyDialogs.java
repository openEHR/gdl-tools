package se.cambio.openehr.util;

import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.util.NodeConversor;

import java.awt.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-01-28
 * Time: 14:36
 */
public class TerminologyDialogs {

    private static TerminologyDialogs _instance = null;
    private Map<String, DialogSelection> terminologySelectableNodes = null;

    private TerminologyDialogs(){
        terminologySelectableNodes = new HashMap<String, DialogSelection>();
    }

    public static DialogSelection getTerminologyDialog(Window owner, String terminologyId, SelectableNode.SelectionMode selectionMode, Collection<String> selectedCodes){
        DialogSelection dialog = getDelegate().terminologySelectableNodes.get(terminologyId);
        SelectableNode<?> rootNode;
        if (dialog==null || (owner != dialog.getOwner())){
            rootNode = TerminologyNodeUtil.getNodeAllTerminologyCodes(terminologyId, null, selectionMode);
            dialog = new DialogSelection(
                    owner,
                    terminologyId,
                    rootNode,
                    false,
                    new Dimension(500, 600));
            dialog.setResizable(true);
            getDelegate().terminologySelectableNodes.put(terminologyId, dialog);
        }
        rootNode = dialog.getNode();
        NodeConversor.setAllVisible(rootNode);
        rootNode.setAllSelected(false, true); //Force cleaning all selection
        if (selectedCodes!=null){
            for (String selectedCode: selectedCodes){
                TerminologyNodeUtil.selectCodesWith(rootNode, selectedCode, false);
            }
        }
        dialog.setRootNode(rootNode, false);
        return dialog;
    }

    public static TerminologyDialogs getDelegate(){
        if (_instance == null){
            _instance = new TerminologyDialogs();
        }
        return _instance;
    }
}

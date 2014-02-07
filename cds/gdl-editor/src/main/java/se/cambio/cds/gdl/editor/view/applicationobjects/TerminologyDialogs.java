package se.cambio.cds.gdl.editor.view.applicationobjects;

import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.view.util.NodeDefinitionConversor;
import se.cambio.openehr.view.dialogs.DialogSelection;
import se.cambio.openehr.view.trees.SelectableNode;

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

    public static DialogSelection getTerminologyDialog(String terminologyId, Collection<String> selectedCodes){
        DialogSelection dialog = getDelegate().terminologySelectableNodes.get(terminologyId);
        SelectableNode<?> rootNode = null;
        if (dialog==null){
            rootNode = NodeDefinitionConversor.getNodeAllTerminologyCodes(terminologyId, null);
            dialog = new DialogSelection(
                    EditorManager.getActiveEditorWindow(),
                    terminologyId,
                    rootNode,
                    false,
                    new Dimension(500, 600));
            dialog.setResizable(true);
            getDelegate().terminologySelectableNodes.put(terminologyId, dialog);
        }
        rootNode = dialog.getNode();
        rootNode.setAllSelected(false);
        if (selectedCodes!=null){
            for (String selectedCode: selectedCodes){
                NodeDefinitionConversor.selectCodesWith(rootNode, selectedCode, false);
            }
        }
        return dialog;
    }

    public static TerminologyDialogs getDelegate(){
        if (_instance == null){
            _instance = new TerminologyDialogs();
        }
        return _instance;
    }
}

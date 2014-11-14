package se.cambio.openehr.util;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.cm.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.trees.SelectableNode;
import se.cambio.openehr.view.trees.SelectableNodeBuilder;

import java.util.*;

public class TerminologyNodeUtil {

    public static SelectableNode<Object> getNodeAllTerminologyCodes(String terminologyId, Collection<String> selectedCodes, SelectableNode.SelectionMode selectionMode){
        SelectableNode<Object> root =
                new SelectableNodeBuilder<Object>()
                        .setName(terminologyId)
                        .setSelectionMode(selectionMode)
                        .setIcon(OpenEHRImageUtil.TERMSET)
                        .createSelectableNode();
        List<TerminologyNodeVO> nodes = null;
        try{
            nodes = OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveAll(terminologyId, OpenEHRDataValuesUI.getLanguageCodePhrase());
        }catch (InternalErrorException e){
            ExceptionHandler.handle(e);
        }
        if (nodes!=null){
            Collections.sort(nodes, new Comparator<TerminologyNodeVO>() {
                @Override
                public int compare(TerminologyNodeVO o1, TerminologyNodeVO o2) {
                    return o1.getValue().getDefiningCode().getCodeString().compareTo(o2.getValue().getDefiningCode().getCodeString());
                }
            });
            for (TerminologyNodeVO node : nodes) {
                root.add(getSelectableNodeTerminologyCodes(node, selectedCodes, selectionMode));
            }
        }
        return root;
    }

    public static SelectableNode<Object> getSelectableNodeTerminologyCodes(
            TerminologyNodeVO node,
            Collection <String> selectedCodes,
            SelectableNode.SelectionMode selectionMode){
        String code = node.getValue().getDefiningCode().getCodeString();
        String name = node.getValue().getValue() +" ("+code+")";
        boolean selected = selectedCodes!=null && selectedCodes.contains(code);
        SelectableNode.SelectionPropagationMode propagationMode =
                selectionMode.equals(SelectableNode.SelectionMode.MULTIPLE)?
                        SelectableNode.SelectionPropagationMode.NONE:SelectableNode.SelectionPropagationMode.HIERARCHICAL;
        SelectableNode<Object> selectableNode =
                new SelectableNodeBuilder<Object>()
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

    public static boolean selectCodesWith(SelectableNode<?> node, Object object, boolean multiple){
        if (node.getObject() instanceof DvCodedText){
            if (object.equals(((DvCodedText)node.getObject()).getDefiningCode().getCodeString())){
                node.setSelected(true);
                node.stateChange(node);
                if (!multiple){
                    return true;
                }
            }
        }
        Enumeration<?> e = node.getAllchildren();
        while(e.hasMoreElements()){
            Object nodeObj = e.nextElement();
            if (nodeObj instanceof SelectableNode){
                boolean found = selectCodesWith((SelectableNode)nodeObj, object, multiple);
                if (found && !multiple){
                    return true;
                }
            }
        }
        return false;
    }
}

package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.util.OpenEHRImageUtil;

import javax.swing.*;
import java.awt.*;


public class DialogLongMessageNotice extends DialogEditor{
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static enum MessageType{
        NORMAL,
        WARNING,
        WARNING_WITH_CANCEL,
        ERROR
    }
    private JPanel panelMsg = null;
    private JPanel panelException = null;
    private JPanel panelButtons = null;
    private String _msg = null;
    private String _longMsg = null;
    private MessageType _type = null;

    public DialogLongMessageNotice(Window owner, String title, String msg, String longMsg, MessageType type) {
        super(owner, title, new Dimension(400, 400), true, true);
        _msg = msg;
        _longMsg = longMsg;
        _type = type;
        init();
    }

    private void init(){
        getJPanel().setLayout(new BorderLayout());
        getJPanel().add(getPanelMsg(), BorderLayout.NORTH);
        getJPanel().add(getPanelException(), BorderLayout.CENTER);
        getJPanel().add(getPanelButtons(), BorderLayout.SOUTH);
    }


    private JPanel getPanelMsg(){
        if(panelMsg==null){
            panelMsg = new JPanel(new BorderLayout());
            JPanel aux = new JPanel();
            aux.add(new JLabel(getImageIcon()));
            panelMsg.add(aux, BorderLayout.WEST);
            JTextPane textPane = new JTextPane();
            textPane.setText(_msg);
            textPane.setFont(textPane.getFont().deriveFont(Font.BOLD));
            textPane.setBackground(null);
            textPane.setEditable(false);
            panelMsg.add(textPane, BorderLayout.CENTER);
        }
        return panelMsg;
    }

    private JPanel getPanelException(){
        if(panelException==null){
            panelException = new JPanel();
            panelException = new JPanel(new BorderLayout());
            JScrollPane scrollPane = new JScrollPane();
            JTextPane textPane = new JTextPane();
            textPane.setText(_longMsg);
            textPane.setEditable(false);
            JPanel noWrapPanel = new JPanel( new BorderLayout() );
            noWrapPanel.add(textPane);
            textPane.setBackground(null);
            scrollPane.setViewportView(noWrapPanel);
            panelException.add(scrollPane);
        }
        return panelException;
    }

    private JPanel getPanelButtons(){
        if(panelButtons==null){
            panelButtons = new JPanel(new FlowLayout(FlowLayout.CENTER));
            panelButtons.add(getAcceptButton());
            if (MessageType.WARNING_WITH_CANCEL.equals(_type)){
                panelButtons.add(getCancelButton());
            }
        }
        return panelButtons;
    }

    private ImageIcon getImageIcon(){
        if (MessageType.ERROR.equals(_type)){
            return OpenEHRImageUtil.CANCEL_ICON;
        }else if (MessageType.WARNING.equals(_type)|| MessageType.WARNING_WITH_CANCEL.equals(_type)){
            return OpenEHRImageUtil.WARNING_ICON;
        }else{
            return OpenEHRImageUtil.EMPTY_ICON;
        }
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */
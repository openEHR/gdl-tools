package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.panels.interfaces.ClosableTabbebPane;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

public class BindingsPanel extends JPanel implements RefreshablePanel, ClosableTabbebPane{
    private static final long serialVersionUID = 8349466066595869612L;
    private GDLEditor _controller;
    private JTabbedPane termsTabPane = null;
    private ArrayList<BindingPanel> bindingPanels = new ArrayList<BindingPanel>();

    public BindingsPanel(GDLEditor controller){
        _controller = controller;
        init();
    }

    /**
     * This method initializes this
     */
    private void init() {
        this.setLayout(new BorderLayout());
        this.setFocusable(true);
        refresh();
    }

    public void refresh(){
        this.removeAll();
        termsTabPane = null;
        bindingPanels.clear();
        if (_controller.getTermBindings().isEmpty()){
            this.setBorder(new EmptyBorder(10, 10, 10, 10));
            this.add(new JLabel(GDLEditorLanguageManager.getMessage("NoBindingsYetUseAddBindingButtonMsg")), BorderLayout.NORTH);
        }else{
            this.setBorder(null);
            this.add(getTabbedPane(), BorderLayout.CENTER);
        }
        this.repaint();
        this.validate();
    }

    public JTabbedPane getTabbedPane(){
        if (termsTabPane == null){
            termsTabPane = new JTabbedPane();
            termsTabPane.setTabLayoutPolicy(JTabbedPane.WRAP_TAB_LAYOUT);

            Set<String> bindingsCodes = _controller.getTermBindings().keySet();
            if (bindingsCodes != null && bindingsCodes.size() > 0) {
                int i = 0;
                for (String tabName: bindingsCodes) {
                    termsTabPane.addTab(tabName, null,getNewBindingPanel(tabName));
                    termsTabPane.setTabComponentAt(i, new ButtonTabComponent(this, GDLEditorLanguageManager.getMessage("DeleteTerminologyDesc")));
                    i++;
                }
            }
        }
        return termsTabPane;
    }

    private BindingPanel getNewBindingPanel(String tabId){
        BindingPanel newBindingPanel = new BindingPanel(_controller, tabId);
        bindingPanels.add(newBindingPanel);
        return newBindingPanel;
    }

    public void addTermTab() {
        String terminologyId = _controller.createNewTerminologyBinding();
        if (terminologyId!=null){
            refresh();
            Iterator<BindingPanel> i = bindingPanels.iterator();
            while(i.hasNext()){
                BindingPanel bp = i.next();
                if (terminologyId.equals(bp.getOwnerTabName())){
                    getTabbedPane().setSelectedComponent(bp);
                }
            }
        }
    }

    public void deleteTab(int index) {
        if (index > -1) {
            String titleRef = getTabbedPane().getTitleAt(index);
            int selection = JOptionPane.showConfirmDialog(this,
                    GDLEditorLanguageManager.getMessage("DeleteTerminologyBindingMessage",titleRef),
                    GDLEditorLanguageManager.getMessage("DeleteTermPopupTitle"),
                    JOptionPane.YES_NO_OPTION);
            if (selection == JOptionPane.YES_OPTION) {
                getTabbedPane().remove(index);
                if (index > 0) {
                    getTabbedPane().setSelectedIndex(index - 1);
                } else if (index == 0 && getTabbedPane().getTabCount() > 1) {
                    getTabbedPane().setSelectedIndex(index);
                } else if (getTabbedPane().getTabCount() == 0) {
                    //hidePanel(false);
                }
                removeReference(titleRef);
            }
        }
    }


    private void removeReference(String removeRef){
        _controller.getTermBindings().remove(removeRef);
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
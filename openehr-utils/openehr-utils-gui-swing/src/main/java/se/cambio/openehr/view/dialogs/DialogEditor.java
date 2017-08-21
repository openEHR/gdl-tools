package se.cambio.openehr.view.dialogs;

import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


public abstract class DialogEditor extends JDialog {

    private static final long serialVersionUID = 1L;
    private AcceptChangesAction acceptChangesAction = null;
    private CancelChangesAction cancelChangesAction = null;
    private JPanel jPanel = null;
    private boolean answer = false;
    private JComponent componentWithFirstFocus = null;
    private JButton acceptButton = null;
    private JButton cancelButton = null;


    public DialogEditor(Window owner, String title, Dimension size, boolean modal) {
        super(owner, title, modal ? ModalityType.APPLICATION_MODAL : ModalityType.MODELESS);
        init(size);
    }

    public DialogEditor(Window owner, String titulo, Dimension size, boolean modal, boolean resizable) {
        super(owner, titulo, modal ? ModalityType.APPLICATION_MODAL : ModalityType.MODELESS);
        init(size);
        this.setResizable(resizable);
    }

    public DialogEditor(Window owner, String titulo, Dimension size) {
        super(owner, titulo);
        init(size);
    }

    private void init(Dimension size) {
        this.setSize(size);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setResizable(false);
        this.addWindowListener(getCancelChangesAction());
        this.setContentPane(getJPanel());
        /* Enter KeyStroke */
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true);
        getJPanel().registerKeyboardAction(getAcceptChangesAction(), enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        KeyStroke esc = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true);
        getJPanel().registerKeyboardAction(getCancelChangesAction(), esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
    }

    protected JPanel getJPanel() {
        if (jPanel == null) {
            jPanel = new JPanel();
        }
        return jPanel;
    }

    protected AcceptChangesAction getAcceptChangesAction() {
        if (acceptChangesAction == null) {
            acceptChangesAction = new AcceptChangesAction();
        }
        return acceptChangesAction;
    }

    protected CancelChangesAction getCancelChangesAction() {
        if (cancelChangesAction == null) {
            cancelChangesAction = new CancelChangesAction();
        }
        return cancelChangesAction;
    }

    protected class CancelChangesAction extends WindowAdapter implements ActionListener {

        public void windowOpened(WindowEvent ex) {
            if (componentWithFirstFocus != null) {
                componentWithFirstFocus.requestFocus();
            }
        }

        public void actionPerformed(ActionEvent ex) {
            exit();
        }

        public void windowClosing(WindowEvent we) {
            exit();
        }
    }

    public class AcceptChangesAction extends AbstractAction {

        private static final long serialVersionUID = -8058749276509227718L;

        public void actionPerformed(ActionEvent ex) {
            accept();
        }
    }

    protected JButton getAcceptButton() {
        if (acceptButton == null) {
            acceptButton = new JButton();
            acceptButton.setText(OpenEHRLanguageManager.getMessage("Accept"));
            acceptButton.setIcon(OpenEHRImageUtil.ACCEPT_ICON);
            acceptButton.setEnabled(true);
            acceptButton.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            acceptButton.addActionListener(getAcceptChangesAction());
        }
        return acceptButton;
    }

    protected JButton getCancelButton() {
        if (cancelButton == null) {
            cancelButton = new JButton();
            cancelButton.setText(OpenEHRLanguageManager.getMessage("Cancel"));
            cancelButton.setIcon(OpenEHRImageUtil.CANCEL_ICON);
            cancelButton.setEnabled(true);
            cancelButton.addActionListener(getCancelChangesAction());
        }
        return cancelButton;
    }

    public final void accept() {
        if (acceptDialog()) {
            answer = true;
            setVisible(false);
        }
    }

    public final void exit() {
        if (cancelDialog()) {
            answer = false;
            setVisible(false);
        }
    }

    public final boolean getAnswer() {
        return answer;
    }

    protected void registerComponentWithFirstFocus(JComponent componentWithFirstFocus) {
        this.componentWithFirstFocus = componentWithFirstFocus;
    }

    protected final void setAnswer(boolean answer) {
        this.answer = answer;
    }

    protected boolean cancelDialog() {
        return true;
    }

    protected boolean acceptDialog() {
        return true;
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
package se.cambio.openehr.view.dialogs;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.util.*;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.StyledEditorKit;
import java.awt.*;
import java.awt.event.*;

public class DialogEditorNullValue  extends JDialog {
    private static final long serialVersionUID = 1L;
    private AceptarCambiosAction acceptChangesAction = null;
    private CancelChangesAction cancelChangesAction = null;
    private boolean _respuesta = false;
    private JComponent _componentWithFirstFocus = null;
    private JPanel mainPanel = null;
    private JPanel bottonPanel;
    private JButton acceptButton;
    private JButton cancelButton;
    private JTextPane jTextPane;
    private JComboBox comboBox;

    public DialogEditorNullValue(Window owner){
        super(owner, OpenEHRLanguageManager.getMessage("NullValue"), ModalityType.APPLICATION_MODAL);
        init(new Dimension(400,150));
    }

    private void init(Dimension size){
        this.setSize(size);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setResizable(false);
        this.addWindowListener(getCancelChangesAction());
        this.setContentPane(getMainPanel());
	/* Enter KeyStroke */
        KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
        getComboBox().registerKeyboardAction(getAcceptChangesAction(), enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        KeyStroke esc = KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE,0,true);
        getComboBox().registerKeyboardAction(getCancelChangesAction(), esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            JPanel panelAux1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
            JLabel label = new JLabel(OpenEHRLanguageManager.getMessage("NullValue")+":");
            label.setIcon(OpenEHRDataValuesUI.getIcon(OpenEHRDataValues.DV_CODED_TEXT));
            panelAux1.add(label);
            panelAux1.add(getComboBox());
            JPanel panelAux2 = new JPanel(new BorderLayout());
            panelAux2.add(getTextPane(), BorderLayout.CENTER);
            panelAux2.add(panelAux1, BorderLayout.SOUTH);
            mainPanel.add(panelAux2,BorderLayout.CENTER);
            mainPanel.add(getBottonPanel(),BorderLayout.SOUTH);
        }
        return mainPanel;
    }

    private JTextPane getTextPane(){
        if (jTextPane == null){
            jTextPane = new JTextPane();
            StyledEditorKit m_kit = new StyledEditorKit();
            jTextPane.setEditorKit(m_kit);
            jTextPane.getDocument().putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");
            jTextPane.setFont(new java.awt.Font("Dialog", java.awt.Font.BOLD, 12));
            jTextPane.setText(OpenEHRLanguageManager.getMessage("NullValueDesc"));
            jTextPane.setEditable(false);
            jTextPane.setBackground(null);
            jTextPane.setPreferredSize(new java.awt.Dimension(250,70));
        }
        return jTextPane;
    }

    public void setNullValue(DvCodedText codedTextDV){
        if (codedTextDV!=null){
            getComboBox().setSelectedItem(codedTextDV.getDefiningCode().getCodeString());
        }
    }

    public DvCodedText getSelectedNullValue(){
        return OpenEHRConstUI.NULL_FLAVOUR_MAP.get(getComboBox().getSelectedItem());
    }

    protected JComboBox getComboBox(){
        if (comboBox==null){
            comboBox = new JComboBox();
            comboBox.setRenderer(new DVComboBoxRendered());
            for (String nullFlavourCode : OpenEHRConstUI.NULL_FLAVOUR_MAP.keySet()) {
                comboBox.addItem(nullFlavourCode);
            }
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    comboBox.requestFocus();
                }
            });
        }
        return comboBox;
    }

    private class DVComboBoxRendered  extends JLabel implements ListCellRenderer{
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        public DVComboBoxRendered(){
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }


        public Component getListCellRendererComponent(JList list, Object value,
                                                      int index, boolean isSelected, boolean cellHasFocus) {
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }
            if (OpenEHRConstUI.NULL_FLAVOUR_MAP.containsKey(value)){
                String desc = OpenEHRConstUI.NULL_FLAVOUR_MAP.get(value).getValue();
                setText(desc);
                setToolTipText(desc);
            }else{
                setText(" ");
                setToolTipText("");
            }
            return this;
        }
    }

    private JPanel getBottonPanel(){
        if (bottonPanel==null){
            bottonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
            bottonPanel.add(getAcceptButton());
            bottonPanel.add(getCancelButton());
        }
        return bottonPanel;
    }
    private JButton getAcceptButton() {
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

    /**
     * This method initializes cancelButton	
     *
     * @return javax.swing.JButton
     */
    private JButton getCancelButton() {
        if (cancelButton == null) {
            cancelButton = new JButton();
            cancelButton.setText(OpenEHRLanguageManager.getMessage("Cancel"));
            cancelButton.setIcon(OpenEHRImageUtil.CANCEL_ICON);
            cancelButton.setEnabled(true);
            cancelButton.addActionListener(getCancelChangesAction());
        }
        return cancelButton;
    }

    protected AceptarCambiosAction getAcceptChangesAction(){
        if (acceptChangesAction == null){
            acceptChangesAction = new AceptarCambiosAction();
        }
        return acceptChangesAction;
    }

    protected CancelChangesAction getCancelChangesAction(){
        if (cancelChangesAction == null){
            cancelChangesAction = new CancelChangesAction();
        }
        return cancelChangesAction;
    }

    protected class CancelChangesAction extends WindowAdapter implements ActionListener{

        public void windowOpened(WindowEvent e){
            if (_componentWithFirstFocus!=null){
                _componentWithFirstFocus.requestFocus();
            }
        }

        public void actionPerformed(ActionEvent e) {
            exit();
        }

        public void windowClosing(WindowEvent we) {
            exit();
        }
    }

    public class AceptarCambiosAction extends AbstractAction{

        /**
         * Comentario para <code>serialVersionUID</code>
         */
        private static final long serialVersionUID = -8058749276509227718L;

        public void actionPerformed(ActionEvent e) {
            accept();
        }
    }

    protected final void accept(){
        if (acceptDialog()){
            _respuesta = true;
            setVisible(false);
        }
    }

    protected final void exit(){
        if (cancelDialog()){
            _respuesta = false;
            setVisible(false);
        }
    }

    public final boolean getAnswer(){
        return  _respuesta;
    }

    protected void registerComponentWithFirstFocus(JComponent componentWithFirstFocus){
        _componentWithFirstFocus = componentWithFirstFocus;
    }

    protected final void setRespuesta(boolean respuesta){
        _respuesta = respuesta;
    }

    protected boolean cancelDialog(){
        return true;
    }

    protected boolean acceptDialog(){
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
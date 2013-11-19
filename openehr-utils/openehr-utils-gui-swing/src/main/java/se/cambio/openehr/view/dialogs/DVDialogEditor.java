/*
 * Creado el 04/01/2010
 */
package se.cambio.openehr.view.dialogs;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.view.panels.DVGenericPanel;
import se.cambio.openehr.view.util.DVPanelFactory;
import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.StyledEditorKit;
import java.awt.*;
import java.awt.event.*;


public class DVDialogEditor  extends JDialog {

    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    private AceptarCambiosAction acceptChangesAction = null;
    private CancelChangesAction cancelChangesAction = null;
    private DVGenericPanel dvGenericPanel = null;
    private boolean _respuesta = false;
    private JComponent _componentWithFirstFocus = null;
    private ArchetypeElementVO _archetypeElementVO = null;
    private JPanel mainPanel = null;
    private boolean _allowNull = false;
    private boolean _enableUnits = false;
    private JPanel bottonPanel;
    private JButton acceptButton;
    private JButton cancelButton;
    private JTextPane jTextPane;

    public DVDialogEditor(Window owner, ArchetypeElementVO archetypeElementVO, boolean allowNull, boolean enableUnits){
        super(owner, archetypeElementVO.getName(), ModalityType.APPLICATION_MODAL);
        _archetypeElementVO = archetypeElementVO;
        _allowNull = allowNull;
        _enableUnits = enableUnits;
        init(new Dimension(450,150));
    }

    private void init(Dimension size){
        this.setSize(size);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
        this.setResizable(true);
        this.addWindowListener(getCancelChangesAction());
        this.setContentPane(getMainPanel());
	/* Enter KeyStroke */
        KeyStroke enter = KeyStroke.getKeyStroke( KeyEvent.VK_ENTER,0,true);
        getDVGenericPanel().registerKeyboardAction(getAcceptChangesAction(), enter, JComponent.WHEN_IN_FOCUSED_WINDOW);
        KeyStroke esc = KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE,0,true);
        getDVGenericPanel().registerKeyboardAction(getCancelChangesAction(), esc, JComponent.WHEN_IN_FOCUSED_WINDOW);
    }

    private JPanel getMainPanel(){
        if (mainPanel==null){
            mainPanel = new JPanel(new BorderLayout());
            JPanel panelAux1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
            JLabel label = new JLabel(_archetypeElementVO.getName()+":");
            label.setIcon(OpenEHRDataValuesUI.getIcon(_archetypeElementVO.getRMType()));
            panelAux1.add(label);
            panelAux1.add(getDVGenericPanel());
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
            jTextPane.setText(_archetypeElementVO.getDescription());
            jTextPane.setEditable(false);
            jTextPane.setBackground(null);
            jTextPane.setPreferredSize(new java.awt.Dimension(250,70));
        }
        return jTextPane;
    }

    public DVGenericPanel getDVGenericPanel() {
        if (dvGenericPanel==null){
            dvGenericPanel =
                    DVPanelFactory.createDVPanel(
                            _archetypeElementVO.getId(),
                            _archetypeElementVO.getIdTemplate(),
                            _archetypeElementVO.getRMType(),
                            _allowNull,
                            _enableUnits,
                            true);
        }
        return dvGenericPanel;
    }

    public void setDVGenericPanel(DVGenericPanel panel) {
        dvGenericPanel = panel;
        this.remove(getMainPanel());
        mainPanel = null;
        this.setContentPane(getMainPanel());
    }

    public DataValue getDataValue(){
        return getDVGenericPanel().getDataValue();
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
package se.cambio.openehr.view.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class TextWithCleanButtonPanel extends JPanel {

    private static final long serialVersionUID = -1100436512017425216L;
    private JTextField _jTextField = null;
    private JButton _jButton = null;
    private KeyListener _keyListener = null;

    public TextWithCleanButtonPanel() {
        super();
        init();
    }

    private void init() {
        this.setLayout(new BorderLayout());
        this.add(getJTextField());
        this.add(getJButton(), BorderLayout.EAST);
        Border textBorder = getJTextField().getBorder();
        getJTextField().setBorder(null);
        this.setBackground(getJTextField().getBackground());
        this.setBorder(textBorder);
    }

    public JTextField getJTextField() {
        if (_jTextField == null) {
            _jTextField = new JTextField();
            _jTextField.setPreferredSize(new Dimension(80, 10));
            _jTextField.setBorder(BorderFactory.createCompoundBorder(_jTextField.getBorder(),
                    BorderFactory.createEmptyBorder(0, 2, 0, 0)));
        }
        return _jTextField;
    }

    public JButton getJButton() {
        if (_jButton == null) {
            _jButton = new JButton();
            _jButton.setIcon(OpenEHRImageUtil.CLEAR_ICON);
            _jButton.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 5));
            _jButton.setContentAreaFilled(false);
            _jButton.setBorderPainted(false);
            _jButton.setToolTipText(OpenEHRLanguageManager.getMessage("ClearTextField"));
            _jButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ev) {
                    getJTextField().setText("");
                    if (_keyListener != null) {
                        _keyListener.keyReleased(null);
                    }

                }
            });
        }
        return _jButton;
    }

    public void addKeyListener(KeyListener keyListener) {
        _keyListener = keyListener;
        getJTextField().addKeyListener(keyListener);
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
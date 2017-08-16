package se.cambio.cds.gdl.editor.view.labels;

import se.cambio.cds.gdl.model.readable.rule.lines.ArchetypeElementInstantiationRuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.ArchetypeElementRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.GTCodeRuleLineElement;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElementWithValue;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Iterator;

public class JLinkRuleElementLabel extends JLabel implements MouseListener {

    private static final long serialVersionUID = 7112008L;

    private Color linkColorVarSet;
    private Color linkColorVarUnSet;
    private Color hoverColor;
    private Color pressColor;
    private java.util.List<ActionListener> listeners;
    public static final String ACTION_RIGHT_CLICK = "LINK_ACTION_RIGHT_CLICK";
    private static final String ACTION_LEFT_CLICK = "LINK_ACTION_LEFT_CLICK";
    private RuleLineElementWithValue<?> ruleLineElementWithValue = null;
    private String language;

    private static final Color LINK_COLOR_VARSET = Color.BLUE;
    private static final Color LINK_COLOR_VARUNSET = new Color(200, 50, 50);
    private static final Color LINK_COLOR_COMMENTED = Color.GRAY;

    public JLinkRuleElementLabel(RuleLineElementWithValue<?> ruleLineElementWithValue, String language) {
        this.ruleLineElementWithValue = ruleLineElementWithValue;
        this.language = language;
        linkColorVarSet = LINK_COLOR_VARSET;
        linkColorVarUnSet = LINK_COLOR_VARUNSET;
        hoverColor = new Color(128, 0, 128);
        pressColor = Color.BLUE;
        listeners = new ArrayList<>();
        refresh();
        addMouseListener(this);
    }

    public void setCommented(boolean commented) {
        if (commented) {
            linkColorVarSet = LINK_COLOR_COMMENTED;
            linkColorVarUnSet = LINK_COLOR_COMMENTED;
        } else {
            linkColorVarSet = LINK_COLOR_VARSET;
            linkColorVarUnSet = LINK_COLOR_VARUNSET;
        }
    }

    public void addActionListener(ActionListener listener) {
        listeners.add(listener);
    }

    public RuleLineElementWithValue<?> getRuleLineElementWithValue() {
        return ruleLineElementWithValue;
    }

    private void mouseClickedAction(String actionCommand) {
        ActionEvent event = new ActionEvent(this, 0, actionCommand);
        ActionListener listener;
        for (Iterator<ActionListener> iterator = listeners.iterator(); iterator.hasNext(); listener.actionPerformed(event)) {
            listener = iterator.next();
        }
    }

    public void mouseClicked(MouseEvent ev) {
        if (super.isEnabled()) {
            if (ev.getButton() == MouseEvent.BUTTON1) {
                mouseClickedAction(ACTION_LEFT_CLICK);
            } else if (ev.getButton() == MouseEvent.BUTTON3) {
                mouseClickedAction(ACTION_RIGHT_CLICK);
            }
        }
    }

    public void mousePressed(MouseEvent ev) {
        if (super.isEnabled()) {
            setForeground(pressColor);
        }
    }

    public void mouseReleased(MouseEvent ev) {
        if (super.isEnabled()) {
            setForeground(hoverColor);
        }
    }

    public void mouseEntered(MouseEvent ev) {
        if (super.isEnabled()) {
            setForeground(hoverColor);
            setCursor(Cursor.getPredefinedCursor(12));
        }
    }

    public void mouseExited(MouseEvent ev) {
        refresh();
        setCursor(Cursor.getPredefinedCursor(0));
    }

    @Override
    public void setText(String text) {
        if (text != null && text.length() > 0) {
            super.setText("<html><u>" + text + "</u></html>");
        } else {
            super.setText("");
        }
    }

    public void refresh() {
        String text = ruleLineElementWithValue.getLabelText(language);
        setText(text);
        if (ruleLineElementWithValue.getValue() instanceof ArchetypeReference) {
            String domainId = ((ArchetypeReference) ruleLineElementWithValue.getValue()).getIdDomain();
            setIcon(DomainsUI.getIcon(domainId));
        } else if (ruleLineElementWithValue.getValue() instanceof ArchetypeElementRuleLineElement) {
            ArchetypeElementRuleLineElement aerle = ((ArchetypeElementRuleLineElement) ruleLineElementWithValue.getValue());
            String domainId = null;
            if (aerle != null) {
                domainId = aerle.getDomainId();
            }
            setIcon(DomainsUI.getIcon(domainId));
        } else if (ruleLineElementWithValue.getValue() instanceof GTCodeRuleLineElement) {
            RuleLine parentRuleLine = ((GTCodeRuleLineElement) ruleLineElementWithValue.getValue()).getParentRuleLine();
            if (parentRuleLine instanceof ArchetypeElementInstantiationRuleLine) {
                String domainId = ((ArchetypeElementInstantiationRuleLine) parentRuleLine).getArchetypeReference().getIdDomain();
                setIcon(DomainsUI.getIcon(domainId));
            }
        }
        if (ruleLineElementWithValue.getValue() != null) {
            setForeground(linkColorVarSet);
        } else {
            setForeground(linkColorVarUnSet);
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
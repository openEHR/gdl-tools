/*
 * Creado el 07/11/2008
 */
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

public class JLinkRuleElementLabel extends JLabel implements MouseListener{

    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 7112008L;

    public static final String LINK_ACTION = "LINK_ACTION";
    private Color linkColorVarSet;
    private Color linkColorVarUnSet;
    private Color hoverColor;
    private Color pressColor;
    private java.util.List<ActionListener> listeners;
    public final static String ACTION_RIGHT_CLICK = "LINK_ACTION_RIGHT_CLICK";
    public final static String ACTION_LEFT_CLICK = "LINK_ACTION_LEFT_CLICK";
    private RuleLineElementWithValue<?> _ruleLineElementWithValue = null;

    private static final Color LINK_COLOR_VARSET = Color.BLUE;//new Color(50, 200, 50);
    private static final Color LINK_COLOR_VARUNSET = new Color(200, 50, 50);
    private static final Color LINK_COLOR_COMMENTED = Color.GRAY;

    public JLinkRuleElementLabel(RuleLineElementWithValue<?> ruleLineElementWithValue){
        _ruleLineElementWithValue = ruleLineElementWithValue;
        linkColorVarSet = LINK_COLOR_VARSET;
        linkColorVarUnSet = LINK_COLOR_VARUNSET;
        hoverColor = new Color(128, 0, 128);
        pressColor = Color.BLUE;
        listeners = new ArrayList<ActionListener>();
        refresh();
        addMouseListener(this);
    }

    public void setCommented(boolean commented){
        if (commented){
            linkColorVarSet = LINK_COLOR_COMMENTED;
            linkColorVarUnSet = LINK_COLOR_COMMENTED;
        }else{
            linkColorVarSet = LINK_COLOR_VARSET;
            linkColorVarUnSet = LINK_COLOR_VARUNSET;
        }
    }


    public void addActionListener(ActionListener listener){
        listeners.add(listener);
    }

    public void removeActionListener(ActionListener listener){
        listeners.remove(listener);
    }

    public RuleLineElementWithValue<?> getRuleLineElementWithValue(){
        return _ruleLineElementWithValue;
    }

    public void setRuleLineElementWithValue(RuleLineElementWithValue<?> ruleLineElementWithValue){
        _ruleLineElementWithValue = ruleLineElementWithValue;
    }

    private void mouseClickedAction(String actionCommand){
        ActionEvent event = new ActionEvent(this, 0, actionCommand);
        ActionListener listener;
        for(Iterator<ActionListener> i$ = listeners.iterator(); i$.hasNext(); listener.actionPerformed(event))
            listener = (ActionListener)i$.next();
    }

    public void setLinkVarSetColor(Color color){
        linkColorVarSet = color;
        refresh();
    }

    public Color getLinkVarSetColor(){
        return linkColorVarSet;
    }

    public void setLinkVarUnSetColor(Color color){
        linkColorVarUnSet = color;
        refresh();
    }

    public Color getLinkVarUnSetColor(){
        return linkColorVarUnSet;
    }

    public void setHoverColor(Color color){
        hoverColor = color;
    }

    public Color getHoverColor(){
        return hoverColor;
    }

    public void setPressColor(Color color){
        pressColor = color;
    }

    public Color getPressColor(){
        return pressColor;
    }

    public void setText(String text){
        if(text != null && text.length() > 0){
            super.setText("<html><u>"+text+"</u></html>");
        }else{
            super.setText("");
        }
    }

    public void mouseClicked(MouseEvent e){
        if(super.isEnabled()){
            if (e.getButton()==MouseEvent.BUTTON1){
                mouseClickedAction(ACTION_LEFT_CLICK);
            }else if (e.getButton()==MouseEvent.BUTTON3){
                mouseClickedAction(ACTION_RIGHT_CLICK);
            }
        }
    }

    public void mousePressed(MouseEvent e){
        if(super.isEnabled()){
            setForeground(pressColor);
        }
    }

    public void mouseReleased(MouseEvent e){
        if(super.isEnabled()){
            setForeground(hoverColor);
        }
    }

    public void mouseEntered(MouseEvent e){
        if(super.isEnabled()){
            setForeground(hoverColor);
            setCursor(Cursor.getPredefinedCursor(12));
        }
    }

    public void mouseExited(MouseEvent e){
        refresh();
        setCursor(Cursor.getPredefinedCursor(0));
    }

    public void refresh(){
        String text = _ruleLineElementWithValue.toString();
        setText(text);
        if (_ruleLineElementWithValue.getValue() instanceof ArchetypeReference){
            String domainId = ((ArchetypeReference)_ruleLineElementWithValue.getValue()).getIdDomain();
            setIcon(DomainsUI.getIcon(domainId));
        }else if (_ruleLineElementWithValue.getValue() instanceof ArchetypeElementRuleLineElement){
            ArchetypeElementRuleLineElement aerle = ((ArchetypeElementRuleLineElement)_ruleLineElementWithValue.getValue());
            String domainId = null;
            if (aerle!=null){
                domainId = aerle.getDomainId();
            }
            setIcon(DomainsUI.getIcon(domainId));
        }else if (_ruleLineElementWithValue.getValue() instanceof GTCodeRuleLineElement){
            RuleLine parentRuleLine = ((GTCodeRuleLineElement)_ruleLineElementWithValue.getValue()).getParentRuleLine();
            if (parentRuleLine instanceof ArchetypeElementInstantiationRuleLine){
                String domainId = ((ArchetypeElementInstantiationRuleLine)parentRuleLine).getArchetypeReference().getIdDomain();
                setIcon(DomainsUI.getIcon(domainId));
            }
        }
        if (_ruleLineElementWithValue.getValue()!=null){
            setForeground(linkColorVarSet);
        }else{
            setForeground(linkColorVarUnSet);
        }
    }
}/*
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
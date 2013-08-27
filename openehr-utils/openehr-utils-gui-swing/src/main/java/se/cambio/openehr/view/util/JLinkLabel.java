/*
 * Creado el 07/11/2008
 */
package se.cambio.openehr.view.util;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.JLabel;

public class JLinkLabel extends JLabel implements MouseListener{


    public static Color LINK_COLOR = Color.BLUE;
    public static Color COMMENTED_LINK_COLOR = Color.GRAY;
    /**
     * Comentario para <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 7112008L;

    public JLinkLabel()
    {
	linkColor = LINK_COLOR;
	hoverColor = new Color(128, 0, 128);
	pressColor = Color.RED;
	listeners = new ArrayList<ActionListener>();
	actionCommand = "LINK_ACTION";
	setForeground(linkColor);
	addMouseListener(this);
    }

    public JLinkLabel(Color linkColor, Color hoverColor, Color pressColor)
    {
	this.linkColor = Color.BLUE;
	this.hoverColor = new Color(128, 0, 128);
	this.pressColor = Color.RED;
	listeners = new ArrayList<ActionListener>();
	actionCommand = "LINK_ACTION";
	this.linkColor = linkColor;
	this.hoverColor = hoverColor;
	this.pressColor = pressColor;
	setForeground(this.linkColor);
	addMouseListener(this);
    }

    public void setActionCommand(String command)
    {
	actionCommand = command;
    }

    public void addActionListener(ActionListener listener)
    {
	listeners.add(listener);
    }

    public void removeActionListener(ActionListener listener)
    {
	listeners.remove(listener);
    }

    private void mouseClickedAction()
    {
	ActionEvent event = new ActionEvent(this, 0, actionCommand);
	ActionListener listener;
	for(Iterator<ActionListener> i$ = listeners.iterator(); i$.hasNext(); listener.actionPerformed(event))
	    listener = (ActionListener)i$.next();

    }

    public void setCommented(boolean commented){
	if (commented){
	    linkColor = COMMENTED_LINK_COLOR;
	}else{
	    linkColor = LINK_COLOR;
	}
	setForeground(linkColor);
    }

    public void setLinkColor(Color color)
    {
	linkColor = color;
	setForeground(linkColor);
    }

    public Color getLinkColor()
    {
	return linkColor;
    }

    public void setHoverColor(Color color)
    {
	hoverColor = color;
    }

    public Color getHoverColor()
    {
	return hoverColor;
    }

    public void setPressColor(Color color)
    {
	pressColor = color;
    }

    public Color getPressColor()
    {
	return pressColor;
    }

    public void setText(String text)
    {
	if(super.isEnabled()){
	    setEnabledText(text);
	}else{
	    setDisabledText(text);
	}
    }


    public void setEnabledText(String text)
    {
	if(text != null && text.length() > 0){
	    text= text.replace("<", "&lt;").replace(">", "&gt;");
	    super.setText((new StringBuilder()).append("<html><u>").append(text).append("</u></html>").toString());
	}else{
	    super.setText("");
	}
    }

    public void setDisabledText(String text)
    {
	if(text != null && text.length() > 0)
	    super.setText(text);
	else
	    super.setText("");
    }

    public void mouseClicked(MouseEvent e)
    {
	if(super.isEnabled()){
	    mouseClickedAction();
	}
    }

    public void mousePressed(MouseEvent e)
    {
	if(super.isEnabled()){
	    setForeground(pressColor);
	}
    }

    public void mouseReleased(MouseEvent e)
    {
	if(super.isEnabled()){
	    setForeground(hoverColor);
	}
    }

    public void mouseEntered(MouseEvent e)
    {
	if(super.isEnabled()){
	    setForeground(hoverColor);
	    setCursor(Cursor.getPredefinedCursor(12));
	}
    }

    public void mouseExited(MouseEvent e)
    {
	setForeground(linkColor);
	setCursor(Cursor.getPredefinedCursor(0));
    }

    public void setEnabled(boolean enabled){
	setText(super.getText());
	super.setEnabled(enabled);
	this.enabled = enabled;
    }

    public void setOcupado(boolean ocupado){
	if (this.ocupado == ocupado){
	    return;
	}else{
	    this.ocupado = ocupado;
	}
	setText(super.getText());
	if (ocupado){
	    enabled = this.isEnabled();
	    super.setEnabled(false);
	}else{
	    super.setEnabled(enabled);
	}
    }

    public static final String LINK_ACTION = "LINK_ACTION";
    private Color linkColor;
    private Color hoverColor;
    private Color pressColor;
    public boolean ocupado = false;
    public boolean enabled = true;
    private java.util.List<ActionListener> listeners;
    private String actionCommand;
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
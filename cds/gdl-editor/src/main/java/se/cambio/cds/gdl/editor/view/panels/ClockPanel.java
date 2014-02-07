package se.cambio.cds.gdl.editor.view.panels;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

public final class ClockPanel extends JPanel{

    private static final long serialVersionUID = 1L;
    private JPanel show_date_time = new JPanel();
    private JLabel show_time = new JLabel("Show Time");

    private DateFormat dateFormat2 = 
	    new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");      
    private java.util.Date date2;
    private JLabel label;
    private JPanel panel;
    private boolean on = true;

    public ClockPanel(){        

	this.setLayout(new BorderLayout());
	this.add(show_Time_date());
    }    

    public JPanel show_Time_date(){

	getShow_date_time().setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

	setShow_time(new JLabel(""));

	updateDateTime();

	//getShow_time().setFont(f);
	getShow_date_time().add(getShow_time());

	return getShow_date_time();
    }

    public void updateDateTime()
    {
	Thread th = new Thread(new Runnable()
	{
	    @Override
	    public void run()
	    {
		while(on)
		{
		    date2 = new java.util.Date();
		    final String dateTime = dateFormat2.format(date2);
		    /*
		     * Any updates to the GUI, must also be done
		     * on the EDT - Event Dispatch Thread.
		     */
		    SwingUtilities.invokeLater(new Runnable()
		    {
			public void run(){
			    getShow_time().setText(dateTime);
			}
		    });
		    try {
			Thread.sleep(1000);
		    } catch (InterruptedException e) {
			e.printStackTrace();
		    }
		}
	    }
	});
	th.start();
    }

    /**
     * @return the show_time
     */
    public JLabel getShow_time() {

	return show_time;
    }

    /**
     * @param show_time the show_time to set
     */
    public void setShow_time(JLabel show_time) {
	this.show_time = show_time;
    }

    /**
     * @return the show_date_time
     */
    public JPanel getShow_date_time() {
	return show_date_time;
    }

    /**
     * @param show_date_time the show_date_time to set
     */
    public void setShow_date_time(JPanel show_date_time) {
	this.show_date_time = show_date_time;
    }

    /**
     * @return the label1
     */


    /**
     * @param label1 the label1 to set
     */


    /**
     * @return the label
     */
    public JLabel getLabel() {

	return label;
    }

    /**
     * @param label the label to set
     */
    public void setLabel(JLabel label) {
	this.label = label;
    }

    /**
     * @return the panel
     */
    public JPanel getPanel() {
	return panel;
    }

    /**
     * @param panel the panel to set
     */
    public void setPanel(JPanel panel) {
	this.panel = panel;
    }

    public void setOff(){
	on = false;
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
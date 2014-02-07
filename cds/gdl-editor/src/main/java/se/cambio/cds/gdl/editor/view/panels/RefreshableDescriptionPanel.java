package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.view.panels.interfaces.RefreshablePanel;

import javax.swing.*;
import java.awt.*;

public class RefreshableDescriptionPanel extends JPanel implements RefreshablePanel{

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private GDLEditor _controller = null;
    private DescriptionPanel descriptionPanel = null;

    public RefreshableDescriptionPanel(GDLEditor gdlEditor){
        _controller = gdlEditor;
        init();
    }

    public void init(){
        this.setLayout(new BorderLayout());
        refresh();
    }

    @Override
    public void refresh() {
        this.removeAll();
        descriptionPanel = null;
        this.add(getDescriptionPanel());
        this.revalidate();
        this.repaint();
    }

    private DescriptionPanel getDescriptionPanel(){
        if (descriptionPanel==null){
            descriptionPanel = new DescriptionPanel(_controller);
        }
        return descriptionPanel;
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
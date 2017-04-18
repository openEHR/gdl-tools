package se.cambio.cds.gdl.graph.view.panel;

import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.graph.controller.renderer.GraphRenderingException;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.cds.view.swing.panel.interfaces.PluginPanelI;
import se.cambio.cds.view.swing.panel.interfaces.RefreshablePanel;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.ExceptionHandler;

import javax.swing.*;
import java.awt.*;
import java.net.URL;
import java.util.Collections;

public class GdlGraphManager {

    private ArchetypeManager archetypeManager;
    private final ArchetypeReferencesManager archetypeReferencesManager;
    private final ElementInstanceCollectionManager elementInstanceCollectionManager;
    private Guide guide;
    private String language;

    public GdlGraphManager(
            ArchetypeManager archetypeManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ElementInstanceCollectionManager elementInstanceCollectionManager){
        this.archetypeManager = archetypeManager;
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
    }


    public DecisionGraphPanel generateDecisionGraphPanel(Guide guide, String language) {
        DecisionGraphPanel decisionGraphPanel = null;
        try {
            decisionGraphPanel =
                    new DecisionGraphPanel(
                    Collections.singleton(guide),
                            true,
                            language,
                            archetypeManager,
                            archetypeReferencesManager,
                            elementInstanceCollectionManager);
        } catch (GraphRenderingException e) {
            ExceptionHandler.handle(e);
        }
        return decisionGraphPanel;
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
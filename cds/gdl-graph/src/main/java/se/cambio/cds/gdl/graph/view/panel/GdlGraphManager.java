package se.cambio.cds.gdl.graph.view.panel;

import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.gdl.graph.controller.renderer.GraphRenderingException;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.ElementInstanceCollectionManager;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import java.util.Collections;

@Slf4j
public class GdlGraphManager {

    private ArchetypeManager archetypeManager;
    private final ArchetypeReferencesManager archetypeReferencesManager;
    private final ElementInstanceCollectionManager elementInstanceCollectionManager;

    public GdlGraphManager(
            ArchetypeManager archetypeManager,
            ArchetypeReferencesManager archetypeReferencesManager,
            ElementInstanceCollectionManager elementInstanceCollectionManager) {
        this.archetypeManager = archetypeManager;
        this.archetypeReferencesManager = archetypeReferencesManager;
        this.elementInstanceCollectionManager = elementInstanceCollectionManager;
    }


    public DecisionGraphPanel generateDecisionGraphPanel(Guide guide, String language) {
        Assert.notNull(guide, "Trying to generate decision graph panel on null guideline!");
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
        } catch (GraphRenderingException ex) {
            log.error("Error rendering graph panel for guideline: " + guide.getId(), ex);
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
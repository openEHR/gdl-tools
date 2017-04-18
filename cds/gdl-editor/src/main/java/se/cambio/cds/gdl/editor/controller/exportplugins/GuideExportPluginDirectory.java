package se.cambio.cds.gdl.editor.controller.exportplugins;

import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideExportPlugin;
import se.cambio.cds.gdl.converters.drools.DroolsGuideExportPlugin;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;

public class GuideExportPluginDirectory {

    private static GuideExportPluginDirectory instance;
    private Collection<GuideExportPlugin> guideExportPlugins = null;

    public GuideExportPluginDirectory(DroolsGuideExportPlugin droolsGuideExportPlugin) {
        guideExportPlugins = new ArrayList<>();
        guideExportPlugins.add(droolsGuideExportPlugin);
    }

    private Collection<GuideExportPlugin> getGuideExportPlugins() {
        return guideExportPlugins;
    }

    public byte[] compile(Guide guide) throws InternalErrorException {
        LoggerFactory.getLogger(GuideExportPluginDirectory.class).info("Compiling ...");
        Collection<GuideExportPlugin> guideExportPlugins = getGuideExportPlugins();
        if (guideExportPlugins.iterator().hasNext()) {
            return guideExportPlugins.iterator().next().compile(guide);
        } else {
            return null;
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
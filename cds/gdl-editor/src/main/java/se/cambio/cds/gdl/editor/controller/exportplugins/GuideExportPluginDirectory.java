package se.cambio.cds.gdl.editor.controller.exportplugins;

import org.apache.log4j.Logger;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;

public class GuideExportPluginDirectory {

    private static GuideExportPluginDirectory _instance;
    public Collection<GuideExportPlugin> _guideExportPlugins = null;

    private GuideExportPluginDirectory(){
        _guideExportPlugins = new ArrayList<GuideExportPlugin>();
        _guideExportPlugins.add(new DroolsGuideExportPlugin());
    }

    public static Collection<GuideExportPlugin> getGuideExportPlugins(){
        return getDelegate()._guideExportPlugins;
    }

    public static byte[] compile(Guide guide) throws InternalErrorException{
        //TODO Should allow choosing...
        Logger.getLogger(GuideExportPluginDirectory.class).info("Compiling ...");
        Collection<GuideExportPlugin> guideExportPlugins = getGuideExportPlugins();
        if(guideExportPlugins.iterator().hasNext()){
            return guideExportPlugins.iterator().next().compile(guide);
        }else{
            return null;
        }
    }

    public static GuideExportPluginDirectory getDelegate(){
        if (_instance==null){
            _instance = new GuideExportPluginDirectory();
        }
        return _instance;
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
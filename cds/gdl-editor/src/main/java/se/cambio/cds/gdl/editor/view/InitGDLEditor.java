package se.cambio.cds.gdl.editor.view;


import org.springframework.core.env.ConfigurableEnvironment;
import se.cambio.cds.gdl.editor.controller.EditorInitializer;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GdlEditorFactory;
import se.cambio.cds.gdl.editor.controller.GuidelineLoadManager;
import se.cambio.cds.gdl.editor.controller.sw.LoadEditorSW;
import se.cambio.cds.gdl.editor.view.dialog.DialogSplash;
import se.cambio.cds.gdl.editor.view.frame.EditorFrame;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.view.dialogs.InfoDialog;
import se.cambio.openehr.view.util.WindowManager;

import java.io.File;

public class InitGDLEditor {

    public static void main(String[] args) {

        ConfigurableEnvironment environment = BeanProvider.getBean(ConfigurableEnvironment.class);
        UserConfigurationManager userConfigurationManager = BeanProvider.getBean(UserConfigurationManager.class);
        String activeRuleEngine = userConfigurationManager.getActiveRuleEngine();
        environment.addActiveProfile(activeRuleEngine);
        String[] activeProfiles = BeanProvider.getActiveProfiles();
        BeanProvider.setActiveProfiles(activeProfiles);

        EditorInitializer editorInitializer = BeanProvider.getBean(EditorInitializer.class);
        EditorFrame ef = editorInitializer.createEditorFrame();
        DialogSplash dialog = new DialogSplash(ef, true);

        WindowManager windowManager = BeanProvider.getBean(WindowManager.class);
        windowManager.registerMainWindow(ef);
        windowManager.registerProgressManager(new InfoDialog(ef));


        GdlEditorFactory gdlEditorFactory = BeanProvider.getBean(GdlEditorFactory.class);
        EditorManager editorManager = BeanProvider.getBean(EditorManager.class);
        new LoadEditorSW(dialog, gdlEditorFactory, editorManager).execute();
        dialog.setVisible(true);
        if (args.length > 0) {
            GuidelineLoadManager guidelineLoadManager = BeanProvider.getBean(GuidelineLoadManager.class);
            guidelineLoadManager.loadGuide(new File(args[0]));
        }
        RuleEngineService ruleEngineService = BeanProvider.getBean(RuleEngineService.class);
        ruleEngineService.setUseCache(false);
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
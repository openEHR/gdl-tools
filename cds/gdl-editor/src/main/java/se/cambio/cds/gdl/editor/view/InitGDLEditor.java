package se.cambio.cds.gdl.editor.view;


import lombok.extern.slf4j.Slf4j;
import org.springframework.core.env.ConfigurableEnvironment;
import se.cambio.cds.gdl.editor.controller.EditorInitializer;
import se.cambio.cds.gdl.editor.controller.EditorManager;
import se.cambio.cds.gdl.editor.controller.GdlEditorFactory;
import se.cambio.cds.gdl.editor.controller.GuidelineLoadManager;
import se.cambio.cds.gdl.editor.controller.sw.LoadEditorSW;
import se.cambio.cds.gdl.editor.view.dialog.DialogSplash;
import se.cambio.cds.gdl.editor.view.frame.EditorFrame;
import se.cambio.cds.gdl.editor.view.menubar.MainMenuBar;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.UserConfiguration;
import se.cambio.openehr.view.dialogs.InfoDialog;
import se.cambio.openehr.view.util.WindowManager;

import javax.swing.*;
import java.io.File;
import java.lang.reflect.InvocationTargetException;

@Slf4j
public class InitGDLEditor {

    public static void main(String[] args) throws InvocationTargetException, InterruptedException {
        ConfigurableEnvironment environment = BeanProvider.getBean(ConfigurableEnvironment.class);
        UserConfigurationManager userConfigurationManager = UserConfiguration.getInstanceUserConfigurationManager();
        String activeRuleEngine = userConfigurationManager.getActiveRuleEngine();
        environment.addActiveProfile(activeRuleEngine);
        String[] activeProfiles = BeanProvider.getActiveProfiles();
        BeanProvider.setActiveProfiles(activeProfiles);

        EditorInitializer editorInitializer = BeanProvider.getBean(EditorInitializer.class);
        EditorFrame ef = editorInitializer.createEditorFrame();

        WindowManager windowManager = BeanProvider.getBean(WindowManager.class);
        windowManager.registerMainWindow(ef);
        windowManager.registerProgressManager(new InfoDialog(ef));

        GdlEditorFactory gdlEditorFactory = BeanProvider.getBean(GdlEditorFactory.class);
        EditorManager editorManager = BeanProvider.getBean(EditorManager.class);


        RuleEngineService ruleEngineService = BeanProvider.getBean(RuleEngineService.class);
        ruleEngineService.setUseCache(false);

        GuidelineLoadManager guidelineLoadManager = BeanProvider.getBean(GuidelineLoadManager.class);

        showSplashDialog(ef);

        initGdlEditor(args, guidelineLoadManager, gdlEditorFactory, editorManager);
    }

    private static void showSplashDialog(EditorFrame ef) {
        DialogSplash dialog = new DialogSplash(ef, true);
        new Thread(() -> {
            dialog.setVisible(true);
        }).start();
        new Thread(() -> {
            try {
                Thread.sleep(2000);
            } catch (InterruptedException ex) {
                log.error("Error waiting for dialog to close", ex);
            }
            dialog.stop();
        }).start();
    }

    public static void initGdlEditor(
            String[] args,
            GuidelineLoadManager guidelineLoadManager,
            GdlEditorFactory gdlEditorFactory,
            EditorManager editorManager) {
        MainMenuBar mainMenuBar = BeanProvider.getBean(MainMenuBar.class);
        new LoadEditorSW(gdlEditorFactory, editorManager, mainMenuBar).execute();

        if (args.length > 0) {
            guidelineLoadManager.loadGuide(new File(args[0]));
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
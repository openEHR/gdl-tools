/*
 * Creado el 07-sep-2007
 *


 */
package se.cambio.cds.gdl.editor.util;

import javax.swing.*;
import java.net.URL;

public class GDLEditorImageUtil {

    //COMMON
    public static final ImageIcon SAVE_ICON = GDLEditorImageUtil.getIcon("disk.png");
    public static final ImageIcon ACCEPT_ICON = GDLEditorImageUtil.getIcon("accept.png");
    public static final ImageIcon UNACCEPT_ICON = GDLEditorImageUtil.getIcon("unaccept.png");
    public static final ImageIcon CLEAR_ICON = GDLEditorImageUtil.getIcon("clear.png");
    public static final ImageIcon EMPTY_ICON = GDLEditorImageUtil.getIcon("empty.png");
    public static final ImageIcon ARROW_BACK_ICON = GDLEditorImageUtil.getIcon("arrow-back.gif");
    public static final ImageIcon OBJECT_ICON = GDLEditorImageUtil.getIcon("object.png");
    public static final ImageIcon FUNCTION_ICON = GDLEditorImageUtil.getIcon("function.png");
    public static final ImageIcon FOLDER_OBJECT_ICON = GDLEditorImageUtil.getIcon("folder_object.png");
    public static final ImageIcon FOLDER_ICON = GDLEditorImageUtil.getIcon("folder.png");
    public static final ImageIcon ADD_ICON = GDLEditorImageUtil.getIcon("add.png");
    public static final ImageIcon DELETE_ICON = GDLEditorImageUtil.getIcon("delete.png");
    public static final ImageIcon EDIT_ICON = GDLEditorImageUtil.getIcon("pencil.png");
    public static final ImageIcon DRAG_ICON = GDLEditorImageUtil.getIcon("drag.png");
    public static final ImageIcon EXCLAMATION_ICON = GDLEditorImageUtil.getIcon("exclamation.png");
    public static final ImageIcon SOURCE_ICON = GDLEditorImageUtil.getIcon("source.png");
    public static final ImageIcon DESCRIPTION_ICON = GDLEditorImageUtil.getIcon("tag_blue.png");
    public static final ImageIcon HTML_ICON = GDLEditorImageUtil.getIcon("html.png");
    public static final ImageIcon RUN_ICON = GDLEditorImageUtil.getIcon("run.png");


    public static final ImageIcon ONTOLOGY_ICON = GDLEditorImageUtil.getIcon("ontology.png");
    public static final ImageIcon ADD_ONTOLOGY_ICON = GDLEditorImageUtil.getIcon("add_ontology.png");

    public static final ImageIcon GDL_LANG_ICON = GDLEditorImageUtil.getIcon("gdllang.png");
    public static final ImageIcon TRANSLATE_ICON = GDLEditorImageUtil.getIcon("translate.png");

    //RULES
    public static final ImageIcon RULE_ICON = GDLEditorImageUtil.getIcon("rule.png");
    public static final ImageIcon ADD_RULE_ICON = GDLEditorImageUtil.getIcon("add_rule.png");

    //CONDITIONS
    public static final ImageIcon CONDITION_ICON = GDLEditorImageUtil.getIcon("condition.png");

    //ACTIONS
    public static final ImageIcon ACTION_ICON = GDLEditorImageUtil.getIcon("action.png");

    public static final ImageIcon LOGO = GDLEditorImageUtil.getIcon("gdl.png");

    public static final ImageIcon SPLASH_IMAGE = GDLEditorImageUtil.getIcon("splash.png");

    private static final String IMAGE_DIR = "/img/";

    private static ImageIcon getIcon(String name) {
        return getImg(IMAGE_DIR + name);
    }

    private static ImageIcon getImg(String imagePath) {
        URL url = GDLEditorImageUtil.class.getResource(imagePath);
        if (url != null) {
            return new ImageIcon(url);
        }
        return null;
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
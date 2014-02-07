/*
 * Creado el 07-sep-2007
 *


 */
package se.cambio.cds.gdl.editor.util;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.net.URL;

import javax.swing.ImageIcon;

/**
 * @author icorram
 *


 */
public class GDLEditorImageUtil {

	//COMMON
	public static final ImageIcon SAVE_ICON = GDLEditorImageUtil.getIcon("disk.png");
	public static final ImageIcon ACCEPT_ICON = GDLEditorImageUtil.getIcon("accept.png");
	public static final ImageIcon UNACCEPT_ICON = GDLEditorImageUtil.getIcon("unaccept.png");
	public static final ImageIcon HALF_ACCEPT_ICON = GDLEditorImageUtil.getIcon("half_accept.png");
	public static final ImageIcon CANCEL_ICON = GDLEditorImageUtil.getIcon("cancel.png");
	public static final ImageIcon STOP_ICON = GDLEditorImageUtil.getIcon("stop.png");
	public static final ImageIcon TEST_ICON = GDLEditorImageUtil.getIcon("tick.png");
	public static final ImageIcon CLEAR_ICON = GDLEditorImageUtil.getIcon("clear.png");
	public static final ImageIcon EMPTY_ICON = GDLEditorImageUtil.getIcon("empty.png");
	public static final ImageIcon ARROW_BACK_ICON = GDLEditorImageUtil.getIcon("arrow-back.gif");
	public static final ImageIcon ARROW_UP_ICON = GDLEditorImageUtil.getIcon("arrow-up.png");
	public static final ImageIcon ARROW_DOWN_ICON = GDLEditorImageUtil.getIcon("arrow-down.png");
	public static final ImageIcon OBJECT_ICON = GDLEditorImageUtil.getIcon("object.png");
	public static final ImageIcon FUNCTION_ICON = GDLEditorImageUtil.getIcon("function.png");
	public static final ImageIcon FOLDER_OBJECT_ICON = GDLEditorImageUtil.getIcon("folder_object.png");
	public static final ImageIcon FOLDER_ICON = GDLEditorImageUtil.getIcon("folder.png");
	public static final ImageIcon CONFIG_ICON = GDLEditorImageUtil.getIcon("wrench.png");
	public static final ImageIcon RESTORE_ICON = GDLEditorImageUtil.getIcon("restore.png");
	public static final ImageIcon EXIT_ICON = GDLEditorImageUtil.getIcon("exit.png");
	public static final ImageIcon ADD_ICON = GDLEditorImageUtil.getIcon("add.png");
	public static final ImageIcon DELETE_ICON = GDLEditorImageUtil.getIcon("delete.png");
	public static final ImageIcon EDIT_ICON = GDLEditorImageUtil.getIcon("pencil.png");
	public static final ImageIcon DRAG_ICON = GDLEditorImageUtil.getIcon("drag.png");
	public static final ImageIcon EXCLAMATION_ICON = GDLEditorImageUtil.getIcon("exclamation.png");
	public static final ImageIcon GREY_EXCLAMATION_ICON = GDLEditorImageUtil.getIcon("grey_exclamation.png");
	public static final ImageIcon DSL_ICON = GDLEditorImageUtil.getIcon("dsl.png");
	public static final ImageIcon CALENDAR_ICON = GDLEditorImageUtil.getIcon("calendar.png");
	public static final ImageIcon BLUE_BULLET_ICON = GDLEditorImageUtil.getIcon("bullet_blue.png");
	public static final ImageIcon EXPAND_ICON = GDLEditorImageUtil.getIcon("expand.png");
	public static final ImageIcon CONTRACT_ICON = GDLEditorImageUtil.getIcon("contract.png");
	public static final ImageIcon LIGHTNING_ICON = GDLEditorImageUtil.getIcon("lightning.png");
	public static final ImageIcon SOURCE_ICON = GDLEditorImageUtil.getIcon("source.png");
	public static final ImageIcon DESCRIPTION_ICON = GDLEditorImageUtil.getIcon("tag_blue.png");
	public static final ImageIcon HTML_ICON = GDLEditorImageUtil.getIcon("html.png");
	public static final ImageIcon MAGNIFIER_ICON = GDLEditorImageUtil.getIcon("magnifier.png");
	public static final ImageIcon REFRESH_ICON = GDLEditorImageUtil.getIcon("arrow_refresh.png");
	public static final ImageIcon COMPILE_ICON = GDLEditorImageUtil.getIcon("cog_go.png");
	
	public static final ImageIcon ONTOLOGY_ICON = GDLEditorImageUtil.getIcon("ontology.png");
	public static final ImageIcon ADD_ONTOLOGY_ICON = GDLEditorImageUtil.getIcon("add_ontology.png");
	
	public static final ImageIcon CONNECT_ICON = GDLEditorImageUtil.getIcon("connect.png");
	public static final ImageIcon GDL_ICON = GDLEditorImageUtil.getIcon("gdl.png");
	public static final ImageIcon GDL_LANG_ICON = GDLEditorImageUtil.getIcon("gdllang.png");
	public static final ImageIcon TRANSLATE_ICON = GDLEditorImageUtil.getIcon("translate.png");
	
	//RULES
	public static final ImageIcon RULE_ICON = GDLEditorImageUtil.getIcon("rule.png");
	public static final ImageIcon ADD_RULE_ICON = GDLEditorImageUtil.getIcon("add_rule.png");
	
	//CONDITIONS
	public static final ImageIcon CONDITION_ICON = GDLEditorImageUtil.getIcon("condition.png");
	public static final ImageIcon ADD_CONDITION_ICON = GDLEditorImageUtil.getIcon("add_condition.png");
	
	//ACTIONS
	public static final ImageIcon ACTION_ICON = GDLEditorImageUtil.getIcon("action.png");
	public static final ImageIcon ADD_ACTION_ICON = GDLEditorImageUtil.getIcon("add_action.png");
	
	//TESTING
	public static final ImageIcon PARAMETERS_ICON = GDLEditorImageUtil.getIcon("parameters.png");
	public static final ImageIcon TREATMENTS_ICON = GDLEditorImageUtil.getIcon("treatments.png");
	
	public static final ImageIcon LOGO = GDLEditorImageUtil.getIcon("gdl.png");
	
	public static final ImageIcon SPLASH_IMAGE = GDLEditorImageUtil.getIcon("splash.png");
	
	public static final String IMAGE_DIR = "/img/";
	public static final String ORIG_PARAM_IMAGE_DIR = IMAGE_DIR+"origparam/";
	
	public static ImageIcon getIcon(String name)  {
		return getImg(IMAGE_DIR + name);
	}
	
	public static ImageIcon getIconOrigParam(String name)  {
		return getImg(ORIG_PARAM_IMAGE_DIR + name);
	}

	public static ImageIcon getImage(String name){
		return getImg(IMAGE_DIR+name);
	}
	
	
	public static BufferedImage createBufferedImage(Image image) {
		if(image instanceof BufferedImage)
			return (BufferedImage)image;
		int w = image.getWidth(null);
		int h = image.getHeight(null);
		BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = bi.createGraphics();
		g.drawImage(image, 0, 0, null);
		g.dispose();
		return bi;
	}

	private static ImageIcon getImg(String imagePath){
		URL url = GDLEditorImageUtil.class.getResource(imagePath);
		if (url != null)  {
			return new ImageIcon(url);
		}
		return null;
	}

	public static URL getImageURL(String image){
		return GDLEditorImageUtil.class.getResource(IMAGE_DIR+image);
	}

	public static String getImgHTMLTag(URL imageURL){
		return "<img src=\""+imageURL+"\">";
	}
	public static String getImgHTMLTag(String image){
		return getImgHTMLTag(getImageURL(image));
	}
	public static ImageIcon blend (ImageIcon imagen1, ImageIcon imagen2){
		BufferedImage bi3 = new BufferedImage (imagen1.getIconWidth(), imagen1.getIconHeight(),
				BufferedImage.TYPE_INT_ARGB);
		bi3.getGraphics().drawImage(imagen1.getImage(), 0, 0, null);
		bi3.getGraphics().drawImage(imagen2.getImage(), 0, 0, null);
		return new ImageIcon(bi3);
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
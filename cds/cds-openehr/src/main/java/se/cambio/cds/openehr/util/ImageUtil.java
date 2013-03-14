/*
 * Creado el 07-sep-2007
 *


 */
package se.cambio.cds.openehr.util;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.net.URL;

import javax.swing.ImageIcon;

/**
 * @author icorram
 *


 */
public class ImageUtil {

	//OpenEHR - Entries
	public static final String ENTRY_OBSERVATION_NAME = "entry_observation.png";
	public static final String ENTRY_EVALUATION_NAME = "entry_evaluation.png";
	public static final String ENTRY_INSTRUCTION_NAME = "entry_instruction.png";
	public static final String ENTRY_ACTION_NAME = "entry_action.png";
	
	public static final ImageIcon ENTRY_OBSERVATION_ICON = ImageUtil.getIcon(ENTRY_OBSERVATION_NAME);
	public static final ImageIcon ENTRY_EVALUATION_ICON = ImageUtil.getIcon(ENTRY_EVALUATION_NAME);
	public static final ImageIcon ENTRY_INSTRUCTION_ICON = ImageUtil.getIcon(ENTRY_INSTRUCTION_NAME);
	public static final ImageIcon ENTRY_ACTION_ICON = ImageUtil.getIcon(ENTRY_ACTION_NAME);
	
	public static final String COMPOSITION_NAME = "composition.png";
	public static final ImageIcon COMPOSITION_ICON = ImageUtil.getIcon(COMPOSITION_NAME);
	
	//OpenEHR - Data values
	public static final String DV_TEXT_NAME = "text.png";
	public static final String DV_CODED_TEXT_NAME = "codedtext.png";
	public static final String DV_ORDINAL_NAME = "ordinal.png";
	public static final String DV_QUANTITY_NAME = "quantity.png";
	public static final String DV_DURATION_NAME = "duration.png";
	public static final String DV_COUNT_NAME = "count.png";
	public static final String DV_DATE_TIME_NAME = "datetime.png";
	public static final String DV_BOOLEAN_NAME = "boolean.png";
	public static final String DV_PROPORTION_NAME = "ratio.png";

	
	public static final ImageIcon DV_TEXT_ICON = ImageUtil.getIcon(DV_TEXT_NAME);
	public static final ImageIcon DV_CODED_TEXT_ICON = ImageUtil.getIcon(DV_CODED_TEXT_NAME);
	public static final ImageIcon DV_ORDINAL_ICON = ImageUtil.getIcon(DV_ORDINAL_NAME);
	public static final ImageIcon DV_QUANTITY_ICON = ImageUtil.getIcon(DV_QUANTITY_NAME);
	public static final ImageIcon DV_DURATION_ICON = ImageUtil.getIcon(DV_DURATION_NAME);
	public static final ImageIcon DV_COUNT_ICON = ImageUtil.getIcon(DV_COUNT_NAME);
	public static final ImageIcon DV_DATE_TIME_ICON = ImageUtil.getIcon(DV_DATE_TIME_NAME);
	public static final ImageIcon DV_BOOLEAN_ICON = ImageUtil.getIcon(DV_BOOLEAN_NAME);
	public static final ImageIcon DV_PROPORTION_ICON = ImageUtil.getIcon(DV_PROPORTION_NAME);

	public static final String SECTION_NAME = "section.png";
	public static final String CLUSTER_NAME = "cluster.png";
	public static final String ACTIVITY_NAME = "activity.png";
	public static final String SLOT_NAME = "slot.png";
	public static final String STRUCTURE_NAME = "structure.png";
	public static final String ITEM_TREE_NAME = "structure.png";
	public static final String ITEM_LIST_NAME = "structure.png";
	public static final String ITEM_TABLE_NAME = "structure.png";
	public static final String ITEM_SINGLE_NAME = "structure.png";
	public static final String ELEMENT_NAME = "element.png";
	public static final String ARCHETYPE_NAME = "archetype.png";
	public static final String TEMPLATE_NAME = "template.png";
	
	public static final ImageIcon SECTION = ImageUtil.getIcon(SECTION_NAME);
	public static final ImageIcon CLUSTER = ImageUtil.getIcon(CLUSTER_NAME);
	public static final ImageIcon ACTIVITY = ImageUtil.getIcon(ACTIVITY_NAME);
	public static final ImageIcon SLOT = ImageUtil.getIcon(SLOT_NAME);
	public static final ImageIcon STRUCTURE = ImageUtil.getIcon(STRUCTURE_NAME);
	public static final ImageIcon ITEM_TREE = ImageUtil.getIcon(ITEM_TREE_NAME);
	public static final ImageIcon ITEM_LIST = ImageUtil.getIcon(ITEM_LIST_NAME);
	public static final ImageIcon ITEM_TABLE = ImageUtil.getIcon(ITEM_TABLE_NAME);
	public static final ImageIcon ITEM_SINGLE = ImageUtil.getIcon(ITEM_SINGLE_NAME);
	public static final ImageIcon ELEMENT = ImageUtil.getIcon(ELEMENT_NAME);
	public static final ImageIcon ARCHETYPE = ImageUtil.getIcon(ARCHETYPE_NAME);
	public static final ImageIcon TEMPLATE = ImageUtil.getIcon(TEMPLATE_NAME);
	
	
	public static final ImageIcon EHR_LABEL_ICON = ImageUtil.getIcon("ehr_label.png");
	public static final ImageIcon KB_LABEL_ICON = ImageUtil.getIcon("kb_label.png");
	public static final ImageIcon CDS_LABEL_ICON = ImageUtil.getIcon("cds_label.png");
	public static final ImageIcon ANY_LABEL_ICON = ImageUtil.getIcon("any_label.png");
	
	public static final ImageIcon CALENDAR_ICON = ImageUtil.getIcon("calendar.png");
	public static final ImageIcon STOP_ICON = ImageUtil.getIcon("stop.png");
	public static final ImageIcon LIGHTNING_ICON = ImageUtil.getIcon("lightning.png");
	public static final ImageIcon RULE_ICON = ImageUtil.getIcon("rule.png");
	public static final ImageIcon TEST_ICON = ImageUtil.getIcon("tick.png");
	
	public static final ImageIcon ACCEPT_ICON = ImageUtil.getIcon("accept.png");
	public static final ImageIcon UNACCEPT_ICON = ImageUtil.getIcon("unaccept.png");
	public static final ImageIcon HALF_ACCEPT_ICON = ImageUtil.getIcon("half_accept.png");
	public static final ImageIcon EMPTY_ICON = ImageUtil.getIcon("empty.png");
	public static final ImageIcon EXPAND_ICON = ImageUtil.getIcon("expand.png");
	public static final ImageIcon CONTRACT_ICON = ImageUtil.getIcon("contract.png");
	public static final ImageIcon CLEAR_ICON = ImageUtil.getIcon("clear.png");
	public static final ImageIcon CANCEL_ICON = ImageUtil.getIcon("cancel.png");
	public static final ImageIcon SAVE_ICON = ImageUtil.getIcon("disk.png");
	public static final ImageIcon WARNING_ICON = ImageUtil.getIcon("warning.png");
	public static final ImageIcon ERROR_ICON = ImageUtil.getIcon("error.png");
	
	public static final ImageIcon SEARCH_ICON = ImageUtil.getIcon("magnifier.png");
	public static final ImageIcon ADD_ICON = ImageUtil.getIcon("add.png");
	public static final ImageIcon DELETE_ICON = ImageUtil.getIcon("delete.png");
	public static final ImageIcon FOLDER_OBJECT_ICON = ImageUtil.getIcon("folder_object.png");
	public static final ImageIcon FOLDER_ICON = ImageUtil.getIcon("folder.png");
	public static final ImageIcon SOURCE_ICON = ImageUtil.getIcon("source.gif");
	public static final ImageIcon OVERVIEW_ICON = ImageUtil.getIcon("layout_content.png");
	

	/* Aggregation functions */
	public static final ImageIcon AF_ALL_ICON = ImageUtil.getIcon("af_all.png");
	public static final ImageIcon AF_DURATION_ICON = ImageUtil.getIcon("af_duration.png");
	public static final ImageIcon AF_LAST_ICON = ImageUtil.getIcon("af_last.png");
	
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
		URL url = ImageUtil.class.getResource(imagePath);
		if (url != null)  {
			return new ImageIcon(url);
		}
		return null;
	}

	public static URL getImageURL(String image){
		return ImageUtil.class.getResource(IMAGE_DIR+image);
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
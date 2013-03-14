package se.cambio.cds.gdl.editor.view.util;

import java.awt.Window;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.jxpath.JXPathContext;

import se.cambio.cds.gdl.editor.util.LanguageManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.GuideImporter;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.openehr.util.ExceptionHandler;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;

public class ExportUtils {

    private static String TR_TD_FONT_OPEN = "<tr valign='top'><td><font face='Calibri'>";
    private static String TR_TD_FONT_OPEN_WITH_BG = "<tr bgcolor='#dbe5f1' valign='top'><td><font face='Calibri'>";
    private static String TD_FONT_CLOSE_TD_FONT_OPEN = "</font></td><td align='left'><font face='Calibri'>";
    private static String FONT_TD_TR_CLOSE = "</font></td></tr>";

    public static void exportToHTML(Window owner, Guide guide, String lang){
	JFileChooser fileChooser = new JFileChooser();
	FileNameExtensionFilter filter = new FileNameExtensionFilter(
		"HTML",new String[]{"html"});
	fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ExportToHTML"));
	fileChooser.setFileFilter(filter);
	File selectedFile = new File(guide.getId()+".html");
	fileChooser.setSelectedFile(selectedFile);
	int result = fileChooser.showSaveDialog(owner);
	if (result != JFileChooser.CANCEL_OPTION){
	    try{
		selectedFile = fileChooser.getSelectedFile();
		FileWriter fstream = new FileWriter(selectedFile);
		BufferedWriter out = new BufferedWriter(fstream);
		out.write(convertToHTML(guide, lang));
		out.close();
	    }catch(IOException e){
		ExceptionHandler.handle(e);
	    }
	}
    }

    @SuppressWarnings("unchecked")
    public static String convertToHTML(Guide guide, String lang){
	TermDefinition td = guide.getOntology().getTermDefinitions().get(lang);
	JXPathContext c = JXPathContext.newContext(guide.getDescription());
	StringBuffer sb = new StringBuffer();
	sb.append("<HTML>");
	sb.append("<TITLE>"+guide.getId()+"</TITLE>");

	sb.append("<b><font face='Calibri' size='6'>"+getTermText(guide.getConcept(), td)+"</font></b>");
	sb.append(getBoxWithTitleStart(LanguageManager.getMessage("GuideDetails")));
	sb.append("<table><font face='Calibri'>");
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Description")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getTermDescription(guide.getConcept(), td)+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Purpose")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/details/"+lang+"/purpose")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Use")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/details/"+lang+"/use")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Misuse")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/details/"+lang+"/misuse")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("References")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/otherDetails/references")+FONT_TD_TR_CLOSE);
	sb.append("</font></table>");
	sb.append(getBoxWithTitleStart(LanguageManager.getMessage("AuthorDetails")));
	sb.append("<table><font face='Calibri'>");
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Name")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/originalAuthor/name")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Email")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/originalAuthor/email")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Organisation")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/originalAuthor/organisation")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Date")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/originalAuthor/date")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("AuthorshipLyfecycle")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/lifecycleState")+FONT_TD_TR_CLOSE);
	sb.append(TR_TD_FONT_OPEN+"<b>"+LanguageManager.getMessage("Copyright")+":</b>"+TD_FONT_CLOSE_TD_FONT_OPEN+getValue(c,"/details/"+lang+"/copyright")+FONT_TD_TR_CLOSE);
	sb.append("</font></table>");

	List<String> keywords = (List<String>)c.getValue("/details/"+lang+"/keywords");
	if (!keywords.isEmpty()){
	    sb.append(getBoxWithTitleStart(LanguageManager.getMessage("Keywords")));

	    sb.append("<font face='Calibri'><i>");
	    boolean first = true;
	    for (String keyword : keywords) {
		if (!first){
		    sb.append(", ");
		}
		sb.append(keyword);
		first = false;
	    }
	    sb.append("</i></font><br>");
	}
	
	List<String> contributors = (List<String>)c.getValue("/otherContributors");
	if (!contributors.isEmpty()){
	    sb.append(getBoxWithTitleStart(LanguageManager.getMessage("Contributors")));
	    boolean first = true;
	    sb.append("<font face='Calibri'><i>");
	    for (String contributor : contributors) {
		if (!first){
		    sb.append(", ");
		}
		sb.append(contributor);
		first = false;
	    }
	    sb.append("</i></font><br>");
	}

	ReadableGuide readableGuide = GuideImporter.importGuide(guide, lang);

	if (!readableGuide.getPreconditionRuleLines().isEmpty()){
	    sb.append(getBoxWithTitleStart(LanguageManager.getMessage("Preconditions")));
	    sb.append("<table width=100%>");
	    for (RuleLine ruleLine : readableGuide.getPreconditionRuleLines()) {
		sb.append(TR_TD_FONT_OPEN_WITH_BG);
		sb.append(ruleLine.toHTMLString());
		sb.append(FONT_TD_TR_CLOSE);
	    }
	    sb.append("</table><br>");
	}

	
	if (!readableGuide.getReadableRules().isEmpty()){
	    sb.append(getBoxWithTitleStart(LanguageManager.getMessage("RuleList")));
	    sb.append("<table width=100%>");
	    for (ReadableRule readableRule : readableGuide.getReadableRules().values()) {
		sb.append(TR_TD_FONT_OPEN_WITH_BG);
		sb.append(readableRule);
		sb.append("</font></tr><tr><td></td></tr>"); 
	    }
	    sb.append("</table>");
	}
	sb.append("</HTML>");
	return sb.toString();
    }

    private static String getTermText(String gtCode, TermDefinition td){
	String text = td.getTerms().get(gtCode).getText();
	if (text!=null){
	    return text;
	}else{
	    return "";
	}
    }

    private static String getTermDescription(String gtCode, TermDefinition td){
	String desc = td.getTerms().get(gtCode).getDescription();
	if (desc!=null){
	    return desc;
	}else{
	    return "";
	}
    }

    private static String getBoxWithTitleStart(String title){
	StringBuffer sb = new StringBuffer();
	sb.append("<br><div style='background-color:#4f81bd; padding:5px'><font size='+1' face='Calibri' color='white'><b>"+title.toUpperCase()+"</b></font></div>");
	return  sb.toString();
    }

    public static String getValue(JXPathContext c, String path){
	String str = (String)c.getValue(path);
	return str!=null?str:"";
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
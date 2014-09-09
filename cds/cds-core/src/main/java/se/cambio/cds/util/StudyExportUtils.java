package se.cambio.cds.util;

import org.apache.commons.jxpath.JXPathContext;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.study.GTCodeReference;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.model.study.StudyDefinition;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.cds.util.misc.CDSLanguageManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2014-09-06
 * Time: 11:30
 */
public class StudyExportUtils {

    private static String TR_TD_FONT_OPEN = "<tr valign='top'><td style='font-family: Calibri;'>";
    private static String TR_TD_FONT_OPEN_WITH_BG = "<tr style='background-color: #dbe5c4; font-family: Calibri;' valign='top'><td>";
    private static String TR_TD_FONT_OPEN_WITH_BG2 = "<tr style='background-color: #92b842; font-family: Calibri;' valign='top'><td>";
    private static String TD_FONT_CLOSE_TD_FONT_OPEN = "</td><td style='font-family: Calibri;' align='left'>";
    private static String FONT_TD_TR_CLOSE = "</font></td></tr>";

    public static void exportToHTML(Window owner, Study study, String lang){
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter("HTML",new String[]{"html"});
        fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ExportToHTML"));
        fileChooser.setFileFilter(filter);
        File selectedFile = new File(study.getStudyId()+".html");
        fileChooser.setSelectedFile(selectedFile);
        int result = fileChooser.showSaveDialog(owner);
        if (result != JFileChooser.CANCEL_OPTION){
            try{
                selectedFile = fileChooser.getSelectedFile();
                FileWriter fstream = new FileWriter(selectedFile);
                BufferedWriter out = new BufferedWriter(fstream);
                out.write(convertToHTML(study, lang));
                out.close();
            }catch(IOException e){
                ExceptionHandler.handle(e);
            }catch(InternalErrorException e){
                ExceptionHandler.handle(e);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public static String convertToHTML(Study study, String lang) throws InternalErrorException {
        JXPathContext c = JXPathContext.newContext(study.getResourceDescription());
        StudyDefinition studyDefinition = study.getStudyDefinitions().get(lang);
        StringBuffer sb = new StringBuffer();
        sb.append("<HTML>");
        sb.append("<HEAD><meta charset='UTF-8'>");
        sb.append("<TITLE>"+study.getStudyId()+"</TITLE>");
        sb.append("</HEAD>");
        sb.append("<BODY>");
        String name = studyDefinition.getName();
        sb.append("<b><font face='Calibri' size='6'>"+(name!=null?name:"")+"</font></b>");
        sb.append(getBoxWithTitleStart("Study details")); //TODO i18n
        sb.append("<table><font face='Calibri'>");
        String desc = studyDefinition.getDescription();
        sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Description")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+(desc!=null?desc:"")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Purpose")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/details/"+lang+"/purpose")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Use")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/details/"+lang+"/use")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Misuse")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/details/"+lang+"/misuse")+FONT_TD_TR_CLOSE);
        if (c.getValue("/otherDetails")!=null){
            sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("References")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/otherDetails/references")+FONT_TD_TR_CLOSE);
        }
        sb.append("</font></table>");
        sb.append(getBoxWithTitleStart(CDSLanguageManager.getMessage("AuthorDetails")));
        sb.append("<table><font face='Calibri'>");
        if (c.getValue("/originalAuthor")!=null){
            sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Name")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/originalAuthor/name")+FONT_TD_TR_CLOSE);
            sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Email")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/originalAuthor/email")+FONT_TD_TR_CLOSE);
            sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Organisation")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/originalAuthor/organisation")+FONT_TD_TR_CLOSE);
            sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Date")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/originalAuthor/date")+FONT_TD_TR_CLOSE);
        }
        sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("AuthorshipLifecycle")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/lifecycleState")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+CDSLanguageManager.getMessage("Copyright")+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+ExportUtils.getValue(c,"/details/"+lang+"/copyright")+FONT_TD_TR_CLOSE);
        sb.append("</font></table>");

        java.util.List<String> keywords = (java.util.List<String>)c.getValue("/details/"+lang+"/keywords");
        if (keywords!=null && !keywords.isEmpty()){
            sb.append(getBoxWithTitleStart(CDSLanguageManager.getMessage("Keywords")));

            sb.append("<font face='Calibri'><i>");
            boolean first = true;
            for (String keyword: keywords) {
                keyword = keyword.replace("\\\"","\"");
                if (!first){
                    sb.append(", ");
                }
                sb.append(keyword);
                first = false;
            }
            sb.append("</i></font><br>");
        }

        List<String> contributors = (List<String>)c.getValue("/otherContributors");
        if (contributors!=null && !contributors.isEmpty()){
            sb.append(getBoxWithTitleStart(CDSLanguageManager.getMessage("Contributors")));
            boolean first = true;
            sb.append("<font face='Calibri'><i>");
            for (String contributor: contributors) {
                contributor = contributor.replace("\\\"","\"");
                if (!first){
                    sb.append(", ");
                }
                sb.append(contributor);
                first = false;
            }
            sb.append("</i></font><br>");
        }
        sb.append(getBoxWithTitleStart("Study Criteria")); //TODO i18n
        sb.append("<table><font face='Calibri'>");
        String inclusionCriteria = ExportUtils.formatToHTMLString(studyDefinition.getInclusionCriteria());
        String exclusionCriteria = ExportUtils.formatToHTMLString(studyDefinition.getExclusionCriteria());
        sb.append(TR_TD_FONT_OPEN+"<b>"+"Inclusion criteria"+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+(inclusionCriteria!=null?inclusionCriteria:"")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+"Exclusion criteria"+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+(exclusionCriteria!=null?exclusionCriteria:"")+FONT_TD_TR_CLOSE);
        sb.append("</font></table>");

        sb.append(getBoxWithTitleStart("Definition")); //TODO i18n
        String guideline = getGuidelines(study);
        String filters = getFilters(study, lang);
        String indicators = getIndicators(study, lang);
        sb.append("<table><font face='Calibri'>");
        sb.append(TR_TD_FONT_OPEN+"<b>"+"Guidelines"+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+(guideline!=null?guideline:"")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+"Filters"+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+(filters!=null?filters:"")+FONT_TD_TR_CLOSE);
        sb.append(TR_TD_FONT_OPEN+"<b>"+"Indicators"+": </b>"+TD_FONT_CLOSE_TD_FONT_OPEN+(indicators!=null?indicators:"")+FONT_TD_TR_CLOSE);
        sb.append("</font></table>");
        sb.append("</BODY>");
        sb.append("</HTML>");
        return sb.toString();
    }

    private static String getGuidelines(Study study){
        StringBuffer sb = new StringBuffer();
        String prefix = "";
        for(String guideId: study.getGuideIds()){
            sb.append(prefix);
            sb.append(guideId);
            prefix = ", ";
        }
        return sb.toString();
    }

    private static String getFilters(Study study, String lang){
        StringBuffer sb = new StringBuffer();
        String prefix = "";
        for(GTCodeReference gtCodeReference: study.getFilters()){
            sb.append(prefix);
            sb.append(getGTCodeText(gtCodeReference, lang));
            prefix = ", ";
        }
        return sb.toString();
    }

    private static String getIndicators(Study study, String lang){
        StringBuffer sb = new StringBuffer();
        String prefix = "";
        for(GTCodeReference gtCodeReference: study.getIndicators()){
            sb.append(prefix);
            sb.append(getGTCodeText(gtCodeReference, lang));
            prefix = ", ";
        }
        return sb.toString();
    }

    public static String getGTCodeText(GTCodeReference gtCodeReference, String lang){
        String text = null;
        try {
            text = getTermText(gtCodeReference, lang);
        } catch (GuideNotFoundException e) {
            ExceptionHandler.handle(e);
        }
        if (text==null){
            text = gtCodeReference.getGuideId()+"/"+gtCodeReference.getGtCode();
        }
        return text;
    }

    private static TermDefinition getTermDefinition(GTCodeReference gtCodeReference, String lang) throws GuideNotFoundException {
        Guide guide = Guides.getGuide(gtCodeReference.getGuideId());
        TermDefinition td = guide.getOntology().getTermDefinitions().get(lang);
        if (td == null || td.getTerms().get(gtCodeReference.getGtCode()) == null){
            String originalLang = guide.getLanguage().getOriginalLanguage().getCodeString();
            td =  guide.getOntology().getTermDefinitions().get(originalLang);
        }
        return td;
    }

    private static String getTermText(GTCodeReference gtCodeReference, String lang) throws GuideNotFoundException {
        TermDefinition td = getTermDefinition(gtCodeReference, lang);
        return ExportUtils.getTermText(gtCodeReference.getGtCode(), td);
    }

    private static String getBoxWithTitleStart(String title){
        StringBuffer sb = new StringBuffer();
        sb.append("<br><div style='background-color: #92b842; padding: 5px'><font size='+1' face='Calibri' color='white'><b>"+title.toUpperCase()+"</b></font></div>");
        return  sb.toString();
    }
}

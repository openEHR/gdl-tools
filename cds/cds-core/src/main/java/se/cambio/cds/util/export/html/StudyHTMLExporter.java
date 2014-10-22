package se.cambio.cds.util.export.html;

import freemarker.ext.beans.StringModel;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.study.GTCodeReference;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class StudyHTMLExporter extends ClinicalModelHTMLExporter<Study> {

    public StudyHTMLExporter(Study entity, String lang) {
        super(entity, lang);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("study", getEntity());
        objectMap.put("study_description", getEntity().getResourceDescription().getDetails().get(getLanguage()));
        objectMap.put("study_definitions", getEntity().getStudyDefinitions().get(getLanguage()));
        objectMap.put("getGTCodeText", new GetGTCodeReferenceTextMethod());
        return objectMap;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        Map<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "StudyDetails");
        addText(textsMap, "StudyCriteria");
        addText(textsMap, "InclusionCriteria");
        addText(textsMap, "ExclusionCriteria");
        addText(textsMap, "StudyDefinition");
        addText(textsMap, "Guidelines");
        addText(textsMap, "Filters");
        addText(textsMap, "Indicators");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return StudyHTMLExporter.class.getClassLoader().getResourceAsStream("study.ftl");
    }

    public static String getGTCodeText(GTCodeReference gtCodeReference, String lang){
        String text = null;
        try {
            text = getTermText(gtCodeReference, lang);
        } catch (InstanceNotFoundException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
        if (text==null){
            text = gtCodeReference.getGuideId()+"/"+gtCodeReference.getGtCode();
        }
        return text;
    }

    private static TermDefinition getTermDefinition(GTCodeReference gtCodeReference, String lang) throws InternalErrorException, InstanceNotFoundException {
        Guide guide = Guides.getInstance().getGuide(gtCodeReference.getGuideId());
        TermDefinition td = guide.getOntology().getTermDefinitions().get(lang);
        if (td == null || td.getTerms().get(gtCodeReference.getGtCode()) == null){
            String originalLang = guide.getLanguage().getOriginalLanguage().getCodeString();
            td =  guide.getOntology().getTermDefinitions().get(originalLang);
        }
        return td;
    }

    private static String getTermText(GTCodeReference gtCodeReference, String lang) throws InstanceNotFoundException, InternalErrorException {
        TermDefinition td = getTermDefinition(gtCodeReference, lang);
        return td.getTermText(gtCodeReference.getGtCode());
    }



    public class GetGTCodeReferenceTextMethod implements TemplateMethodModelEx {

        public TemplateModel exec(List args) throws TemplateModelException {
            if (args.size() != 1 || !(((StringModel)args.get(0)).getWrappedObject() instanceof GTCodeReference)) {
                throw new TemplateModelException("Wrong arguments");
            }
            GTCodeReference gtCodeReference = (GTCodeReference)((StringModel)args.get(0)).getWrappedObject();
            return new SimpleScalar(getGTCodeText(gtCodeReference, getLanguage()));
        }
    }
}

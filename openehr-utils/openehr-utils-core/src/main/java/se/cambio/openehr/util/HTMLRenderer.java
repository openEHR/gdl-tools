package se.cambio.openehr.util;

import freemarker.template.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-10
 * Time: 17:09
 */
public class HTMLRenderer {
    Configuration cfg = new Configuration();
    Template template = null;

    public HTMLRenderer(Reader templateReader) throws InternalErrorException {
        cfg.setObjectWrapper(new DefaultObjectWrapper());
        cfg.setDefaultEncoding("UTF-8");
        cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
        cfg.setIncompatibleImprovements(new Version(2, 3, 20));  // FreeMarker 2.3.20
        try {
            template = new Template(null, templateReader, cfg);
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
    }

    public String proccess(Map<String, Object> model) throws InternalErrorException {
        try {
            if (template==null){
                throw new InternalErrorException(new Exception("No template defined!"));
            }
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            Writer out = new OutputStreamWriter(baos);
            template.process(model, out);
            return baos.toString("UTF-8");
        } catch (IOException e) {
            throw new InternalErrorException(e);
        } catch (TemplateException e) {
            throw new InternalErrorException(e);
        }
    }
}

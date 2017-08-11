package se.cambio.openehr.util;

import freemarker.core.Environment;
import freemarker.template.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.util.Map;

public class HTMLRenderer {
    Template template = null;

    public HTMLRenderer(Reader templateReader) throws InternalErrorException {
        Configuration cfg = new Configuration();
        cfg.setObjectWrapper(new DefaultObjectWrapper());
        String encoding = "UTF-8";
        cfg.setDefaultEncoding(encoding);
        cfg.setOutputEncoding(encoding);
        cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
        cfg.setIncompatibleImprovements(new Version(2, 3, 20));  // FreeMarker 2.3.20
        try {
            template = new Template(null, templateReader, cfg);
        } catch (IOException ex) {
            throw new InternalErrorException(ex);
        }
    }

    public String process(Map<String, Object> model) throws InternalErrorException {
        try {
            if (template == null) {
                throw new InternalErrorException(new Exception("No template defined!"));
            }
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            Writer writer = new OutputStreamWriter(baos, "UTF-8");
            Environment env = template.createProcessingEnvironment(model, writer);
            env.setURLEscapingCharset("UTF-8");
            env.setOutputEncoding("UTF-8");
            env.process();
            return baos.toString("UTF-8");
        } catch (IOException | TemplateException ex) {
            throw new InternalErrorException(ex);
        }
    }
}

package se.cambio.cm.configuration;


import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.PropertySources;
import org.springframework.core.env.Environment;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Configuration
@PropertySources({
        @PropertySource(value = "classpath:default-terminology-service-config.properties"),
        @PropertySource(value = "file:${CDS_CONFIG_DIR:/opt/cds-config}/terminology-service-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:conf/terminology-service-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "classpath:terminology-service-config.properties", ignoreResourceNotFound = true)
})
@ComponentScan("se.cambio.cm.controller.terminology")
public class TerminologyServiceConfiguration {

    private static TerminologyServiceConfiguration _delegate = null;
    private Set<CodePhrase> supportedLanguages;
    private Map<String, String> terminologyPluginSourcesClassMap;
    private Map<String, String> terminologyURLs;
    private static Logger log = Logger.getLogger(TerminologyServiceConfiguration.class);

    @Autowired
    Environment environment;
    private static final String URL_POSTFIX = "-url";
    private static final String PLUGIN_CLASS_POSTFIX = "-plugin.class";

    public TerminologyServiceConfiguration() {
        // TODO hardcoded language supports
        this.supportedLanguages = new HashSet<CodePhrase>();
        this.supportedLanguages.add(new CodePhrase("ISO_639-1", "en"));
        this.supportedLanguages.add(new CodePhrase("ISO_639-1", "sv"));
        this.supportedLanguages.add(new CodePhrase("ISO_639-1", "es"));
        this.terminologyURLs = new HashMap<String, String>();
        this.terminologyPluginSourcesClassMap = new HashMap<String, String>();
    }

    public boolean languageSupported(CodePhrase language) {
        return supportedLanguages.contains(language);
    }

    public String terminologyURL(String terminologyId) {
        if (!terminologyURLs.containsKey(terminologyId)) {
            loadFromProperties(terminologyId);
        }
        return terminologyURLs.get(terminologyId);
    }

    public String getPluginSourceClass(String terminologyId) {
        if (!terminologyPluginSourcesClassMap.containsKey(terminologyId)) {
            loadFromProperties(terminologyId);
        }
        return terminologyPluginSourcesClassMap.get(terminologyId);
    }

    private void loadFromProperties(String terminologyId) {
        String url = environment.getProperty(terminologyId + URL_POSTFIX);
        String pluginClass = environment.getProperty(terminologyId + PLUGIN_CLASS_POSTFIX);
        terminologyURLs.put(terminologyId, url);
        terminologyPluginSourcesClassMap.put(terminologyId, pluginClass);
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
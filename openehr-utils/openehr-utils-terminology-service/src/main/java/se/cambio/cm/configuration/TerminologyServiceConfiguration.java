package se.cambio.cm.configuration;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.*;
import org.springframework.core.env.Environment;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.controller.terminology.TerminologyServiceImpl;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.facade.configuration.ClinicalModelsConfiguration;
import se.cambio.cm.util.TerminologyConfigVO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.HashMap;
import java.util.Map;

@Configuration
@PropertySources({
        @PropertySource(value = "classpath:default-terminology-service-config.properties"),
        @PropertySource(value = "file:${CDS_CONFIG_DIR:/opt/cds-config}/terminology-service-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:conf/terminology-service-config.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "classpath:terminology-service-config.properties", ignoreResourceNotFound = true)})
@Import({UserConfigurationManager.class, ClinicalModelsConfiguration.class})
public class TerminologyServiceConfiguration {

    private Map<String, TerminologyConfigVO> terminologyConfigMap;
    @Autowired
    private Environment environment;

    private static final String TERMINOLOGY_PROPERTY_PREFIX = "terminologies.";
    private static final String CLEAN_CODES_PROPERTY_POSTFIX = ".clean-codes";
    private static final String SIMPLE_PARENT_CHECK_PROPERTY_POSTFIX = ".simple-parent-check";
    private static final String CODE_EXISTENCE_CHECK_PROPERTY_POSTFIX = ".code-existence-check";
    private static final String CLASS_PROPERTY_POSTFIX = ".class";
    private TerminologyService terminologyService;
    private static TerminologyServiceConfiguration instance;


    public TerminologyServiceConfiguration() {
        this.terminologyConfigMap = new HashMap<>();
    }

    public TerminologyConfigVO getTerminologyConfig(String terminologyId) {
        return terminologyConfigMap.computeIfAbsent(terminologyId, this::getTerminologyConfigFromProperties);
    }

    @Bean
    public TerminologyService terminologyService(ClinicalModelsService clinicalModelsService) {
        TerminologyServiceImpl terminologyService = new TerminologyServiceImpl(this, clinicalModelsService);
        getInstance().terminologyService = terminologyService;
        return terminologyService;
    }

    public static TerminologyService getTerminologyServiceInstance() {
        return getInstance().terminologyService;
    }

    private TerminologyConfigVO getTerminologyConfigFromProperties(String terminologyId) {
        String terminologyPrefix = TERMINOLOGY_PROPERTY_PREFIX + terminologyId;
        Boolean simpleParentCheck = environment.getProperty(terminologyPrefix + SIMPLE_PARENT_CHECK_PROPERTY_POSTFIX, Boolean.class, false);
        Boolean codeExistenceCheck = environment.getProperty(terminologyPrefix + CODE_EXISTENCE_CHECK_PROPERTY_POSTFIX, Boolean.class, true);
        Boolean cleanCodes = environment.getProperty(terminologyPrefix + CLEAN_CODES_PROPERTY_POSTFIX, Boolean.class, false);
        String clazz = environment.getProperty(terminologyPrefix + CLASS_PROPERTY_POSTFIX);
        return new TerminologyConfigVO(terminologyId, simpleParentCheck, codeExistenceCheck, cleanCodes, clazz);
    }

    private static TerminologyServiceConfiguration getInstance() {
        if (instance == null) {
            instance = new TerminologyServiceConfiguration();
        }
        return instance;
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
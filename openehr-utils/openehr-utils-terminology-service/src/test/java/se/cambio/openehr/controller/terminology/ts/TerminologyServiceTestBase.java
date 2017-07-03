package se.cambio.openehr.controller.terminology.ts;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.testng.AbstractTestNGSpringContextTests;
import org.testng.annotations.BeforeClass;
import se.cambio.cm.configuration.CmPersistenceConfig;
import se.cambio.cm.configuration.CmServiceConfiguration;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;
import se.cambio.cm.controller.terminology.TerminologyServiceImpl;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

@ContextConfiguration
@ActiveProfiles("cm-admin-file-dao")
public class TerminologyServiceTestBase extends AbstractTestNGSpringContextTests {

    protected static final String ICD10 = "ICD10";
    static final String TEST_TERMINOLOGY = "TEST-TERMINOLOGY";
    static final String MULTI_LANG_TEST_TERMINOLOGY = "MULTI-LANG-TEST-TERMINOLOGY";

    @Autowired
    TerminologyServiceImpl terminologyService;

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @Value("classpath:/terminologies")
    private Resource terminologies;

    @BeforeClass
    public void loadCM() throws InternalErrorException, URISyntaxException, IOException {
        BeanProvider.setActiveProfiles("cm-admin-file-dao");
        userConfigurationManager.setTerminologiesFolderPath(terminologies.getURL().getPath());
    }

    @Configuration
    @Import({CmPersistenceConfig.class, CmServiceConfiguration.class, TerminologyServiceConfiguration.class})
    static class Config {
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
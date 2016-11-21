package se.cambio.openehr.controller.terminology.ts;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cm.controller.terminology.TerminologyServiceImpl;
import se.cambio.openehr.util.BeanProvider;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.net.URISyntaxException;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = TerminologyTestConfig.class)
public class TerminologyServiceTestBase {

    protected static final CodePhrase EN = new CodePhrase("ISO_639-1", "en");
    protected static final CodePhrase SV = new CodePhrase("ISO_639-1", "sv");
    protected static final String SCT = "SNOMED-CT";
    protected static final String ICD10 = "ICD10";

    @Autowired
    protected TerminologyServiceImpl terminologyService;

    @Before
    public void loadCM() throws InternalErrorException, URISyntaxException, IOException {
        BeanProvider.setActiveProfiles("cm-admin-dummy-service", "cm-admin-file-dao");
        UserConfigurationManager.instance().setTerminologiesFolderPath(TerminologyServiceTestBase.class.getClassLoader().getResource("terminologies1").toURI().getPath());
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
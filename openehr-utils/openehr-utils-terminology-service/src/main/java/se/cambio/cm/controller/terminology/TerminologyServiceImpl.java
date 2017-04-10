package se.cambio.cm.controller.terminology;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;
import se.cambio.cm.controller.terminology.plugins.CSVTerminologyServicePlugin;
import se.cambio.cm.controller.terminology.plugins.TerminologyServicePlugin;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.util.TerminologyNodeVO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.cm.util.TerminologyConfigVO;
import se.cambio.cm.util.exceptions.UnsupportedTerminologyException;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.*;

import static java.lang.String.format;

public class TerminologyServiceImpl implements TerminologyService {

    private Long lastUpdate = null;
    private static final long MAX_INTERVAL_BEFORE_UPLOAD = 5000;
    private Map<String, TerminologyService> terminologyPlugins;
    private Set<String> supportedTerminologies = null;
    private TerminologyServiceConfiguration terminologyServiceConfiguration;
    private ClinicalModelsService clinicalModelsService;
    private static Logger log = LoggerFactory.getLogger(TerminologyServiceImpl.class);

    public TerminologyServiceImpl(TerminologyServiceConfiguration terminologyServiceConfiguration, ClinicalModelsService clinicalModelsService) {
        this.terminologyServiceConfiguration = terminologyServiceConfiguration;
        this.clinicalModelsService = clinicalModelsService;
    }

    private TerminologyServicePlugin generateTerminologyService(TerminologyDTO terminologyDTO) {
        try {
            log.debug("Loading terminology : " + terminologyDTO.getId());
            InputStream input = new ByteArrayInputStream(terminologyDTO.getSource().getBytes("UTF8"));
            TerminologyConfigVO terminologyConfig = terminologyServiceConfiguration.getTerminologyConfig(terminologyDTO.getId());
            TerminologyServicePlugin terminologyServicePlugin;
            String clazz = terminologyConfig.getClazz();
            if (clazz != null) {
                try {
                    terminologyServicePlugin = (TerminologyServicePlugin) Class.forName(clazz).newInstance();
                } catch (Exception e) {
                    throw new RuntimeException("ERROR instantiating class '" + clazz + "'", e);
                }
            } else {
                terminologyServicePlugin = new CSVTerminologyServicePlugin(terminologyConfig);
            }
            if (terminologyServicePlugin != null) {
                terminologyServicePlugin.init(input);
            }
            return terminologyServicePlugin;
        } catch (Exception e) {
            throw new RuntimeException("Failed to load terminology '" + terminologyDTO.getId() + "'");
        }
    }

    public boolean isSubclassOf(CodePhrase a, CodePhrase b) {

        log.debug("Checking isSubclassOf (" + a + ", " + b + ")");

        checkTerminologySupported(a);
        checkTerminologySupported(b);

        String terminologyId = a.getTerminologyId().getValue();
        boolean ret = getTerminologyServicePlugin(terminologyId).isSubclassOf(a, b);

        log.debug("isSubclassOf: " + ret);
        return ret;
    }

    public boolean isSubclassOf(CodePhrase code, Set<CodePhrase> codes) {
        log.debug("Checking isSubclassOf (" + code + ", " + codes + ")");
        checkTerminologySupported(code);
        for (CodePhrase cp : codes) {
            checkTerminologySupported(cp);
        }
        String terminologyId = code.getTerminologyId().getValue();
        log.debug("Checking isSubclassOf using classification..");
        boolean ret = getTerminologyServicePlugin(terminologyId).isSubclassOf(code, codes);
        log.debug("isSubclassOf: " + ret);
        return ret;
    }

    public boolean isTerminologySupported(String terminologyId) {
        return getSupportedTerminologiesI().contains(terminologyId);
    }

    private void checkTerminologySupported(CodePhrase code) {
        checkTerminologySupported(code.getTerminologyId().getValue());
    }

    private void checkTerminologySupported(String terminology) {
        if (!isTerminologySupported(terminology)) {
            throw new UnsupportedTerminologyException(format("Unsupported terminology %s", terminology));
        }
    }

    public boolean isTerminologySupported(CodePhrase code) {
        return isTerminologySupported(code.getTerminologyId().getValue());
    }

    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language) {
        log.debug("retrieve all subclasses of " + concept);
        String terminologyId = concept.getTerminologyId().getValue();
        TerminologyService ts = getTerminologyServicePlugin(terminologyId);

        TerminologyNodeVO node;
        if (ts != null) {
            node = ts.retrieveAllSubclasses(concept, language);
        } else {
            throw new UnsupportedTerminologyException(format("Unsupported terminology %s", terminologyId));
        }
        return node;
    }

    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language) {
        TerminologyService ts = getTerminologyServicePlugin(terminologyId);
        if (ts != null) {
            return ts.retrieveAll(terminologyId, language);
        } else {
            throw new UnsupportedTerminologyException(format("Unsupported terminology %s", terminologyId));
        }
    }

    public String retrieveTerm(CodePhrase concept, CodePhrase language) {

        String terminologyId = concept.getTerminologyId().getValue();
        TerminologyService ts = getTerminologyServicePlugin(terminologyId);
        if (ts != null) {
            return ts.retrieveTerm(concept, language);
        } else {
            throw new UnsupportedTerminologyException(format("Unsupported terminology %s", terminologyId));
        }
    }

    public boolean isValidCodePhrase(CodePhrase codePhrase) {
        String terminologyId = codePhrase.getTerminologyId().getValue();
        TerminologyService ts = getTerminologyServicePlugin(terminologyId);
        return ts != null && ts.isValidCodePhrase(codePhrase);
    }

    private TerminologyService getTerminologyServicePlugin(String terminologyId) {
        TerminologyService terminologyService = getTerminologyServicePluginMap().get(terminologyId);
        if (terminologyService == null && isSupported(terminologyId)) {
            try {
                Collection<TerminologyDTO> terminologyDTOs = clinicalModelsService.searchCMElementsByIds(TerminologyDTO.class, Collections.singleton(terminologyId));
                TerminologyDTO terminologyDTO = terminologyDTOs.iterator().next();
                terminologyService = generateTerminologyService(terminologyDTO);
                getTerminologyServicePluginMap().put(terminologyId, terminologyService);
            } catch (InstanceNotFoundException e) {
                ExceptionHandler.handle(e);
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return terminologyService;
    }

    private Map<String, TerminologyService> getTerminologyServicePluginMap() {
        if (terminologyPlugins == null) {
            terminologyPlugins = new HashMap<>();
        }
        return terminologyPlugins;
    }

    private boolean isSupported(String terminologyId) {
        return getSupportedTerminologiesI().contains(terminologyId);
    }

    private Collection<String> getSupportedTerminologiesI() {
        if (supportedTerminologies == null) {
            supportedTerminologies = new HashSet<>();
        }
        if (shouldUpdateSupportedTerminologyIds()) {
            try {
                supportedTerminologies.clear();
                supportedTerminologies.addAll(clinicalModelsService.getAllCMElementIds(TerminologyDTO.class));
                lastUpdate = System.currentTimeMillis();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return supportedTerminologies;
    }

    public Collection<String> getSupportedTerminologies() {
        return Collections.unmodifiableCollection(getSupportedTerminologiesI());
    }

    private boolean shouldUpdateSupportedTerminologyIds() {
        long currentTimeMinusWaitInterval = System.currentTimeMillis() - MAX_INTERVAL_BEFORE_UPLOAD;
        return lastUpdate == null || lastUpdate < currentTimeMinusWaitInterval;
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
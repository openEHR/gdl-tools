package se.cambio.cm.controller.terminology.plugins;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.cm.util.TerminologyConfigVO;
import se.cambio.openehr.util.exceptions.*;
import se.cambio.openehr.util.misc.CSVReader;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

import static java.lang.String.format;
import static org.apache.commons.lang.StringUtils.isEmpty;

public class CSVTerminologyServicePlugin implements TerminologyServicePlugin {

    private Map<String, ArrayList<String>> parentsMap = null;
    private Map<String, ArrayList<String>> childrenMap = null;
    private Map<String, String> descriptionsMap = null;
    private static Logger log = LoggerFactory.getLogger(CSVTerminologyServicePlugin.class);
    private TerminologyConfigVO terminologyConfig;

    public CSVTerminologyServicePlugin(TerminologyConfigVO terminologyConfig) {
        this.terminologyConfig = terminologyConfig;
    }

    @Override
    public void init(InputStream is) {
        try {
            CSVReader csvReader = new CSVReader(new BufferedReader(new InputStreamReader(is, "UTF-8")));
            parentsMap = new HashMap<>();
            childrenMap = new HashMap<>();
            descriptionsMap = new HashMap<>();
            csvReader.readHeaders();
            processCSV(csvReader);
        } catch (Exception e) {
            String message = format("Failed to initialize the terminology service '%s'", terminologyConfig.getTerminologyId());
            throw new RuntimeException(message, e);
        }
    }

    private void processCSV(CSVReader csvReader) throws IOException {
        while (csvReader.readRecord()) {
            String id = csvReader.get("id");
            String description = csvReader.get("text");
            String parent = csvReader.get("parent");
            log.debug("id: " + id + ", description: " + description);
            addTerm(id, description, parent);
        }
        log.debug("Total " + descriptionsMap.size() + " term(s) loaded..");
    }

    @Override
    public String getTerminologyId() {
        return terminologyConfig.getTerminologyId();
    }

    @Override
    public boolean isSubclassOf(CodePhrase a, CodePhrase b)
            throws UnsupportedTerminologyException, InvalidCodeException {
        checkTerminologySupported(a);
        checkTerminologySupported(b);
        return checkSubclassOf(a, b);
    }

    @Override
    public boolean isSubclassOf(CodePhrase code, Set<CodePhrase> codes)
            throws UnsupportedTerminologyException, InvalidCodeException {
        checkTerminologySupported(code);
        boolean ret = false;
        for (CodePhrase cp : codes) {
            if (!code.getTerminologyId().equals(cp.getTerminologyId())) {
                continue;
            }
            try {
                if (checkSubclassOf(code, cp)) {
                    ret = true;
                    break;
                }
            } catch (InvalidCodeException e) {
                log.warn(format("InvalidCodeException: checkSubclassOf('%s','%s') ignored. Message: %s", code, cp.getCodeString(), e.getMessage()));
            }
        }
        return ret;
    }

    @Override
    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException, InvalidCodeException {
        String code = concept.getCodeString();
        return retrieveAllSubclasses(code, language);
    }

    @Override
    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language) throws UnsupportedTerminologyException, UnsupportedLanguageException {
        return null;
    }

    @Override
    public boolean isTerminologySupported(String terminologyId) {
        return this.terminologyConfig.getTerminologyId().equalsIgnoreCase(terminologyId);
    }

    @Override
    public String retrieveTerm(CodePhrase concept, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException {
        String code = cleanUpCode(concept.getCodeString());
        return retrieveTerm(code, language);
    }

    @Override
    public Collection<String> getSupportedTerminologies() {
        Collection<String> supportedTerminologies = new ArrayList<>();
        supportedTerminologies.add(terminologyConfig.getTerminologyId());
        return supportedTerminologies;
    }

    @Override
    public boolean isValidCodePhrase(CodePhrase codePhrase) {
        return isValidTerminologyCode(codePhrase) && !invalidCode(codePhrase.getCodeString());
    }

    private void addTerm(String id, String description, String parent) {
        if (!isEmpty(id) && !isEmpty(description)) {
            descriptionsMap.put(id, description);
            addParent(id, parent);
        }
    }

    private void addParent(String id, String parent) {
        if (!isEmpty(parent)) {
            ArrayList<String> parents = new ArrayList<>();
            parents.add(parent);
            ArrayList<String> hierarchy = parentsMap.get(parent);
            if (hierarchy != null) {
                parents.addAll(hierarchy);
            }
            parentsMap.put(id, parents);
            ArrayList<String> children = childrenMap.computeIfAbsent(parent, k -> new ArrayList<>());
            children.add(id);
        }
    }

    private TerminologyNodeVO retrieveAllSubclasses(String code, CodePhrase language)
            throws UnsupportedTerminologyException, UnsupportedLanguageException {
        String cleanCode = cleanUpCode(code);
        TerminologyNodeVO node = getNodeForCode(cleanCode, language);
        ArrayList<String> children = childrenMap.get(cleanCode);
        if (children != null) {
            for (String childCode : children) {
                TerminologyNodeVO nodeAux = retrieveAllSubclasses(childCode, language);
                if (nodeAux != null) {
                    node.addChild(nodeAux);
                }
            }
        }
        return node;
    }

    private TerminologyNodeVO getNodeForCode(String code, CodePhrase language)
            throws UnsupportedTerminologyException, UnsupportedLanguageException {
        String desc = retrieveTerm(code, language);
        if (desc == null) {
            desc = code;
        }
        return new TerminologyNodeVO(new DvCodedText(desc, new CodePhrase(terminologyConfig.getTerminologyId(), code)));
    }


    private boolean checkSubclassOf(CodePhrase a, CodePhrase b)
            throws UnsupportedTerminologyException, InvalidCodeException {
        if (isValidTerminologyCode(a) && isValidTerminologyCode(b)) {
            String as = a.getCodeString();
            String bs = b.getCodeString();
            return checkSubclassOf(as, bs);
        } else {
            throw new UnsupportedTerminologyException(a.getTerminologyId() + " not supported");
        }
    }

    private boolean checkSubclassOf(String as, String bs)
            throws UnsupportedTerminologyException, InvalidCodeException {
        String cleanAS = cleanUpCode(as);
        if (invalidCode(cleanAS)) {
            throw new InvalidCodeException("Invalid " + terminologyConfig.getTerminologyId() + " code: " + as);
        }
        String cleanBS = cleanUpCode(bs);
        if (invalidCode(cleanBS)) {
            throw new InvalidCodeException("Invalid " + terminologyConfig.getTerminologyId() + " code: " + bs);
        }
        if (terminologyConfig.isSimpleParentCheck()) {
            return cleanAS.startsWith(cleanBS);
        } else {
            if (cleanAS.equals(cleanBS)) {
                return true;
            } else {
                ArrayList<String> parents = parentsMap.get(cleanAS);
                return (parents != null && parents.contains(cleanBS));
            }
        }
    }

    private void checkTerminologySupported(CodePhrase code)
            throws UnsupportedTerminologyException {
        checkTerminologySupported(code.getTerminologyId().getValue());
    }

    private void checkTerminologySupported(String terminology)
            throws UnsupportedTerminologyException {
        if (!isTerminologySupported(terminology)) {
            throw new UnsupportedTerminologyException(terminology + " not supported");
        }
    }

    boolean invalidCode(String code) {
        if (terminologyConfig.isCodeExistenceCheck()) {
            String cleanCode = cleanUpCode(code);
            return descriptionsMap.get(cleanCode) == null;
        } else {
            return false;
        }
    }

    private boolean isValidTerminologyCode(CodePhrase code) {
        return isTerminologySupported(code.getTerminologyId().getValue());
    }

    protected String retrieveTerm(String code, CodePhrase language) {
        String cleanCode = cleanUpCode(code);
        String description = descriptionsMap.get(cleanCode);
        return description != null ? description : "";
    }

    private String cleanUpCode(String code) {
        if (terminologyConfig.isCleanCodes()) {
            code = code.replace("-", "");
            code = code.replace(".", "");
            while (code.length() > 1 && invalidCode(code)) {
                code = code.substring(0, code.length() - 1);
            }
        }
        return code;
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
package se.cambio.cm.controller.terminology.plugins;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.lang.StringUtils;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.util.TerminologyNodeVO;
import se.cambio.cm.util.TerminologyConfigVO;
import se.cambio.cm.util.exceptions.InvalidCodeException;
import se.cambio.cm.util.exceptions.UnsupportedTerminologyException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

import static java.lang.String.format;
import static org.apache.commons.lang.StringUtils.isEmpty;

public class CSVTerminologyServicePlugin implements TerminologyServicePlugin {

    private Map<String, Set<String>> parentsMap = null;
    private Map<String, Set<String>> childrenMap = null;
    private Map<String, Map<String, String>> descriptionsMap = null;
    private static Logger log = LoggerFactory.getLogger(CSVTerminologyServicePlugin.class);
    private TerminologyConfigVO terminologyConfig;

    public CSVTerminologyServicePlugin(TerminologyConfigVO terminologyConfig) {
        this.terminologyConfig = terminologyConfig;
    }

    @Override
    public void init(InputStream is) {
        try {
            parentsMap = new HashMap<>();
            childrenMap = new HashMap<>();
            descriptionsMap = new HashMap<>();
            CSVParser csvParser =
                    new CSVParser(new BufferedReader(new InputStreamReader(is, "UTF-8")), CSVFormat.DEFAULT.withFirstRecordAsHeader().withIgnoreSurroundingSpaces());
            processCsv(csvParser);
        } catch (Exception ex) {
            String message = format("Failed to initialize the terminology service '%s'", terminologyConfig.getTerminologyId());
            throw new RuntimeException(message, ex);
        }
    }

    private void processCsv(CSVParser csvParser) throws IOException {
        for (CSVRecord csvRecord : csvParser.getRecords()) {
            if (!csvRecord.isSet("id")) {
                throw new RuntimeException(format("Error reading terminology '%s'. Term found without id!", terminologyConfig.getTerminologyId()));
            }
            String id = csvRecord.get("id");
            if (!csvRecord.isSet("text")) {
                throw new RuntimeException(format("Error reading terminology '%s'. Term '%s' does not have description!",
                        terminologyConfig.getTerminologyId(), id));
            }
            String description = csvRecord.get("text");
            String parent = null;
            if (csvRecord.isSet("parent")) {
                parent = csvRecord.get("parent");
            }
            log.debug("id: " + id + ", description: " + description);
            addTerm(id, description, parent, "");
            for (String header : csvParser.getHeaderMap().keySet()) {
                if (header.startsWith("text_")) {
                    String language = StringUtils.substringAfter(header, "text_");
                    if (csvRecord.isSet(header)) {
                        description = csvRecord.get(header);
                        addTerm(id, description, parent, language);
                    }
                }
            }
        }
        log.debug("Total " + getDefaultDescription().size() + " term(s) loaded..");
    }

    @Override
    public String getTerminologyId() {
        return terminologyConfig.getTerminologyId();
    }

    @Override
    public boolean isSubclassOf(CodePhrase codeA, CodePhrase codeB) {
        checkTerminologySupported(codeA);
        checkTerminologySupported(codeB);
        return checkSubclassOf(codeA, codeB);
    }

    @Override
    public boolean isSubclassOf(CodePhrase code, Set<CodePhrase> codes) {
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
            } catch (Exception ex) {
                log.warn(format("InvalidCodeException: checkSubclassOf('%s','%s') ignored. Message: %s", code, cp.getCodeString(), ex.getMessage()));
            }
        }
        return ret;
    }

    @Override
    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language) {
        String code = concept.getCodeString();
        return retrieveAllSubclasses(code, language);
    }

    private TerminologyNodeVO retrieveAllSubclasses(String code, CodePhrase language) {
        String cleanCode = cleanUpCode(code);
        TerminologyNodeVO node = getNodeForCode(cleanCode, language);
        Set<String> children = childrenMap.get(cleanCode);
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

    @Override
    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language) {
        if (this.terminologyConfig.getTerminologyId().equals(terminologyId)) {
            ArrayList<TerminologyNodeVO> allNodes = new ArrayList<>();
            for (String code : getDefaultDescription().keySet()) {
                if (!code.isEmpty()
                        && (parentsMap.get(code) == null || parentsMap.get(code).isEmpty())) {
                    TerminologyNodeVO node = retrieveAllSubclasses(code, language);
                    allNodes.add(node);
                }
            }
            return allNodes;
        } else {
            throw new UnsupportedTerminologyException(terminologyId + " not supported");
        }
    }

    private Map<String, String> getDefaultDescription() {
        return getDescriptionByLanguage("");
    }

    @Override
    public boolean isTerminologySupported(String terminologyId) {
        return this.terminologyConfig.getTerminologyId().equalsIgnoreCase(terminologyId);
    }

    @Override
    public String retrieveTerm(CodePhrase concept, CodePhrase language) {
        String code = cleanUpCode(concept.getCodeString());
        return retrieveTerm(code, language);
    }

    private String retrieveTerm(String code, CodePhrase languageCodePhrase) {
        String language = "";
        if (languageCodePhrase == null) {
            log.warn("Language not defined!");
        } else {
            language = languageCodePhrase.getCodeString();
        }
        String cleanCode = cleanUpCode(code);
        String description = getDescription(cleanCode, language);
        if (description == null && !language.isEmpty()) {
            description = getDescription(cleanCode, "");
        }
        return description != null ? description : "";
    }

    @Override
    public DvCodedText translate(DvCodedText codedText, CodePhrase language) {
        if (codedText == null) {
            throw new RuntimeException(format("Detected 'null' codedText trying to perform translation for language '%s'", language));
        }
        if (language == null) {
            throw new RuntimeException(format("Detected 'null' language trying to translate concept '%s'", codedText.toString()));
        }
        String code = codedText.getCode();
        String description = getDescription(code, language.getCodeString());
        if (description == null) {
            description = getDescription(code, "");
        }
        if (description == null) {
            return codedText;
        } else {
            return new DvCodedText(description, codedText.getDefiningCode());
        }
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

    private void addTerm(String id, String description, String parent, String language) {
        if (!isEmpty(id) && !isEmpty(description)) {
            getDescriptionByLanguage(language).put(id, description);
            addParent(id, parent);
        }
    }

    private Map<String, String> getDescriptionByLanguage(String language) {
        return descriptionsMap.computeIfAbsent(language, k -> new HashMap<>());
    }

    private void addParent(String id, String parent) {
        if (!isEmpty(parent) && !parentsMap.containsKey(id)) {
            Set<String> parentHierarchy = parentsMap.computeIfAbsent(parent, f -> new HashSet<>());
            Set<String> childHierarchy = new HashSet<>();
            childHierarchy.addAll(parentHierarchy);
            childHierarchy.add(parent);
            parentsMap.put(id, childHierarchy);
            Set<String> children = childrenMap.computeIfAbsent(parent, k -> new HashSet<>());
            children.add(id);
        }
    }

    private TerminologyNodeVO getNodeForCode(String code, CodePhrase language) {
        String desc = retrieveTerm(code, language);
        if (desc == null || desc.trim().isEmpty()) {
            desc = code;
        }
        return new TerminologyNodeVO(new DvCodedText(desc, new CodePhrase(terminologyConfig.getTerminologyId(), code)));
    }


    private boolean checkSubclassOf(CodePhrase codeA, CodePhrase codeB) {
        if (isValidTerminologyCode(codeA) && isValidTerminologyCode(codeB)) {
            String as = codeA.getCodeString();
            String bs = codeB.getCodeString();
            return checkSubclassOf(as, bs);
        } else {
            throw new UnsupportedTerminologyException(codeA.getTerminologyId() + " not supported");
        }
    }

    private boolean checkSubclassOf(String as, String bs) {
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
                Set<String> parents = parentsMap.get(cleanAS);
                return (parents != null && parents.contains(cleanBS));
            }
        }
    }

    private void checkTerminologySupported(CodePhrase code) {
        checkTerminologySupported(code.getTerminologyId().getValue());
    }

    private void checkTerminologySupported(String terminology) {
        if (!isTerminologySupported(terminology)) {
            throw new UnsupportedTerminologyException(terminology + " not supported");
        }
    }

    private boolean invalidCode(String code) {
        if (terminologyConfig.isCodeExistenceCheck()) {
            String cleanCode = cleanUpCode(code);
            return getDefaultDescription().get(cleanCode) == null;
        } else {
            return false;
        }
    }

    private boolean isValidTerminologyCode(CodePhrase code) {
        return isTerminologySupported(code.getTerminologyId().getValue());
    }

    private String getDescription(String code, String language) {
        return getDescriptionByLanguage(language).get(code);
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
package se.cambio.openehr.controller.terminology.plugins;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnsupportedLanguageException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;

import java.io.InputStream;
import java.util.*;


public class ATCTerminologyServicePlugin extends CSVTerminologyServicePlugin{

    private static String TERMINOLOGY_ID = "ATC";
    private Map<String, TerminologyNodeVO> _allTerminologyNodeMap = null;
    private Map<String, TerminologyNodeVO> _parentsTerminologyNodeMap = null;

    public ATCTerminologyServicePlugin() {
        super(TERMINOLOGY_ID);
    }

    public void init(InputStream is) throws InternalErrorException {
        super.init(is);
        List<String> ids = new ArrayList<String>(getDescriptionsMap().keySet());
        Collections.sort(ids);
        boolean parentFound = false;
        for (String code: ids){
            parentFound = false;
            TerminologyNodeVO tnVO = generateTerminologyNodeVO(code);
            int l = code.length();
            while(l>1){
                l--;
                String idAux = code.substring(0,l);
                if (getAllTerminologyNodeMap().containsKey(idAux)){
                    getAllTerminologyNodeMap().get(idAux).addChild(tnVO);
                    parentFound = true;
                    break;
                }
            }
            if (!parentFound){
                getParentTerminologyNodeMap().put(code, tnVO);
            }
            getAllTerminologyNodeMap().put(code, tnVO);
        }
    }

    protected boolean checkSubclassOf(String as, String bs)
            throws UnsupportedTerminologyException, InvalidCodeException {
        String cleanAS = cleanUpCode(as);
        if (invalidCode(cleanAS)){
            throw new InvalidCodeException("Invalid ATC code: " + as);
        }
        String cleanBS = cleanUpCode(bs);
        if (invalidCode(cleanBS)){
            throw new InvalidCodeException("Invalid ATC code: " + bs);
        }
        return cleanAS.contains(cleanBS); //No need to check parent nodes
        //return super.checkSubclassOf(cleanAS, cleanBS);
    }

    public String retrieveTerm(String code, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException {
        code = cleanUpCode(code);
        return super.getDescription(code);
    }

    private String cleanUpCode(String code) {
        code = code.replace("-", "");
        code = code.replace(".", "");
        while (code.length()>1 && invalidCode(code)){
            code = code.substring(0, code.length()-1);
        }
        return code;
    }

    public boolean isValidCodePhrase(CodePhrase codePhrase) {
        String code = cleanUpCode(codePhrase.getCodeString());
        return  TERMINOLOGY_ID.equals(codePhrase.getTerminologyId().getValue()) && !invalidCode(code);
    }

    @Override
    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException {
        if (TERMINOLOGY_ID.equals(terminologyId)){
            return new ArrayList<TerminologyNodeVO>(getParentTerminologyNodeMap().values());
        }else{
            throw new UnsupportedTerminologyException(terminologyId+ " not supported");
        }
    }

    @Override
    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException, InvalidCodeException {
        String code = concept.getCodeString();
        code = cleanUpCode(code);
        return retrieveAllSubclasses(code);
    }

    private TerminologyNodeVO retrieveAllSubclasses(String code)
            throws UnsupportedTerminologyException, UnsupportedLanguageException {
        return getAllTerminologyNodeMap().get(code);
    }

    public Map<String, TerminologyNodeVO> getParentTerminologyNodeMap(){
        if (_parentsTerminologyNodeMap ==null){
            _parentsTerminologyNodeMap = new HashMap<String, TerminologyNodeVO>();
        }
        return _parentsTerminologyNodeMap;
    }

    public Map<String, TerminologyNodeVO> getAllTerminologyNodeMap(){
        if (_allTerminologyNodeMap ==null){
            _allTerminologyNodeMap = new HashMap<String, TerminologyNodeVO>();
        }
        return _allTerminologyNodeMap;
    }

    private TerminologyNodeVO generateTerminologyNodeVO(String code){
        return new TerminologyNodeVO(new DvCodedText(getDescriptionsMap().get(code), new CodePhrase(TERMINOLOGY_ID, code)));
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
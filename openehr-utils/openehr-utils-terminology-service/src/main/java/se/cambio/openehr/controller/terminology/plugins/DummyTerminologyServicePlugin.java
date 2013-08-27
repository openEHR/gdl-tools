package se.cambio.openehr.controller.terminology.plugins;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnknownPropertyException;
import se.cambio.openehr.util.exceptions.UnsupportedLanguageException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;

public class DummyTerminologyServicePlugin implements TerminologyServicePlugin {

    private String _terminologyId = null;
    private int _maxCodeSize = 50;
    private Map<String, String> _descriptionsMap = null;

    public DummyTerminologyServicePlugin(String terminologyId, int maxCodeSize) {
	_terminologyId = terminologyId;
	_maxCodeSize = maxCodeSize;
    }

    public String getTerminologyId() {
	return _terminologyId;
    }

    public boolean isSubclassOf(CodePhrase a, CodePhrase b)
	    throws UnsupportedTerminologyException, InvalidCodeException {
	checkTerminologySupported(a);
	checkTerminologySupported(b);
	return checkSubclassOf(a, b);
    }

    public boolean isSubclassOf(CodePhrase code, Set<CodePhrase> codes)
	    throws UnsupportedTerminologyException, InvalidCodeException {

	checkTerminologySupported(code);
	for (CodePhrase cp : codes) {
	    checkTerminologySupported(cp);
	}
	boolean ret = false;
	for (CodePhrase cp : codes) {
	    if (checkSubclassOf(code, cp)) {
		ret = true;
		break;
	    }
	}
	return ret;
    }

    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException, InvalidCodeException {
	String desc = getDescription(concept.getCodeString());
	if (desc == null) {
	    desc = concept.toString();
	}
	return new TerminologyNodeVO(new DvCodedText(desc, concept));
    }

    public List<TerminologyNodeVO> retrieve(String expression, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException {
	// TODO Auto-generated method stub
	return null;
    }
    
    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException {
	return new ArrayList<TerminologyNodeVO>();
    }

    public boolean hasPropertyOfValue(CodePhrase concept, CodePhrase property,
	    CodePhrase value) throws UnsupportedTerminologyException,
	    UnknownPropertyException {
	// TODO Auto-generated method stub
	return false;
    }

    public List<DvCodedText> retrieveAllPossibleValues(CodePhrase property,
	    CodePhrase language) throws UnsupportedTerminologyException,
	    UnknownPropertyException, UnsupportedLanguageException {
	// TODO Auto-generated method stub
	return null;
    }

    public boolean isTerminologySupported(String terminologyId) {
	return _terminologyId.equalsIgnoreCase(terminologyId);
    }

    private boolean checkSubclassOf(CodePhrase a, CodePhrase b)
	    throws UnsupportedTerminologyException, InvalidCodeException {
	if (isValidTerminologyCode(a) && isValidTerminologyCode(b)) {
	    String as = a.getCodeString();
	    if (invalidCode(as)) {
		throw new InvalidCodeException("Invalid " + _terminologyId
			+ " code: " + as);
	    }
	    as = cleanUpCode(as);

	    String bs = b.getCodeString();
	    bs = bs.replace(".", "");

	    // I73.9 => I739
	    if (bs.length() == _maxCodeSize) {
		return bs.equals(as);
	    } else if (bs.length() < _maxCodeSize) {
		return as.indexOf(bs) == 0;
	    } else {
		throw new UnsupportedTerminologyException("Unsupported "
			+ _terminologyId + " code: " + b);
	    }
	} else {
	    throw new UnsupportedTerminologyException(a.getTerminologyId()
		    + " not supported");
	}
    }

    private void checkTerminologySupported(CodePhrase code)
	    throws UnsupportedTerminologyException {
	checkTerminologySupported(code.getTerminologyId().getValue());
    }

    private void checkTerminologySupported(String terminology)
	    throws UnsupportedTerminologyException {
	if (!isTerminologySupported(terminology)) {
	    throw new UnsupportedTerminologyException(terminology
		    + " not supported");
	}
    }

    // return null if not good
    private String cleanUpCode(String code) {
	if (code.length() > _maxCodeSize) {
	    code = code.substring(0, _maxCodeSize);
	}
	code = code.replace("-", "");
	code = code.replace(".", "");
	return code;
    }

    private boolean invalidCode(String code) {
	return code == null || code.length() < 3;
    }

    private Map<String, String> getDescriptionsMap() {
	if (_descriptionsMap == null) {
	    _descriptionsMap = new HashMap<String, String>();
	}
	return _descriptionsMap;
    }

    public void registerDescription(String code, String description) {
	getDescriptionsMap().put(code, description);
    }

    private String getDescription(String code) {
	return getDescriptionsMap().get(code);
    }

    private boolean isValidTerminologyCode(CodePhrase code) {
	return isTerminologySupported(code.getTerminologyId().getValue());
    }

    public String retrieveTerm(CodePhrase concept, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException {
	// TODO Auto-generated method stub
	return null;
    }

    public void init(InputStream br) {

    }

    public Collection<String> getSupportedTerminologies() {
	Collection<String> supportedTerminologies = new ArrayList<String>();
	supportedTerminologies.add(_terminologyId);
	return supportedTerminologies;
    }

    public boolean isValidCodePhrase(CodePhrase codePhrase) {
	return true;
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
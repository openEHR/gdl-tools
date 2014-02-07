package se.cambio.openehr.model.facade.terminology.delegate;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnsupportedLanguageException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;

import java.util.Collection;
import java.util.List;
import java.util.Set;



/**
 * @author iago.corbal
 *
 */
public interface TerminologyFacadeDelegate {
    public boolean isSubclassOf(CodePhrase a, CodePhrase b) 
	    throws InternalErrorException, UnsupportedTerminologyException, InvalidCodeException;

    public boolean isSubclassOf(CodePhrase a, Set<CodePhrase> b) 
	    throws InternalErrorException, UnsupportedTerminologyException, InvalidCodeException;

    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language)  
	    throws InternalErrorException, UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException;

    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language)
	    throws InternalErrorException, UnsupportedTerminologyException, UnsupportedLanguageException;

    public String retrieveTerm(CodePhrase concept, CodePhrase language)  
	    throws InternalErrorException, UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException;

    public boolean isValidCodePhrase(CodePhrase cp) throws InternalErrorException;

    public Collection<String> getSupportedTerminologies() throws InternalErrorException;

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
package se.cambio.cds.util;

import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.ts.InvalidCodeException;
import se.cambio.cds.ts.Node;
import se.cambio.cds.ts.TerminologyService;
import se.cambio.cds.ts.TerminologyServiceImpl;
import se.cambio.cds.ts.UnsupportedLanguageException;
import se.cambio.cds.ts.UnsupportedTerminologyException;

public class CDSTerminologyService {

    private static String TERMINOLOGY_CONFIG_FILE = "TerminologyServiceConfig.properties";

    private static CDSTerminologyService _instance = null;
    private TerminologyService _terminologyService = null;


    private CDSTerminologyService(){
	InputStream is = this.getClass().getClassLoader().getResourceAsStream(TERMINOLOGY_CONFIG_FILE);
	TerminologyServiceImpl tsi = (TerminologyServiceImpl)TerminologyServiceImpl.getInstance(is);
	_terminologyService = tsi;
    }


    public static boolean isSubclassOf(CodePhrase a, CodePhrase b) 
	    throws UnsupportedTerminologyException, InvalidCodeException{
	return getDelegate()._terminologyService.isSubclassOf(a, b);
    }

    public static boolean isSubclassOf(CodePhrase a, Set<CodePhrase> b) 
	    throws UnsupportedTerminologyException, InvalidCodeException{
	return getDelegate()._terminologyService.isSubclassOf(a, b);
    }

    public static Node retrieveAllSubclasses(CodePhrase concept, CodePhrase language)  
	    throws UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException{
	return getDelegate()._terminologyService.retrieveAllSubclasses(concept, language);
    }
    
    public static List<Node> retrieveAll(String terminologyId, CodePhrase language)
    throws UnsupportedTerminologyException, UnsupportedLanguageException{
	return getDelegate()._terminologyService.retrieveAll(terminologyId, language);
    }

    public static String retrieveTerm(CodePhrase concept, CodePhrase language)  
	    throws UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException{
	return getDelegate()._terminologyService.retrieveTerm(concept, language);
    }

    public Collection<String> getSupportedTerminologies() {
	return getDelegate()._terminologyService.getSupportedTerminologies();
    }
    
    public static CDSTerminologyService getDelegate(){
	if (_instance==null){
	    _instance = new CDSTerminologyService();
	}
	return _instance;
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
package se.cambio.cm.controller.terminology;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cm.util.TerminologyNodeVO;

import java.util.Collection;
import java.util.List;
import java.util.Set;

public interface TerminologyService {

    boolean isSubclassOf(CodePhrase codeA, CodePhrase codeB);

    boolean isSubclassOf(CodePhrase code, Set<CodePhrase> codes);

    TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language);

    List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language);

    String retrieveTerm(CodePhrase concept, CodePhrase language);

    DvCodedText translate(DvCodedText concept, CodePhrase language);

    boolean isTerminologySupported(String terminologyId);

    boolean isValidCodePhrase(CodePhrase codePhrase);

    Collection<String> getSupportedTerminologies();
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla License Version
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
package se.cambio.cds.gdl.model;

import java.io.Serializable;
import java.util.Map;

import org.openehr.rm.datatypes.text.CodePhrase;

public class Language implements Serializable{

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
	public Language(){
	}
	
	public Language(CodePhrase originalLanguage,
			Map<String, TranslationDetails> translations) {
		this.originalLanguage = originalLanguage;
		this.translations = translations;
	}

	/**
	 * @return the originalLanguage
	 */
	public CodePhrase getOriginalLanguage() {
		return originalLanguage;
	}

	/**
	 * @return the translations
	 */
	public Map<String, TranslationDetails> getTranslations() {
		return translations;
	}
	
	/**
	 * @param originalLanguage the originalLanguage to set
	 */
	public void setOriginalLanguage(CodePhrase originalLanguage) {
		this.originalLanguage = originalLanguage;
	}

	/**
	 * @param translations the translations to set
	 */
	public void setTranslations(Map<String, TranslationDetails> translations) {
		this.translations = translations;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((originalLanguage == null) ? 0 : originalLanguage.hashCode());
		result = prime * result
				+ ((translations == null) ? 0 : translations.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Language other = (Language) obj;
		if (originalLanguage == null) {
			if (other.originalLanguage != null)
				return false;
		} else if (!originalLanguage.equals(other.originalLanguage))
			return false;
		if (translations == null) {
			if (other.translations != null)
				return false;
		} else if (!translations.equals(other.translations))
			return false;
		return true;
	}

	private CodePhrase originalLanguage;
	private Map<String, TranslationDetails> translations;
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
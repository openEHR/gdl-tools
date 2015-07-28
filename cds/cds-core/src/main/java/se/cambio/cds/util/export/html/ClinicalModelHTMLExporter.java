package se.cambio.cds.util.export.html;

import se.cambio.cds.util.misc.CDSLanguageManager;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.HTMLRenderer;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

public abstract class ClinicalModelHTMLExporter<E> {

    private ArchetypeManager archetypeManager;
    private E entity;
    private String lang;

    protected ClinicalModelHTMLExporter(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
    }

    private HashMap<String, String> getTextsHashMap() {
        HashMap<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "Description");
        addText(textsMap, "Purpose");
        addText(textsMap, "Use");
        addText(textsMap, "Misuse");
        addText(textsMap, "References");
        addText(textsMap, "AuthorDetails");
        addText(textsMap, "Name");
        addText(textsMap, "Email");
        addText(textsMap, "Organisation");
        addText(textsMap, "Date");
        addText(textsMap, "AuthorshipLifecycle");
        addText(textsMap, "Copyright");
        addText(textsMap, "Keywords");
        addText(textsMap, "Contributors");
        textsMap.putAll(getEntityTextMap());
        return textsMap;
    }

    public Map<String, Object> getObjectsMap() throws InternalErrorException, InstanceNotFoundException {
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.putAll(getEntityObjectsMap());
        objectMap.put("texts", getTextsHashMap());
        objectMap.put("language", lang);
        return objectMap;
    }


    public String convertToHTML(E entity, String lang) throws InternalErrorException {
        this.entity = entity;
        this.lang = lang;
        try {
            InputStream is = getInputStreamTemplate();
            InputStreamReader isr = new InputStreamReader(is, "UTF-8");
            HTMLRenderer htmlRenderer = new HTMLRenderer(isr);
            return htmlRenderer.proccess(getObjectsMap());
        } catch (UnsupportedEncodingException e) {
            throw new InternalErrorException(e);
        } catch (InstanceNotFoundException e) {
            throw new InternalErrorException(e);
        }
    }

    public abstract Map<String, Object> getEntityObjectsMap() throws InternalErrorException, InstanceNotFoundException;
    public abstract Map<String, String> getEntityTextMap();
    public abstract InputStream getInputStreamTemplate();

    public String addText(Map<String, String> textsMap, String textId) {
        return textsMap.put(textId, CDSLanguageManager.getMessageWithLanguage(textId, getLanguage()));
    }

    public E getEntity() {
        return entity;
    }

    public String getLanguage() {
        return lang;
    }

    public ArchetypeManager getArchetypeManager() {
        return archetypeManager;
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

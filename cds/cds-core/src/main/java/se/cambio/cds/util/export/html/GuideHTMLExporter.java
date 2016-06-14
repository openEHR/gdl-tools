package se.cambio.cds.util.export.html;

import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.ReadableRule;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.gdl.model.readable.rule.lines.interfaces.ArchetypeReferenceRuleLine;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.GuideImporter;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.*;

public class GuideHTMLExporter extends ClinicalModelHTMLExporter<Guide> {

    public GuideHTMLExporter(ArchetypeManager archetypeManager) {
        super(archetypeManager);
    }

    @Override
    public Map<String, Object> getEntityObjectsMap() throws InternalErrorException {
        String lang = getLanguage();
        ReadableGuide readableGuide = new GuideImporter(getArchetypeManager()).importGuide(getEntity(), lang);
        Collection<String> htmlReadableRules = getHTMLReadableRules(readableGuide, lang);
        List<RuleLine> definitionRuleLines = readableGuide.getDefinitionRuleLines().getRuleLines();
        Collection<String> definitionsHtmlEhr = getHTMLRuleLines(getDefinitionRuleLinesByDomainId(definitionRuleLines, Domains.EHR_ID), lang);
        Collection<String> definitionsHtmlCds = getHTMLRuleLines(getDefinitionRuleLinesByDomainId(definitionRuleLines, Domains.CDS_ID), lang);
        Collection<String> definitionsHtmlAny = getHTMLRuleLines(getDefinitionRuleLinesByDomainId(definitionRuleLines, null), lang);
        Collection<String> preconditionsHtml = getHTMLRuleLines(readableGuide.getPreconditionRuleLines().getRuleLines(), lang);
        Collection<String> defaultActionsHtml = getHTMLRuleLines(readableGuide.getDefaultActions().getRuleLines(), lang);
        Map<String, Object> objectMap = new HashMap<String, Object>();
        objectMap.put("guide", getEntity());
        objectMap.put("guide_details", getEntity().getDescription().getDetails().get(lang));
        objectMap.put("guide_definitions_ehr", definitionsHtmlEhr);
        objectMap.put("guide_definitions_cds", definitionsHtmlCds);
        objectMap.put("guide_definitions_any", definitionsHtmlAny);
        objectMap.put("guide_preconditions", preconditionsHtml);
        objectMap.put("guide_default_actions", defaultActionsHtml);
        objectMap.put("guide_rules", htmlReadableRules);
        objectMap.put("guide_terms", getEntity().getOntology().getTermDefinitions().get(lang).getTerms());
        return objectMap;
    }

    private Collection<RuleLine> getDefinitionRuleLinesByDomainId(List<RuleLine> definitionRulelines, String domainId) {
        List<RuleLine> ruleLines = new ArrayList<>();
        for (RuleLine ruleLine : definitionRulelines) {
            if (ruleLine instanceof ArchetypeReferenceRuleLine) {
                String domainIdRuleLine = ((ArchetypeReferenceRuleLine) ruleLine).getArchetypeReference().getIdDomain();
                if (domainId == null && domainIdRuleLine == null || (domainId != null && domainId.equals(domainIdRuleLine))) {
                    ruleLines.add(ruleLine);
                }
            }
        }
        return ruleLines;
    }

    private static Collection<String> getHTMLReadableRules(ReadableGuide readableGuide, String lang) {
        Collection<String> htmlReadableRules = new ArrayList<String>();
        for (ReadableRule readableRule : readableGuide.getReadableRules().values()) {
            htmlReadableRules.add(readableRule.toHTMLString(lang));
        }
        return htmlReadableRules;
    }

    private static Collection<String> getHTMLRuleLines(Collection<RuleLine> ruleLines, String lang) {
        Collection<String> htmlRuleLine = new ArrayList<String>();
        for (RuleLine ruleLine : ruleLines) {
            htmlRuleLine.add(ruleLine.toHTMLString(lang));
        }
        return htmlRuleLine;
    }

    @Override
    public Map<String, String> getEntityTextMap() {
        HashMap<String, String> textsMap = new HashMap<String, String>();
        addText(textsMap, "GuideDetails");
        addText(textsMap, "Definitions");
        addText(textsMap, "Preconditions");
        addText(textsMap, "Defaults");
        addText(textsMap, "RuleList");
        addText(textsMap, "Bindings");
        return textsMap;
    }

    @Override
    public InputStream getInputStreamTemplate() {
        return GuideHTMLExporter.class.getClassLoader().getResourceAsStream("gdl.ftl");
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
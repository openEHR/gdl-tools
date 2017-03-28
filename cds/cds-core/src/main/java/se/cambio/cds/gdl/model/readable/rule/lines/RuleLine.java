package se.cambio.cds.gdl.model.readable.rule.lines;

import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;
import se.cambio.cds.gdl.model.readable.rule.lines.elements.RuleLineElement;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.ArrayList;


public abstract class RuleLine {
    private String name = null;
    private String description = null;
    private ArrayList<RuleLineElement> ruleLineElements = null;
    private boolean commented = false;
    private RuleLine parentRuleLine = null;
    private RuleLineCollection childrenRuleLines = null;
    private ReadableGuide readableGuide;

    public RuleLine(String name, String description) {
        super();
        this.name = name;
        this.description = description;
        this.ruleLineElements = new ArrayList<>();
    }

    public void setReadableGuide(ReadableGuide readableGuide) {
        this.readableGuide = readableGuide;
        this.getChildrenRuleLines().setReadableGuide(readableGuide);
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public ArrayList<RuleLineElement> getRuleLineElements() {
        return ruleLineElements;
    }

    public String toString() {
        String language = getLanguage(); //TODO Remove
        StringBuffer sb = new StringBuffer();
        int i = 0;
        for (RuleLineElement ruleLineElement : ruleLineElements) {
            sb.append(ruleLineElement.getLabelText(language));
            i++;
            if (i < ruleLineElements.size()) {
                sb.append(" ");
            }
        }
        return sb.toString();
    }

    public String toHTMLString(String lang) {
        return toHTMLString(0, lang);
    }

    public String toHTMLString(int level, String lang) {
        StringBuffer sb = new StringBuffer();
        int i = 0;
        sb.append(getLevelSpace(level));
        for (RuleLineElement ruleLineElement : ruleLineElements) {
            sb.append(ruleLineElement.getLabelTextHTML(lang));
            i++;
            if (i < ruleLineElements.size()) {
                sb.append(" ");
            }
        }
        return sb.toString();
    }

    protected String getLevelSpace(int level) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < level; i++) {
            sb.append("&nbsp;&nbsp;&nbsp;&nbsp;");
        }
        return sb.toString();
    }

    public boolean isCommented() {
        return commented;
    }

    public void setCommented(boolean commented) {
        this.commented = commented;
        for (RuleLine ruleLine : getChildrenRuleLines().getRuleLines()) {
            ruleLine.setCommented(commented);
        }
    }

    public RuleLine getParentRuleLine() {
        return parentRuleLine;
    }

    private void setParentRuleLine(RuleLine parentRuleLine) {
        this.parentRuleLine = parentRuleLine;
    }

    public RuleLineCollection getChildrenRuleLines() {
        if (childrenRuleLines == null) {
            childrenRuleLines = new RuleLineCollection(readableGuide);
        }
        return childrenRuleLines;
    }

    public void addChildRuleLine(RuleLine ruleLine) {
        getChildrenRuleLines().add(ruleLine);
        ruleLine.setParentRuleLine(this);
    }

    public void detachFromParent() {
        this.parentRuleLine.getChildrenRuleLines().remove(this);
        this.parentRuleLine = null;
    }

    public ReadableGuide getReadableGuide() {
        if (readableGuide == null) {
            if (parentRuleLine != null) {
                readableGuide = parentRuleLine.getReadableGuide();
            }
        }
        return readableGuide;
    }


    public TermDefinition getTermDefinition() {
        return getReadableGuide().getTermDefinition();
    }

    public ArchetypeManager getArchetypeManager() {
        return getReadableGuide().getArchetypeManager();
    }

    protected String getLanguage() {
        return UserConfigurationManager.instance().getLanguage();
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
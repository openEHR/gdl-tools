package se.cambio.cds.gdl.model.readable.rule.lines.elements;

import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.cds.util.export.DVDefSerializer;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.OpenEHRLanguageManager;

public class ArchetypeDataValueRuleLineElement extends DataValueRuleLineElement {

    private ArchetypeElementVO _archetypeElementVO = null;

    public ArchetypeDataValueRuleLineElement(RuleLine ruleLine) {
        super(ruleLine);
    }

    public void setArchetypeElementVO(ArchetypeElementVO archetypeElementVO) {
        _archetypeElementVO = archetypeElementVO;
    }

    public String getDvText(String lang) {
        if (getValue() != null) {
            if (getValue() instanceof DvCodedText && _archetypeElementVO != null) {
                DvCodedText codedText = (DvCodedText) getValue();
                String text = getArchetypeManager().getCodedTexts().getText(_archetypeElementVO.getIdTemplate(), _archetypeElementVO.getId(), codedText.getCode(), lang);
                if (text != null) {
                    return text;
                } else {
                    return codedText.getValue();
                }
            } else if (getValue() instanceof DvOrdinal && _archetypeElementVO != null) {
                DvOrdinal ordinal = (DvOrdinal) getValue();
                String text = getArchetypeManager().getOrdinals().getText(_archetypeElementVO.getIdTemplate(), _archetypeElementVO.getId(), ordinal.getCode(), lang);
                if (text != null) {
                    return text;
                } else {
                    return ordinal.getSymbol().getValue();
                }
            } else {
                return DVDefSerializer.getReadableValue(getValue(), getParentRuleLine().getTermDefinition());
            }
        }
        return OpenEHRLanguageManager.getMessage("DataValue"); //Default
    }

    @Override
    public String getLabelText(String lang) {
        return getDvText(lang);
    }

    @Override
    public String getLabelTextHTML(String lang) {
        return "<font color='#00803a'><b>" + getDvText(lang) + "</b></font>";
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
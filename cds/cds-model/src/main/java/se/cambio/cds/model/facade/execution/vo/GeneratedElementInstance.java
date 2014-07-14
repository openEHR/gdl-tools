package se.cambio.cds.model.facade.execution.vo;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ContainerInstance;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.HashSet;
import java.util.Set;


public class GeneratedElementInstance extends ElementInstance{

    private static final long serialVersionUID = 1L;
    private Set<RuleReference> ruleReferences = new HashSet<RuleReference>();

    public GeneratedElementInstance(
            String id,
            DataValue dataValue,
            ArchetypeReference archetypeReference,
            ContainerInstance containerInstance,
            DvCodedText nullFlavour) {
        super(id, dataValue, archetypeReference, containerInstance, nullFlavour);
    }

    public GeneratedElementInstance clone(){
        GeneratedElementInstance generatedElementInstance = new GeneratedElementInstance(getId(), getDataValue(), getArchetypeReference().clone(), getContainerInstance(), getNullFlavour());
        generatedElementInstance.setRuleReferences(new HashSet<RuleReference>(getRuleReferences()));
        return generatedElementInstance;
    }

    public boolean isPredicate(){
        return false;
    }

    public Set<RuleReference> getRuleReferences() {
        return ruleReferences;
    }

    public void setRuleReferences(Set<RuleReference> ruleReferences) {
        this.ruleReferences = ruleReferences;
    }

    public String toString(){
        return "(G)"+super.toString();
    }

    public boolean hasNoValue(String gtCodeReference){
        if (gtCodeReference!=null){
            for(RuleReference ruleReference: ruleReferences){
                if (ruleReference.toString().equals(gtCodeReference)){
                    return super.hasNoValue();
                }
            }
        }
        return false;
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
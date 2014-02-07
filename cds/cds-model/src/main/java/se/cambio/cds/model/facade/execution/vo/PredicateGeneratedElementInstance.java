package se.cambio.cds.model.facade.execution.vo;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ContainerInstance;

public class PredicateGeneratedElementInstance extends GeneratedElementInstance{

    private static final long serialVersionUID = 1L;
    private OperatorKind operatorKind = null;

    public PredicateGeneratedElementInstance(
            String id,
            DataValue dataValue,
            ArchetypeReference archetypeReference,
            ContainerInstance containerInstance,
            DvCodedText nullFlavour,
            String guideId,
            String gtCode,
            OperatorKind operatorKind) {
        super(id, dataValue, archetypeReference, containerInstance, nullFlavour, guideId, gtCode);
        this.operatorKind = operatorKind;
    }

    public OperatorKind getOperatorKind(){
        return operatorKind;
    }

    public void setOperatorKind(OperatorKind operatorKind) {
        this.operatorKind = operatorKind;
    }

    /**
     * METHODS FOR THE RULE ENGINE!!
     * Do not use inside core (unless totally necessary)
     */
    public void setDataValue(DataValue dataValue) {
        Logger.getLogger(PredicateGeneratedElementInstance.class).warn("Attempt to change data value of generated element (from '"+getDataValue()+"' to '"+dataValue+"')");
    }

    public boolean hasValue(){
        return false;
    }

    public boolean isPredicate(){
        return true;
    }

    public boolean hasNoValue(){
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
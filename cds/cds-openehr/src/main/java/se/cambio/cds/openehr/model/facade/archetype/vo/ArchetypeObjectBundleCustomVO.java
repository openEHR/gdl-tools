package se.cambio.cds.openehr.model.facade.archetype.vo;

import java.io.Serializable;
import java.util.Collection;

import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.model.archetypeslot.vo.ArchetypeSlotVO;
import se.cambio.cds.openehr.model.cluster.vo.ClusterVO;
import se.cambio.cds.openehr.model.codedtext.vo.CodedTextVO;
import se.cambio.cds.openehr.model.ordinal.vo.OrdinalVO;
import se.cambio.cds.openehr.model.proportiontype.vo.ProportionTypeVO;
import se.cambio.cds.openehr.model.unit.vo.UnitVO;

public class ArchetypeObjectBundleCustomVO implements Serializable {

    private static final long serialVersionUID = 25042012L;
    private ArchetypeDTO archetypeVO = null;
    private Collection<ArchetypeElementVO> archetypeElementVOs = null;
    private Collection<ClusterVO> clusterVOs = null;
    private Collection<ArchetypeSlotVO> archetypeSlotVOs = null;
    private Collection<CodedTextVO> codedTextVOs = null;
    private Collection<OrdinalVO> ordinalVOs = null;
    private Collection<UnitVO> unitVOs = null;
    private Collection<ProportionTypeVO> proportionTypeVOs = null;

    public ArchetypeObjectBundleCustomVO(
	    ArchetypeDTO archetypeVO,
	    Collection<ArchetypeElementVO> archetypeElementVOs,
	    Collection<ClusterVO> clusterVOs,
	    Collection<ArchetypeSlotVO> archetypeSlotVOs,
	    Collection<CodedTextVO> codedTextVOs,
	    Collection<OrdinalVO> ordinalVOs,
	    Collection<UnitVO> unitVOs,
	    Collection<ProportionTypeVO> proportionTypeVOs) {
	super();
	this.archetypeVO = archetypeVO;
	this.archetypeElementVOs = archetypeElementVOs;
	this.clusterVOs = clusterVOs;
	this.archetypeSlotVOs = archetypeSlotVOs;
	this.codedTextVOs = codedTextVOs;
	this.ordinalVOs = ordinalVOs;
	this.unitVOs = unitVOs;
	this.proportionTypeVOs = proportionTypeVOs;
    }
    public ArchetypeDTO getArchetypeVO() {
        return archetypeVO;
    }
    public void setArchetypeVO(ArchetypeDTO archetypeVO) {
        this.archetypeVO = archetypeVO;
    }
    public Collection<ArchetypeElementVO> getElementVOs() {
        return archetypeElementVOs;
    }
    public void setElementVOs(Collection<ArchetypeElementVO> archetypeElementVOs) {
        this.archetypeElementVOs = archetypeElementVOs;
    }
    public Collection<ClusterVO> getClusterVOs() {
        return clusterVOs;
    }
    public void setClusterVOs(Collection<ClusterVO> clusterVOs) {
        this.clusterVOs = clusterVOs;
    }
    public Collection<ArchetypeSlotVO> getSlotVOs() {
        return archetypeSlotVOs;
    }
    public void setSlotVOs(Collection<ArchetypeSlotVO> archetypeSlotVOs) {
        this.archetypeSlotVOs = archetypeSlotVOs;
    }
    public Collection<CodedTextVO> getCodedTextVOs() {
        return codedTextVOs;
    }
    public void setCodedTextVOs(Collection<CodedTextVO> codedTextVOs) {
        this.codedTextVOs = codedTextVOs;
    }
    public Collection<OrdinalVO> getOrdinalVOs() {
        return ordinalVOs;
    }
    public void setOrdinalVOs(Collection<OrdinalVO> ordinalVOs) {
        this.ordinalVOs = ordinalVOs;
    }
    public Collection<UnitVO> getUnitVOs() {
        return unitVOs;
    }
    public void setUnits(Collection<UnitVO> unitVOs) {
        this.unitVOs = unitVOs;
    }
    public Collection<ProportionTypeVO> getProportionTypes() {
        return proportionTypeVOs;
    }
    public void setProportionTypeVOs(Collection<ProportionTypeVO> proportionTypeVOs) {
        this.proportionTypeVOs = proportionTypeVOs;
    }
    
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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
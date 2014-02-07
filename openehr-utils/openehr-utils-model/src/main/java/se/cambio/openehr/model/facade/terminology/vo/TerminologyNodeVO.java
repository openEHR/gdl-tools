package se.cambio.openehr.model.facade.terminology.vo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.openehr.rm.datatypes.text.DvCodedText;

/**
 * A generic tree representation with codedText as value. Immutable object.
 * 
 * @author rong.chen
 * 
 */
public class TerminologyNodeVO {

	public TerminologyNodeVO(DvCodedText value, List<TerminologyNodeVO> children) {
		if (value == null) {
			throw new IllegalArgumentException("value is null");
		}
		this.value = value;
		if(children != null) {
			this.children = new ArrayList<TerminologyNodeVO>(children);
		} else {
			this.children = new ArrayList<TerminologyNodeVO>();
		}
	}
	
	public TerminologyNodeVO(DvCodedText value) {
		this(value, null);
	}

	public DvCodedText getValue() {
		return value;
	}
	
	public void addChild(TerminologyNodeVO child) {
		children.add(child);
	}

	public List<TerminologyNodeVO> getChildren() {
		return Collections.unmodifiableList(children);
	}

	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (!(o instanceof TerminologyNodeVO))
			return false;
		if (!super.equals(o))
			return false;

		final TerminologyNodeVO node = (TerminologyNodeVO) o;

		return new EqualsBuilder().append(value, node.value)
				.append(children, node.children)
				.isEquals();
	}

	private final DvCodedText value;
	private final List<TerminologyNodeVO> children;
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
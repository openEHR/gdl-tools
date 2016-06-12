package se.cambio.cds.gdl.model;

import java.io.Serializable;

public class ElementBinding  implements Serializable{
    private String id;
    private String path;
	private String type;

    private static final long serialVersionUID = 1L;
	public ElementBinding() {
	}

	/**
	 * Create an Element with id, path
	 * 
	 * @param id
	 * @param path
	 */
	public ElementBinding(String id, String path) {
		if (id == null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid id");
		}
		if (path == null || path.isEmpty()) {
			throw new IllegalArgumentException("Invalid path");
		}
		this.id = id;
		this.path = path;
	}

	/**
	 * @return the id as a gtcode in the guide
	 */
	public String getId() {
		return id;
	}

	/**
	 * @return the path
	 */
	public String getPath() {
		return path;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @param path
	 *            the path to set
	 */
	public void setPath(String path) {
		this.path = path;
	}

    /**
     * @return null if not set
     */
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ElementBinding that = (ElementBinding) o;

        if (!id.equals(that.id)) return false;
        if (!path.equals(that.path)) return false;
        return type != null ? type.equals(that.type) : that.type == null;

    }

    @Override
    public int hashCode() {
        int result = id.hashCode();
        result = 31 * result + path.hashCode();
        result = 31 * result + (type != null ? type.hashCode() : 0);
        return result;
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
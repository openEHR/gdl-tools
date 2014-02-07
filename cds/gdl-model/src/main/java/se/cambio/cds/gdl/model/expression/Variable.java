package se.cambio.cds.gdl.model.expression;

public class Variable extends ExpressionItem {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

	public static Variable createByCode(String code) {
		return new Variable(code);
	}

	public static Variable createByPath(String path) {
		return new Variable(null, null, path);
	}

	public Variable(String code, String name, String path, String attribute) {
		this.name = name;
		this.code = code;
		this.path = path;
		this.attribute = attribute;
	}

	public Variable(String code, String name) {
		this(code, name, null);
	}

	public Variable(String code, String name, String path) {
		this(code, name, path, null);
	}

	public Variable(String code) {
		this.code = code;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the code
	 */
	public String getCode() {
		return code;
	}

	/**
	 * @return the path
	 */
	public String getPath() {
		return path;
	}

	/**
	 * String representation of a variable as one of the following 1. code 2.
	 * code|name| 3. code.attribute 4. code|name|.attribute 5. path
	 */
	public String toString() {
		StringBuffer buf = new StringBuffer();
		if (code != null) {
			buf.append("$");
			buf.append(code);
			if (name != null) {
				buf.append("|");
				buf.append(name);
				buf.append("|");
			}
			if (attribute != null) {
				buf.append(".");
				buf.append(attribute);
			}

		} else {
			buf.append(path);
		}
		return buf.toString();
	}

	private String name;
	private String code;
	private String path;
	private String attribute;

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((attribute == null) ? 0 : attribute.hashCode());
		result = prime * result + ((code == null) ? 0 : code.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((path == null) ? 0 : path.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
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
		Variable other = (Variable) obj;
		if (attribute == null) {
			if (other.attribute != null)
				return false;
		} else if (!attribute.equals(other.attribute))
			return false;
		if (code == null) {
			if (other.code != null)
				return false;
		} else if (!code.equals(other.code))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (path == null) {
			if (other.path != null)
				return false;
		} else if (!path.equals(other.path))
			return false;
		return true;
	}

	/**
	 * @return the attribute
	 */
	public String getAttribute() {
		return attribute;
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
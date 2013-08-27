package se.cambio.cds.gdl.parser;

import java.io.InputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.SortedSet;
import java.util.StringTokenizer;
import java.util.TreeSet;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;

import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.Binding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.gdl.model.Rule;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermBinding;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.gdl.model.TranslationDetails;

public class DADLSerializer {

	public DADLSerializer() {
		loadProfile("gdl.properties");
	}
	
	
	/**
	 * Serialize an object into DADL format using reflection
	 * 
	 * @param obj
	 * @return List of serialized strings 
	 * @throws Exception
	 */
	public List<String> toDADL(Object obj) throws Exception {
		List<String> lines = new ArrayList<String>();
		return toDADL(obj, 1, lines);
	}

	private List<String> toDADL(Object obj, int indent, List<String> lines)
			throws Exception {

		log.debug("toDADL on obj.getClass: "
				+ obj.getClass().getCanonicalName() + ", indent: " + indent
				+ ", line.size: " + lines.size());

		Class klass = obj.getClass();
		String className = klass.getSimpleName();
		String rmName = toUnderscoreSeparated(className).toUpperCase();
		String typeHeader = "(" + rmName + ") <";
		int size = lines.size();
		if (size == 0) {
			lines.add(typeHeader);
		} else {
			String l = lines.get(size - 1);
			l += typeHeader;
			lines.set(size - 1, l);
		}
		Collection<String> attributes = attributeList(obj.getClass());
		
		if(profile.containsKey(className)) {
			attributes = sortByPreferredOrder(attributes, profile.get(className));
		}
		
		String name = null;
		Object value = null;
		StringBuffer buf = null;
		for (Iterator<String> names = attributes.iterator(); names.hasNext();) {
			name = names.next();

			// suppress Id output in GDL!!
			if ("id".equals(name)
					&& (obj instanceof ArchetypeBinding || obj instanceof Rule
							|| obj instanceof ElementBinding
							|| obj instanceof Binding
							|| obj instanceof TermBinding
							|| obj instanceof Term
							|| obj instanceof TermDefinition
							|| obj instanceof ResourceDescriptionItem || obj instanceof TranslationDetails)) {
				continue;
			}

			// skip objects generated in 2nd phase parsing
			if ("preConditionExpressions".equals(name)
					|| "whenStatements".equals(name)
					|| "thenStatements".equals(name)
					|| "predicateStatements".equals(name)) {
				continue;
			}

			Method getter = getter(name, obj.getClass());
			if (getter != null) {
				value = getter.invoke(obj, null);
				buf = new StringBuffer();
				if (value != null) {
					for (int i = 0; i < indent; i++) {
						buf.append("\t");
					}
					buf.append(toUnderscoreSeparated(name));
					buf.append(" = ");

					if (value instanceof List) {
						if (!((List) value).isEmpty()) {
							buf.append("<");

							List list = (List) value;
							Object member = null;
							if (list.size() > 0
									&& isSupportedPrimitiveType(list.get(0))) {
								if (list.get(0) instanceof String) {
									for (int i = 0, j = list.size(); i < j; i++) {
										buf.append("\"");
										buf.append(list.get(i));
										buf.append("\"");
										if (i != j - 1) {
											buf.append(", ");
										}
									}
								} else if (list.get(0) instanceof CodePhrase)  {
									for (int i = 0, j = list.size(); i < j; i++) {
										buf.append("[");
										buf.append(list.get(i));
										buf.append("]");
										if (i != j - 1) {
											buf.append(", ");
										}
									}
								}
								if (list.size() == 1) {
									buf.append(",...");
								}
								buf.append(">");
								lines.add(buf.toString());
							} else {
								for (int i = 0, j = list.size(); i < j; i++) {
									member = list.get(i);
									lines.add(buf.toString());
									buf = new StringBuffer();
									for (int k = 0; k < indent + 1; k++) {
										buf.append("\t");
									}
									lines.add(buf.toString() + "[" + (i + 1)
											+ "] = ");
									toDADL(member, indent + 2, lines);
								}
								buf = new StringBuffer();
								for (int i = 0; i < indent; i++) {
									buf.append("\t");
								}
								buf.append(">");
								lines.add(buf.toString());
							}
						}
					} else if (value instanceof Map) {
						if (!((Map) value).isEmpty()) {
							buf.append("<");
							lines.add(buf.toString());
							Map map = (Map) value;
							SortedSet sorted = new TreeSet(map.keySet());
							for (Object key : sorted) {
								buf = new StringBuffer();
								for (int k = 0; k < indent + 1; k++) {
									buf.append("\t");
								}
								String keystr;
								if (key instanceof CodePhrase) {
									keystr = "[" + key + "]";
								} else {
									keystr = "\"" + key.toString() + "\"";
								}

								Object member = map.get(key);
								if (member instanceof String) {
									buf.append("[");
									buf.append(keystr);
									buf.append("] = <\"");
									buf.append(member);
									buf.append("\">");
									lines.add(buf.toString());
								} else {
									lines.add(buf.toString() + "[" + keystr
											+ "] = ");
									toDADL(member, indent + 2, lines);
								}
							}
							buf = new StringBuffer();
							for (int i = 0; i < indent; i++) {
								buf.append("\t");
							}
							buf.append(">");
							lines.add(buf.toString());
						}
					} else if (isSupportedPrimitiveType(value)) {

						log.debug("value.type: " + value.getClass());
						buf.append("<");
						if (value instanceof String) {
							buf.append("\"");
							buf.append(value);
							buf.append("\"");
						} else if (value instanceof CodePhrase) {
							buf.append("[");
							buf.append(value);
							buf.append("]");
						} else {
							buf.append(value.toString());
						}
						buf.append(">");
						lines.add(buf.toString());
					} else { // complex block

						log.debug("complex block..");

						lines.add(buf.toString());

						toDADL(value, indent + 1, lines);
					}
				}
			}
		}
		buf = new StringBuffer();
		for (int i = 0; i < indent - 1; i++) {
			buf.append("\t");
		}
		buf.append(">");
		lines.add(buf.toString());
		return lines;
	}

	private boolean isSupportedPrimitiveType(Object obj) {
		return (obj instanceof String) || (obj instanceof Integer)
				|| (obj instanceof Double) || (obj instanceof CodePhrase);
	}
	
	private Method getter(String attributeName, Class klass) {
		Method[] methods = klass.getMethods();
		String name = "get" + attributeName.substring(0, 1).toUpperCase()
				+ attributeName.substring(1);

		log.debug("search getter method of name '" + name + "'");
		
		for (Method method : methods) {
			if (method.getName().equals(name)) {
				Type[] paras = method.getParameterTypes();
				if (paras.length == 0) {
					log.debug("found '" + name + "'");
					return method;
				}
			}
		}
		return null;
	}

	private SortedSet<String> attributeList(Class klass) {
		SortedSet<String> set = new TreeSet<String>();
		Method[] methods = klass.getMethods();
		for (Method method : methods) {
			String name = method.getName();
			if (name.startsWith("set")) {
				name = name.substring(3);
				name = name.substring(0, 1).toLowerCase() + name.substring(1);
				set.add(name);				
			}
		}
		log.debug("attribute list: " + set);
		return set;
	}
	
	private List<String> sortByPreferredOrder(Collection<String> attributes,
			List<String> preferred) {
		List<String> ret = new ArrayList<String>();
		
		for(String attr : preferred) {
			if(attributes.contains(attr)) {
				ret.add(attr); // add preferred attributes
			}
		}
		for(String attr : attributes) {
			if( ! ret.contains(attr)) {
				ret.add(attr); // add the rest
			}
		}
		return ret;
	}

	private String toUnderscoreSeparated(String camelCase) {
		String[] array = StringUtils.splitByCharacterTypeCamelCase(camelCase);
		StringBuffer buf = new StringBuffer();
		for (int i = 0; i < array.length; i++) {
			String s = array[i];
			buf.append(s.substring(0, 1).toLowerCase());
			buf.append(s.substring(1));
			if (i != array.length - 1) {
				buf.append("_");
			}
		}
		return buf.toString();
	}
	
	private void loadProfile(String name) {
		profile = new HashMap<String, List<String>>();
		try {
			InputStream input = this.getClass().getClassLoader().getResourceAsStream(name);
			Properties props = new Properties();
			props.load(input);
			Enumeration keys = props.propertyNames();
			StringTokenizer tokens = null;
			List<String> attributes;
			while(keys.hasMoreElements()) {
				Object key = keys.nextElement();
				String line = props.getProperty( (String) key);
				log.debug(line);
				
				tokens = new StringTokenizer(line, ",");
				attributes  = new ArrayList<String>();
				while(tokens.hasMoreTokens()) {
					attributes.add(tokens.nextToken());
				}
				profile.put((String) key, attributes);
				
				log.debug(key.toString() + " with attributes: " + attributes);
			}
			log.debug(profile.size() + " class(es) loaded..");
			
		} catch(Exception e) {
			log.error("failed to load " + name);
		}
	}

	private Map<String, List<String>> profile;
	
	private static Logger log = Logger.getLogger(DADLSerializer.class);
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
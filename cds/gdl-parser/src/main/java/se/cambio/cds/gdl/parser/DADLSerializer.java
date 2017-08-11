package se.cambio.cds.gdl.parser;

import org.apache.commons.lang.StringUtils;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.*;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.*;

public class DADLSerializer {

    public DADLSerializer() {
        loadProfile();
    }

    public List<String> toDADL(Object obj) {
        List<String> lines = new ArrayList<>();
        return toDADL(obj, 1, lines);
    }

    private List<String> toDADL(Object obj, int indent, List<String> lines) {

        log.debug("toDADL on obj.getClass: {}, indent: {}, line.size: {}",
                obj.getClass().getCanonicalName(), indent, lines.size());

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

        if (profile.containsKey(className)) {
            attributes = sortByPreferredOrder(attributes, profile.get(className));
        }

        String name;
        Object value;
        for (String attribute : attributes) {
            name = attribute;
            if (outputShouldBeIgnoredInGdl(obj, name)
                    || objectIsGeneratedIn2ndPhaseParsing(name)) {
                continue;
            }

            Method getter = getter(name, obj.getClass());
            if (getter != null) {
                try {
                    value = getter.invoke(obj);
                } catch (IllegalAccessException | InvocationTargetException ex) {
                    throw new RuntimeException(ex);
                }
                addValue(indent, lines, name, value);
            }
        }
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < indent - 1; i++) {
            sb.append("\t");
        }
        sb.append(">");
        lines.add(sb.toString());
        return lines;
    }

    private void addValue(int indent, List<String> lines, String name, Object value) {
        StringBuilder sb = new StringBuilder();
        if (value != null) {
            for (int i = 0; i < indent; i++) {
                sb.append("\t");
            }
            sb.append(toUnderscoreSeparated(name));
            sb.append(" = ");

            if (value instanceof List) {
                addListObject(indent, lines, (List) value, sb);
            } else if (value instanceof Map) {
                addMapObject(indent, lines, (Map) value, sb);
            } else if (isSupportedPrimitiveType(value)) {
                addPrimitiveValue(lines, value, sb);
            } else {
                addComplexBlock(indent, lines, value, sb);
            }
        }
    }

    private void addComplexBlock(int indent, List<String> lines, Object value, StringBuilder sb) {
        log.debug("complex block..");
        lines.add(sb.toString());
        toDADL(value, indent + 1, lines);
    }

    private void addPrimitiveValue(List<String> lines, Object value, StringBuilder sb) {
        log.debug("value.type: {}", value.getClass());
        sb.append("<");
        if (value instanceof String) {
            sb.append("\"");
            sb.append(value);
            sb.append("\"");
        } else if (value instanceof CodePhrase) {
            sb.append("[");
            sb.append(value);
            sb.append("]");
        } else {
            sb.append(value.toString());
        }
        sb.append(">");
        lines.add(sb.toString());
    }

    private void addMapObject(int indent, List<String> lines, Map<Object,Object> valueMap, StringBuilder buf) {
        if (!valueMap.isEmpty()) {
            buf.append("<");
            lines.add(buf.toString());
            SortedSet<Object> sorted = new TreeSet<>(valueMap.keySet());
            for (Object key : sorted) {
                buf = new StringBuilder();
                for (int k = 0; k < indent + 1; k++) {
                    buf.append("\t");
                }
                String keystr;
                if (key instanceof CodePhrase) {
                    keystr = "[" + key + "]";
                } else {
                    keystr = "\"" + key.toString() + "\"";
                }

                Object member = valueMap.get(key);
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
            buf = new StringBuilder();
            for (int i = 0; i < indent; i++) {
                buf.append("\t");
            }
            buf.append(">");
            lines.add(buf.toString());
        }
    }

    private void addListObject(int indent, List<String> lines, List<Object> valueList, StringBuilder buf) {
        if (!valueList.isEmpty()) {
            buf.append("<");

            Object member;
            if (valueList.size() > 0
                    && isSupportedPrimitiveType(valueList.get(0))) {
                if (valueList.get(0) instanceof String) {
                    for (int i = 0, j = valueList.size(); i < j; i++) {
                        buf.append("\"");
                        buf.append(valueList.get(i));
                        buf.append("\"");
                        if (i != j - 1) {
                            buf.append(", ");
                        }
                    }
                } else if (valueList.get(0) instanceof CodePhrase) {
                    for (int i = 0, j = valueList.size(); i < j; i++) {
                        buf.append("[");
                        buf.append(valueList.get(i));
                        buf.append("]");
                        if (i != j - 1) {
                            buf.append(", ");
                        }
                    }
                }
                if (valueList.size() == 1) {
                    buf.append(",...");
                }
                buf.append(">");
                lines.add(buf.toString());
            } else {
                for (int i = 0, j = valueList.size(); i < j; i++) {
                    member = valueList.get(i);
                    lines.add(buf.toString());
                    buf = new StringBuilder();
                    for (int k = 0; k < indent + 1; k++) {
                        buf.append("\t");
                    }
                    lines.add(buf.toString() + "[" + (i + 1)
                            + "] = ");
                    toDADL(member, indent + 2, lines);
                }
                buf = new StringBuilder();
                for (int i = 0; i < indent; i++) {
                    buf.append("\t");
                }
                buf.append(">");
                lines.add(buf.toString());
            }
        }
    }

    private boolean objectIsGeneratedIn2ndPhaseParsing(String name) {
        return "preConditionExpressions".equals(name)
                || "defaultActionExpressions".equals(name)
                || "whenStatements".equals(name)
                || "thenStatements".equals(name)
                || "predicateStatements".equals(name);
    }

    private boolean outputShouldBeIgnoredInGdl(Object obj, String name) {
        return "id".equals(name)
                && (obj instanceof ArchetypeBinding || obj instanceof Rule
                || obj instanceof ElementBinding
                || obj instanceof Binding
                || obj instanceof TermBinding
                || obj instanceof Term
                || obj instanceof TermDefinition
                || obj instanceof ResourceDescriptionItem || obj instanceof TranslationDetails);
    }

    private boolean isSupportedPrimitiveType(Object obj) {
        return (obj instanceof String) || (obj instanceof Integer)
                || (obj instanceof Double) || (obj instanceof CodePhrase);
    }

    private Method getter(String attributeName, Class klass) {
        Method[] methods = klass.getMethods();
        String name = "get" + attributeName.substring(0, 1).toUpperCase()
                + attributeName.substring(1);

        log.debug("search getter method of name '{}'", name);

        for (Method method : methods) {
            if (method.getName().equals(name)) {
                Type[] paras = method.getParameterTypes();
                if (paras.length == 0) {
                    log.debug("found '{}'", name);
                    return method;
                }
            }
        }
        return null;
    }

    private SortedSet<String> attributeList(Class klass) {
        SortedSet<String> set = new TreeSet<>();
        Method[] methods = klass.getMethods();
        for (Method method : methods) {
            String name = method.getName();
            if (name.startsWith("set")) {
                name = name.substring(3);
                name = name.substring(0, 1).toLowerCase() + name.substring(1);
                set.add(name);
            }
        }
        log.debug("attribute list: {}", set);
        return set;
    }

    private List<String> sortByPreferredOrder(Collection<String> attributes,
                                              List<String> preferred) {
        List<String> ret = new ArrayList<>();

        for (String attr : preferred) {
            if (attributes.contains(attr)) {
                ret.add(attr);
            }
        }
        for (String attr : attributes) {
            if (!ret.contains(attr)) {
                ret.add(attr);
            }
        }
        return ret;
    }

    private String toUnderscoreSeparated(String camelCase) {
        String[] array = StringUtils.splitByCharacterTypeCamelCase(camelCase);
        StringBuilder buf = new StringBuilder();
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

    private void loadProfile() {
        profile = new HashMap<>();
        try {
            InputStream input = this.getClass().getClassLoader().getResourceAsStream("gdl.properties");
            Properties props = new Properties();
            props.load(input);
            Enumeration keys = props.propertyNames();
            StringTokenizer tokens;
            List<String> attributes;
            while (keys.hasMoreElements()) {
                Object key = keys.nextElement();
                String line = props.getProperty((String) key);
                log.debug(line);
                tokens = new StringTokenizer(line, ",");
                attributes = new ArrayList<>();
                while (tokens.hasMoreTokens()) {
                    attributes.add(tokens.nextToken());
                }
                profile.put((String) key, attributes);
                log.debug("{} with attributes: {}", key.toString(), attributes);
            }
            log.debug("{} class(es) loaded..", profile.size());
        } catch (Exception ex) {
            log.error("failed to load {}", "gdl.properties");
        }
    }

    private Map<String, List<String>> profile;

    private static Logger log = LoggerFactory.getLogger(DADLSerializer.class);
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
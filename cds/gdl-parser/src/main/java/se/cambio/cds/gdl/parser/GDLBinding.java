package se.cambio.cds.gdl.parser;

import lombok.extern.slf4j.Slf4j;
import org.openehr.am.parser.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

@Slf4j
public class GDLBinding {
    private static final String MODEL_PACKAGE = "se.cambio.cds.gdl.model.";

    Object createModelClass(String name, Map<String, Object> valueMap)
            throws ClassNotFoundException, SecurityException,
            NoSuchMethodException, IllegalArgumentException,
            InstantiationException, IllegalAccessException,
            InvocationTargetException {

        log.debug("class: " + name + ", valueMap: " + valueMap);

        String className = MODEL_PACKAGE + toCamelCase(name);
        Class klass = Class.forName(className);
        Constructor constructor = klass.getConstructor();
        Object obj = constructor.newInstance();
        Method[] methods = klass.getMethods();
        for (Method method : methods) {
            String methodName = method.getName();
            if (methodName.startsWith("set")) {
                for (Map.Entry<String, Object> entry : valueMap.entrySet()) {
                    String attribute = entry.getKey();
                    String setter = "set" + toCamelCase(attribute);

                    if (methodName.equals(setter)) {
                        log.debug("setter: " + setter);
                        method.invoke(obj, entry.getValue());
                        break;
                    }
                }
            }
        }
        log.debug("class: " + name + " created !! ");
        return obj;
    }

    private String toCamelCase(String underscoreSeparated) {
        StringTokenizer tokens = new StringTokenizer(underscoreSeparated, "_");
        StringBuilder buf = new StringBuilder();
        while (tokens.hasMoreTokens()) {
            String word = tokens.nextToken();
            buf.append(word.substring(0, 1).toUpperCase());
            buf.append(word.substring(1).toLowerCase());
        }
        return buf.toString();
    }

    public GDLBinding() {
    }

    public Object bind(ContentObject co) throws BindingException {
        if (co.getAttributeValues() != null) {

            return bindAttributes(null, co.getAttributeValues());

        } else {
            ComplexObjectBlock complexObj = co.getComplexObjectBlock();
            return bindComplexBlock(complexObj);
        }
    }

    private Object bindAttributes(String type, List<AttributeValue> attributes)
            throws BindingException {

        log.debug("bind attributes for type: " + type);

        Map<String, Object> values = new HashMap<String, Object>();
        for (AttributeValue attr : attributes) {
            String id = attr.getId();
            Object value = bindObjectBlock(attr.getValue());
            values.put(id, value);
        }

        try {
            return createModelClass(type, values);
        } catch (Exception ex) {
            throw new BindingException("failed to create instance of " + type
                    + ", with values: " + values, ex);
        }
    }

    private Object bindObjectBlock(ObjectBlock block) throws BindingException {
        if (block instanceof PrimitiveObjectBlock) {
            return bindPrimitiveBlock((PrimitiveObjectBlock) block);
        } else {
            return bindComplexBlock((ComplexObjectBlock) block);
        }
    }

    private Object bindPrimitiveBlock(PrimitiveObjectBlock block)
            throws BindingException {

        if (block.getSimpleValue() != null) {
            return block.getSimpleValue().getValue();
        } else if (block.getSimpleListValue() != null) {
            List<SimpleValue> values = block.getSimpleListValue();
            List list = new ArrayList(values.size());
            for (SimpleValue sv : values) {
                list.add(sv.getValue());
            }
            return list;
        } else if (block.getSimpleIntervalValue() != null) {
            // TODO
            return null;
        } else if (block.getTermCode() != null) {
            return block.getTermCode();
        } else if (block.getTermCodeListValue() != null) {
            return block.getTermCodeListValue();
        } else {
            throw new BindingException("empty block");
        }
    }

    private Object bindComplexBlock(ComplexObjectBlock block) throws BindingException {

        if (block instanceof SingleAttributeObjectBlock) {
            SingleAttributeObjectBlock singleBlock = (SingleAttributeObjectBlock) block;

            // a special case to deal with empty attribute list
            if ("LIST".equalsIgnoreCase(singleBlock.getTypeIdentifier())
                    && singleBlock.getAttributeValues().isEmpty()) {

                return new ArrayList();
            }

            return bindAttributes(singleBlock.getTypeIdentifier(),
                    singleBlock.getAttributeValues());

        } else {
            MultipleAttributeObjectBlock multiBlock = (MultipleAttributeObjectBlock) block;
            List<KeyedObject> list = multiBlock.getKeyObjects();

            // can't tell list or map
            if (list.size() == 0) {
                return null;
            }
            // list
            if (isNumericKey(list.get(0))) {
                List<Object> valueList = new ArrayList<Object>();
                for (KeyedObject ko : list) {
                    Object value = bindObjectBlock(ko.getObject());
                    valueList.add(value);
                }
                return valueList;

            } else { // map
                Map map = new HashMap();
                for (KeyedObject ko : list) {
                    Object key = ko.getKey().getValue();
                    Object value = bindObjectBlock(ko.getObject());

                    // special setId() pattern for keyed objects
                    setId(value, key);
                    map.put(key, value);
                }
                return map;
            }
        }
    }

    private void setId(Object obj, Object key) {
        if (obj instanceof String) {
            return;
        }
        try {
            Class klass = obj.getClass();
            Method method = klass.getMethod("setId", key.getClass());
            method.invoke(obj, key);
        } catch (Exception ex) {
            log.warn("failed to setId for class: " + obj.getClass());
        }
    }

    private boolean isNumericKey(KeyedObject obj) {
        try {
            Integer.parseInt(obj.getKey().getValue().toString());
            return true;
        } catch (Exception ex) {
            return false;
        }
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
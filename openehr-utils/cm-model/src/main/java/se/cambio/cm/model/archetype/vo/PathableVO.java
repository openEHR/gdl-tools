package se.cambio.cm.model.archetype.vo;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class PathableVO implements Serializable {

    private static final long serialVersionUID = 25042012L;
    String name;
    String description;
    String type;
    String idArchetype;
    String idTemplate;
    String path;
    Integer lowerCardinality;
    Integer upperCardinality;

    public String getId() {
        return idArchetype + path;
    }

    public String getParentId() {
        int indexOfLastSlash = path.lastIndexOf("/");
        String parentPath = path.substring(0, indexOfLastSlash);
        if (parentPath.isEmpty()) {
            return null;
        }
        return idArchetype + parentPath;
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
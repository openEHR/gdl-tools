package se.cambio.openehr.model.instance;

import java.io.Serializable;

public class SimpleContainerInstance implements Serializable{

    private static final long serialVersionUID = 1L;
    private String id = null;
    private SimpleContainerInstance parentContainerInstance = null;

    public SimpleContainerInstance(String id, SimpleContainerInstance parentContainerInstance) {
	super();
	this.id = id;
	this.parentContainerInstance = parentContainerInstance;
    }
    public String getId() {
	return id;
    }
    public void setId(String id) {
	this.id = id;
    }

    public SimpleContainerInstance getParentContainerInstance() {
	return parentContainerInstance;
    }
    public void setParentContainerInstance(SimpleContainerInstance parentContainerInstance) {
	this.parentContainerInstance = parentContainerInstance;
    }
    public SimpleContainerInstance clone(){
	return new SimpleContainerInstance(id, parentContainerInstance);
    }
    public boolean containsOrContainedIn(SimpleContainerInstance containerInstance){
	if (containerInstance!=null){
	    SimpleContainerInstance containerInstance2 = this;
	    if (this.getId().length()>containerInstance.getId().length()){
		containerInstance2 = containerInstance;
		containerInstance = this;
	    }
	    while(containerInstance!=null && !containerInstance.equals(containerInstance2)){
		containerInstance = containerInstance.getParentContainerInstance();
	    }
	    return containerInstance!=null && containerInstance.equals(containerInstance2);
	}else{
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
package se.cambio.cds.gdl.converters.drools;


import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;

import org.drools.KnowledgeBase;
import org.drools.KnowledgeBaseFactory;
import org.drools.RuntimeDroolsException;
import org.drools.builder.KnowledgeBuilder;
import org.drools.builder.KnowledgeBuilderConfiguration;
import org.drools.builder.KnowledgeBuilderErrors;
import org.drools.builder.KnowledgeBuilderFactory;
import org.drools.builder.ResourceType;
import org.drools.definition.KnowledgePackage;
import org.drools.io.Resource;
import org.drools.io.ResourceFactory;

public class CompilationManager {
    
    public static byte[] compile(String guideStr) throws CompilationErrorException {
	Resource guide = ResourceFactory.newByteArrayResource(guideStr.getBytes());
	return compile(guide);
    }
    
    public static byte[] compile(Resource guide) 
	    throws CompilationErrorException {
	try {
	    Collection<Resource> guides = new ArrayList<Resource>();
	    guides.add(guide);
	    KnowledgeBase kb = getKnowledgeBase(guides);
	    KnowledgePackage kpakage = kb.getKnowledgePackages().iterator().next();
	    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	    ObjectOutputStream objOut = new ObjectOutputStream(baos);
	    objOut.writeObject(kpakage);
	    objOut.flush();
	    return baos.toByteArray();
	} catch (RuntimeDroolsException e) {
	    throw new CompilationErrorException(e);
	} catch (IOException e) {
	    throw new CompilationErrorException(e);
	}
    }

    public static KnowledgeBase getKnowledgeBase(Collection<Resource> guides)
	    throws CompilationErrorException{
	return getKnowledgeBase(guides, null);
    }

    public static KnowledgeBase getKnowledgeBase(Collection<Resource> guides, KnowledgeBuilderConfiguration kbc) 
	    throws CompilationErrorException{
	final KnowledgeBuilder kbuilder = KnowledgeBuilderFactory.newKnowledgeBuilder(kbc);
	for (Resource guia : guides) {
	    kbuilder.add( guia, ResourceType.DRL);
	}
	KnowledgeBase kbase = KnowledgeBaseFactory.newKnowledgeBase();
	kbase.addKnowledgePackages( kbuilder.getKnowledgePackages() );
	KnowledgeBuilderErrors packErrors = kbuilder.getErrors();
	if (packErrors.size()>0){
	    throw new CompilationErrorException(packErrors.iterator().next().getMessage());
	}
	return kbase;
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
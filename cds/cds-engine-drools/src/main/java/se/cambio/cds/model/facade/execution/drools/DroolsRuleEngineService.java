package se.cambio.cds.model.facade.execution.drools;

import org.apache.commons.io.IOUtils;
import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.KieRepository;
import org.kie.api.builder.Message;
import org.kie.api.io.Resource;
import org.kie.api.runtime.KieContainer;
import org.kie.internal.io.ResourceFactory;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.converters.drools.GDLDroolsConverter;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cm.model.guide.dto.GuideDTO;
import sun.nio.ch.IOUtil;

import java.io.*;
import java.util.*;

import static java.lang.String.format;

public class DroolsRuleEngineService implements RuleEngineService {

    private DroolsExecutionManager droolsExecutionManager;

    public DroolsRuleEngineService(DroolsExecutionManager droolsExecutionManager) {
        this.droolsExecutionManager = droolsExecutionManager;
    }

    public RuleExecutionResult execute(
            String ehrId,
            List<GuideDTO> guides,
            Collection<ArchetypeReference> archetypeReferences,
            Calendar date) {
        final HashSet<Object> workingMemoryObjects = new HashSet<>();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            workingMemoryObjects.addAll(archetypeReference.getElementInstancesMap().values());
            workingMemoryObjects.add(archetypeReference);
        }

        final ExecutionLogger executionLogger = new ExecutionLogger();
        if (!guides.isEmpty()) {
            LoggerFactory.getLogger(DroolsRuleEngineService.class).debug("Executing " + guides.size() + " guidelines using " + workingMemoryObjects.size() + " objects.");
            droolsExecutionManager.executeGuides(
                    guides, date, workingMemoryObjects, executionLogger);
        }
        final Set<ArchetypeReference> modifiedArchetypeReferences = new HashSet<>();
        //Search for modified elements
        for (ElementInstance elementInstance : executionLogger.getElementInstancesSet()) {
            modifiedArchetypeReferences.add(elementInstance.getArchetypeReference());
        }
        final List<RuleReference> ruleReferences =
                GuideUtil.getRuleReferences(executionLogger.getFiredRules());

        if (date == null) {
            date = Calendar.getInstance();
        }
        RuleExecutionResult ruleExecutionResult = new RuleExecutionResult(ehrId, date.getTime(), modifiedArchetypeReferences, executionLogger.getLog(), ruleReferences);
        ruleExecutionResult.setTimedOut(executionLogger.executionTimedOut());
        return ruleExecutionResult;
    }

    @Override
    public void cancelExecution() {
        droolsExecutionManager.cancelCurrentExecution();
    }

    @Override
    public void clearCache() {
        droolsExecutionManager.clearCache();
    }

    @Override
    public void setUseCache(boolean useCache) {
        droolsExecutionManager.setUseCache(useCache);
    }

    @Override
    public byte[] compile(Guide guide) {
        String droolsGuide = new GDLDroolsConverter(guide, droolsExecutionManager.getArchetypeManager()).convertToDrools();
        final KieServices kieServices = KieServices.Factory.get();
        final KieFileSystem kieFileSystem = kieServices.newKieFileSystem();
        Resource resource = getResource(droolsGuide);
        if (resource != null) {
            kieFileSystem.write("src/main/resources/" + guide.getId() + ".drl", resource);
        }
        final KieBuilder kieBuilder = kieServices.newKieBuilder(kieFileSystem);
        kieBuilder.buildAll();
        if (kieBuilder.getResults().hasMessages(Message.Level.ERROR)) {
            throw new RuntimeException("Build Errors:\n" + kieBuilder.getResults().toString());
        }
        final KieRepository kr = kieServices.getRepository();
        final KieContainer kContainer = kieServices.newKieContainer(kr.getDefaultReleaseId());
        return compiledGuideToByteArray(guide.getId(), kContainer.getKieBase());
    }

    private byte[] compiledGuideToByteArray(String guideId, Object compiledGuide) {
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            ObjectOutput out = new ObjectOutputStream(bos);
            out.writeObject(compiledGuide);
            out.flush();
            return bos.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(format("Error converting compiled guideline '%s' to byte array", guideId));
        }
    }

    private Resource getResource(String guideStr) {
        if (guideStr == null) {
            return null;
        }
        try {
            return ResourceFactory.newByteArrayResource(guideStr.getBytes("UTF8"));
        } catch (UnsupportedEncodingException exception) {
            throw new RuntimeException(exception);
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
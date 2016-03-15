package se.cambio.cds.gdl.converters.drools;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.kie.api.KieBase;
import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.KieRepository;
import org.kie.api.builder.Message;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.StatelessKieSession;
import org.kie.internal.io.ResourceFactory;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cds.util.RuleExecutionWMLogger;
import se.cambio.cds.util.export.CdsGsonBuilderFactory;
import se.cambio.openehr.util.configuration.CdsConfiguration;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.DataValueGenerator;

import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Type;
import java.util.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = {CdsConfiguration.class})
public class SimpleDroolsTest extends GDLTestCase {

    @Value("classpath:/multiple-guide-salience-test-ar.json")
    Resource testAR;

    @Test
    public void shouldExecuteGuidelinesInCorrectOrder() throws IOException, InstanceNotFoundException, InternalErrorException {

        Gson gson = new CdsGsonBuilderFactory().getGsonBuilder().create();
        InputStreamReader streamReader = new InputStreamReader(testAR.getInputStream());
        Type listType = new TypeToken<ArrayList<ArchetypeReference>>() {
        }.getType();
        Collection<ArchetypeReference> ars = gson.fromJson(streamReader, listType);
        Collection<ElementInstance> elementInstances = getElementInstances(ars);
        List<String> guideIds = new ArrayList<>();
        guideIds.add("CHA2DS2VASc_diagnosis_review.v1");
        guideIds.add("Stroke_prevention_compliance_checking_in_AF.v2");

        GuideManager guideManager = generateGuideManager(guideIds);
        ElementInstanceCollection eic = new ElementInstanceCollection();
        eic.addAll(elementInstances);
        Calendar date = Calendar.getInstance();
        cdsManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, date);
        Collection<ArchetypeReference> archetypeReferences = eic.getAllArchetypeReferences();

        final HashSet<Object> workingMemoryObjects = new HashSet<>();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            workingMemoryObjects.addAll(archetypeReference.getElementInstancesMap().values());
            workingMemoryObjects.add(archetypeReference);
        }

        final ExecutionLogger executionLogger = new ExecutionLogger();
        final RuleExecutionWMLogger ruleExecutionWMLogger = new RuleExecutionWMLogger();

        String drlFileName = "guides/simple_drools_test1.drl";
        final StatelessKieSession session = getStatelessKieSession(guideIds, date, ruleExecutionWMLogger, executionLogger, drlFileName);
        session.execute(workingMemoryObjects);
        executionLogger.setFiredRules(ruleExecutionWMLogger.getFiredRules());
    }

    @Test
    public void shouldExecuteCorrectAmountOfRules() throws IOException, InstanceNotFoundException, InternalErrorException {
        String drlFileName = "guides/simple_drools_test2.drl";
        Calendar date = Calendar.getInstance();

        final HashSet<Object> workingMemoryObjects = new HashSet<>();
        final ExecutionLogger executionLogger = new ExecutionLogger();
        final RuleExecutionWMLogger ruleExecutionWMLogger = new RuleExecutionWMLogger();

        List<String> guideIds = new ArrayList<>();
        guideIds.add("test_creation_and_order_1");
        guideIds.add("test_creation_and_order_2");

        ArchetypeReference ar = generateOngoingMedicationArchetypeReference("A01AA01");
        ElementInstanceCollection eic = new ElementInstanceCollection();
        eic.add(ar);
        GuideManager guideManager = generateGuideManager(guideIds);
        cdsManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, date);
        Collection<ArchetypeReference> archetypeReferences = eic.getAllArchetypeReferences();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            workingMemoryObjects.addAll(archetypeReference.getElementInstancesMap().values());
            workingMemoryObjects.add(archetypeReference);
        }

        final StatelessKieSession session = getStatelessKieSession(guideIds, date, ruleExecutionWMLogger, executionLogger, drlFileName);
        session.execute(workingMemoryObjects);
        final Set<ArchetypeReference> modifiedArchetypeReferences = new HashSet<>();

        for (ElementInstance elementInstance : executionLogger.getElementInstancesSet()) {
            modifiedArchetypeReferences.add(elementInstance.getArchetypeReference());
        }
        executionLogger.setFiredRules(ruleExecutionWMLogger.getFiredRules());
     }

    private StatelessKieSession getStatelessKieSession(List<String> guideIds, Calendar date, RuleExecutionWMLogger ruleExecutionWMLogger, ExecutionLogger executionLogger, String drlFileName) {
        final KieServices kieServices = KieServices.Factory.get();
        final KieFileSystem kieFileSystem = kieServices.newKieFileSystem();
        final KieRepository kr = kieServices.getRepository();
        org.kie.api.io.Resource resource = ResourceFactory.newClassPathResource(drlFileName);
        kieFileSystem.write("src/main/resources/test.drl", resource);
        final KieBuilder kieBuilder = kieServices.newKieBuilder(kieFileSystem);
        kieBuilder.buildAll();
        if (kieBuilder.getResults().hasMessages(Message.Level.ERROR)) {
            throw new RuntimeException("Build Errors:\n" + kieBuilder.getResults().toString());
        }
        final KieContainer kContainer = kieServices.newKieContainer(kr.getDefaultReleaseId());
        KieBase kieBase = kContainer.getKieBase();
        final StatelessKieSession session = kieBase.newStatelessKieSession();
        session.addEventListener(ruleExecutionWMLogger);
        final DvDateTime currentDateTime = DataValueGenerator.toDvDateTime(date);
        session.setGlobal("$currentDateTime", currentDateTime);
        session.setGlobal("$executionLogger", executionLogger);
        session.setGlobal("$bindingMap", new HashMap<ElementInstance, Map<String, Boolean>>());
        List<String> reverseGuideIds = new ArrayList<>(guideIds);
        Collections.reverse(reverseGuideIds);
        int initSalience = 0;
        for (String guideId : reverseGuideIds) {
            String guideSalienceId = getGuideSalienceId(guideId);
            session.setGlobal(guideSalienceId, initSalience);
            initSalience = initSalience + 1000;
        }
        return session;
    }

    public static String getGuideSalienceId(String guideId) {
        return "$" + guideId.replaceAll("[^a-zA-Z0-9]+", "") + "_salience";
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
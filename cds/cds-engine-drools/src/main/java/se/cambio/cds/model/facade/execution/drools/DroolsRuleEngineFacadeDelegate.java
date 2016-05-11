package se.cambio.cds.model.facade.execution.drools;

import org.apache.log4j.Logger;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cds.controller.execution.DroolsExecutionManager;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.converters.drools.CompilationManager;
import se.cambio.cds.gdl.converters.drools.GDLDroolsConverter;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.ExecutionLogger;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.*;

@Component
@Profile("rule-drools-engine")
public class DroolsRuleEngineFacadeDelegate implements RuleEngineFacadeDelegate {

    public RuleExecutionResult execute(
            String ehrId,
            List<GuideDTO> guides,
            Collection<ArchetypeReference> archetypeReferences,
            Calendar date)
            throws InternalErrorException, PatientNotFoundException {
        final HashSet<Object> workingMemoryObjects = new HashSet<>();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            workingMemoryObjects.addAll(archetypeReference.getElementInstancesMap().values());
            workingMemoryObjects.add(archetypeReference);
        }

        final ExecutionLogger executionLogger = new ExecutionLogger();
        if (!guides.isEmpty()) {
            Logger.getLogger(DroolsRuleEngineFacadeDelegate.class).debug("Executing " + guides.size() + " guides using " + workingMemoryObjects.size() + " objects.");
            DroolsExecutionManager.executeGuides(guides, date, workingMemoryObjects, executionLogger);
        }
        final Set<ArchetypeReference> modifiedArchetypeReferences = new HashSet<>();

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
        DroolsExecutionManager.cancelCurrentExecution();
    }

    @Override
    public void clearCache() {
        DroolsExecutionManager.clearCache();
    }

    @Override
    public void setUseCache(boolean useCache) {
        DroolsExecutionManager.setUseCache(useCache);
    }

    @Override
    public byte[] compile(Guide guide) throws InternalErrorException {
        try {
            String droolsGuide = new GDLDroolsConverter(guide, ArchetypeManager.getInstance()).convertToDrools();
            return CompilationManager.compile(droolsGuide);
        } catch (Exception e) {
            throw new InternalErrorException(e);
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
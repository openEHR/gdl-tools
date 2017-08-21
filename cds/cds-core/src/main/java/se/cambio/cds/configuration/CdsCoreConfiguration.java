package se.cambio.cds.configuration;

import org.springframework.context.annotation.*;
import org.springframework.core.env.Environment;
import se.cambio.cds.controller.cds.CdsDataManager;
import se.cambio.cds.controller.session.data.ArchetypeReferencesManager;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.model.facade.ehr.delegate.EhrService;
import se.cambio.cds.model.facade.ehr.dummy.configuration.EhrDummyServiceConfiguration;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineService;
import se.cambio.cds.util.*;
import se.cambio.cds.util.export.html.GuideHTMLExporter;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.openehr.controller.session.configuration.ClinicalModelsCacheConfiguration;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

@Configuration
@PropertySources({
        @PropertySource(value = "classpath:default-date-time-path.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "file:conf/date-time-path.properties", ignoreResourceNotFound = true),
        @PropertySource(value = "classpath:date-time-path.properties", ignoreResourceNotFound = true)})
@Import({ClinicalModelsCacheConfiguration.class, EhrDummyServiceConfiguration.class})
public class CdsCoreConfiguration {

    private static final String CDS_FILTER_ARCHETYPE_REFERENCES = "cds-execution.filter.archetype-references";

    @Bean
    DateTimeARFinder dateTimeARFinder(Environment environment) {
        return new DateTimeARFinder(environment);
    }

    @Bean
    RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter() {
        return new RepeatedArchetypeReferencesFilter();
    }

    @Bean
    PredicateFilterManager predicateFilterManager(TerminologyService terminologyService, RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter) {
        return new PredicateFilterManager(terminologyService, repeatedArchetypeReferencesFilter);
    }

    @Bean
    EhrDataFilterManager ehrDataFilterManager(DateTimeARFinder dateTimeARFinder, PredicateFilterManager predicateFilterManager) {
        return new EhrDataFilterManager(dateTimeARFinder, predicateFilterManager);
    }

    @Bean
    CdsDataManager cdsDataManager(
            Environment environment,
            DateTimeARFinder dateTimeARFinder,
            EhrDataFilterManager ehrDataFilterManager,
            ElementInstanceCollectionManager elementInstanceCollectionManager,
            EhrService ehrService) {
        boolean filterArchetypeReferences = environment.getProperty(CDS_FILTER_ARCHETYPE_REFERENCES, Boolean.class, true);
        return new CdsDataManager(dateTimeARFinder, ehrDataFilterManager, filterArchetypeReferences, elementInstanceCollectionManager, ehrService);
    }

    @Bean
    ElementInstanceCollectionManager elementInstanceCollectionManager(TerminologyService terminologyService) {
        return new ElementInstanceCollectionManager(terminologyService);
    }

    @Bean
    Guides guides(ClinicalModelsService clinicalModelsService, RuleEngineService ruleEngineService) {
        return new Guides(clinicalModelsService, ruleEngineService);
    }

    @Bean
    GuideImporter guideImporter(ArchetypeManager archetypeManager, ArchetypeReferencesManager archetypeReferencesManager) {
        return new GuideImporter(archetypeManager, archetypeReferencesManager);
    }

    @Bean
    GuideHTMLExporter guideHTMLExporter(ArchetypeManager archetypeManager, GuideImporter guideImporter) {
        return new GuideHTMLExporter(archetypeManager, guideImporter);
    }

    @Bean
    ArchetypeReferencesManager archetypeReferencesManager(ArchetypeManager archetypeManager) {
        return new ArchetypeReferencesManager(archetypeManager);
    }
}

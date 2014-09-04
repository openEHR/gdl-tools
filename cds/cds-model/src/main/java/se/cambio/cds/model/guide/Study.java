package se.cambio.cds.model.guide;

import org.openehr.rm.common.resource.ResourceDescription;
import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.model.study.GTCodeReference;
import se.cambio.cds.model.study.IndicatorRange;

import java.util.Collection;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 12:00
 */
public class Study {
    private String studyId;
    private String concept;
    private Language language;
    private ResourceDescription resourceDescription;
    private Map<String, TermDefinition> termDefinitions;
    private Collection<GTCodeReference> filters;
    private Collection<GTCodeReference> indicators;
    private Collection<IndicatorRange> indicatorRanges;

    public Study(
            String studyId,
            String concept,
            Language language,
            ResourceDescription resourceDescription,
            Map<String, TermDefinition> termDefinitions,
            Collection<GTCodeReference> filters,
            Collection<GTCodeReference> indicators,
            Collection<IndicatorRange> indicatorRanges) {
        this.studyId = studyId;
        this.concept = concept;
        this.language = language;
        this.resourceDescription = resourceDescription;
        this.termDefinitions = termDefinitions;
        this.filters = filters;
        this.indicators = indicators;
        this.indicatorRanges = indicatorRanges;
    }

    public String getStudyId() {
        return studyId;
    }

    public void setStudyId(String studyId) {
        this.studyId = studyId;
    }

    public String getConcept() {
        return concept;
    }

    public void setConcept(String concept) {
        this.concept = concept;
    }

    public Language getLanguage() {
        return language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public ResourceDescription getResourceDescription() {
        return resourceDescription;
    }

    public void setResourceDescription(ResourceDescription resourceDescription) {
        this.resourceDescription = resourceDescription;
    }

    public Map<String, TermDefinition> getTermDefinitions() {
        return termDefinitions;
    }

    public void setTermDefinitions(Map<String, TermDefinition> termDefinitions) {
        this.termDefinitions = termDefinitions;
    }

    public Collection<GTCodeReference> getFilters() {
        return filters;
    }

    public void setFilters(Collection<GTCodeReference> filters) {
        this.filters = filters;
    }

    public Collection<GTCodeReference> getIndicators() {
        return indicators;
    }

    public void setIndicators(Collection<GTCodeReference> indicators) {
        this.indicators = indicators;
    }

    public Collection<IndicatorRange> getIndicatorRanges() {
        return indicatorRanges;
    }

    public void setIndicatorRanges(Collection<IndicatorRange> indicatorRanges) {
        this.indicatorRanges = indicatorRanges;
    }
}

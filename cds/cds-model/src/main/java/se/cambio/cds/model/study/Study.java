package se.cambio.cds.model.study;

import se.cambio.cds.gdl.model.Language;
import se.cambio.cds.gdl.model.ResourceDescription;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 12:00
 */
public class Study {
    private String studyId;
    private Language language;
    private ResourceDescription resourceDescription;
    private Map<String, StudyDefinition> studyDefinitions;
    private Collection<String> guideIds;
    private Collection<GTCodeReference> filters;
    private Collection<GTCodeReference> indicators;
    private Collection<IndicatorRange> indicatorRanges;

    public Study(String studyId){
        this.studyId = studyId;
    }

    public String getStudyId() {
        return studyId;
    }

    public void setStudyId(String studyId) {
        this.studyId = studyId;
    }


    public Language getLanguage() {
        return language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public ResourceDescription getResourceDescription() {
        if (resourceDescription == null) {
            resourceDescription = new ResourceDescription();
        }
        return resourceDescription;
    }

    public void setResourceDescription(ResourceDescription resourceDescription) {
        this.resourceDescription = resourceDescription;
    }

    public Map<String, StudyDefinition> getStudyDefinitions() {
        if (studyDefinitions == null) {
            studyDefinitions = new HashMap<String, StudyDefinition>();
        }
        return studyDefinitions;
    }

    public void setStudyDefinitions(Map<String, StudyDefinition> studyDefinitions) {
        this.studyDefinitions = studyDefinitions;
    }

    public Collection<String> getGuideIds() {
        if (guideIds == null) {
            guideIds = new ArrayList<String>();
        }
        return guideIds;
    }

    public void setGuideIds(Collection<String> guideIds) {
        this.guideIds = guideIds;
    }

    public Collection<GTCodeReference> getFilters() {
        if (filters == null) {
            filters = new ArrayList<GTCodeReference>();
        }
        return filters;
    }

    public void setFilters(Collection<GTCodeReference> filters) {
        this.filters = filters;
    }

    public Collection<GTCodeReference> getIndicators() {
        if (indicators == null) {
            indicators = new ArrayList<GTCodeReference>();
        }
        return indicators;
    }

    public void setIndicators(Collection<GTCodeReference> indicators) {
        this.indicators = indicators;
    }

    public Collection<IndicatorRange> getIndicatorRanges() {
        if (indicatorRanges == null) {
            indicatorRanges = new ArrayList<IndicatorRange>();
        }
        return indicatorRanges;
    }

    public void setIndicatorRanges(Collection<IndicatorRange> indicatorRanges) {
        this.indicatorRanges = indicatorRanges;
    }
}

package se.cambio.cds.model.dsvdata;

import se.cambio.cds.model.facade.cds.vo.DomainData;
import se.cambio.cds.model.facade.execution.vo.RuleReference;

import java.util.ArrayList;
import java.util.List;

public class DSVData {
    private DomainData ehrDomainData;
    private DomainData cdsDomainData;
    private List<RuleReference> firedRules = new ArrayList<RuleReference>();

    public DSVData() {
    }

    public DSVData(DomainData ehrDomainData, DomainData cdsDomainData) {
        this.ehrDomainData = ehrDomainData;
        this.cdsDomainData = cdsDomainData;
    }

    public DomainData getEhrDomainData() {
        return ehrDomainData;
    }

    public void setEhrDomainData(DomainData ehrDomainData) {
        this.ehrDomainData = ehrDomainData;
    }

    public DomainData getCdsDomainData() {
        return cdsDomainData;
    }

    public void setCdsDomainData(DomainData cdsDomainData) {
        this.cdsDomainData = cdsDomainData;
    }

    public List<RuleReference> getFiredRules() {
        return firedRules;
    }

    public void setFiredRules(List<RuleReference> firedRules) {
        this.firedRules = firedRules;
    }
}

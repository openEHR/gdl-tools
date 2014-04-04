package se.cambio.cds.model.integration;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2014-04-03
 * Time: 08:54
 */
public class EHRData implements Serializable{
    private static final long serialVersionUID = 20140404L;
    private String ehrId;
    private String dataFormat;
    private List<EHRDataInstance> ehrDataInstanceList;

    public EHRData(String ehrId, String dataFormat) {
        this.ehrId = ehrId;
        this.dataFormat = dataFormat;
        this.ehrDataInstanceList = new ArrayList<EHRDataInstance>();
    }

    public String getEhrId() {
        return ehrId;
    }

    public void setEhrId(String ehrId) {
        this.ehrId = ehrId;
    }

    public void add(EHRDataInstance ehrDataInstance){
        getEhrDataInstanceList().add(ehrDataInstance);
    }

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    public List<EHRDataInstance> getEhrDataInstanceList() {
        return ehrDataInstanceList;
    }

    public void setEhrDataInstanceList(List<EHRDataInstance> ehrDataInstanceList) {
        this.ehrDataInstanceList = ehrDataInstanceList;
    }
}

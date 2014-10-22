package se.cambio.cds.model.study;

import java.io.Serializable;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 12:03
 */
public class GTCodeReference implements Serializable {

    private String guideId;
    private String gtCode;

    public GTCodeReference(String guideId, String gtCode) {
        this.guideId = guideId;
        this.gtCode = gtCode;
    }

    public String getGuideId() {
        return guideId;
    }

    public void setGuideId(String guideId) {
        this.guideId = guideId;
    }

    public String getGtCode() {
        return gtCode;
    }

    public void setGtCode(String gtCode) {
        this.gtCode = gtCode;
    }

    @Override
    public String toString() {
        return "GTCodeReference{" +
                "guideId='" + guideId + '\'' +
                ", gtCode='" + gtCode + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof GTCodeReference)) return false;

        GTCodeReference that = (GTCodeReference) o;

        if (gtCode != null ? !gtCode.equals(that.gtCode) : that.gtCode != null) return false;
        if (guideId != null ? !guideId.equals(that.guideId) : that.guideId != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = guideId != null ? guideId.hashCode() : 0;
        result = 31 * result + (gtCode != null ? gtCode.hashCode() : 0);
        return result;
    }
}

package se.cambio.cds.model.study;

/**
 * User: iago.corbal
 * Date: 2014-09-04
 * Time: 12:07
 */
public class IndicatorRange {
    private GTCodeReference indicator;
    private String indicatorValue;
    private Double max;
    private Double min;
    private Double recommended;

    public IndicatorRange(GTCodeReference indicator, String indicatorValue, Double max, Double min, Double recommended) {
        this.indicator = indicator;
        this.indicatorValue = indicatorValue;
        this.max = max;
        this.min = min;
        this.recommended = recommended;
    }

    public GTCodeReference getIndicator() {
        return indicator;
    }

    public void setIndicator(GTCodeReference indicator) {
        this.indicator = indicator;
    }

    public String getIndicatorValue() {
        return indicatorValue;
    }

    public void setIndicatorValue(String indicatorValue) {
        this.indicatorValue = indicatorValue;
    }

    public Double getMax() {
        return max;
    }

    public void setMax(Double max) {
        this.max = max;
    }

    public Double getMin() {
        return min;
    }

    public void setMin(Double min) {
        this.min = min;
    }

    public Double getRecommended() {
        return recommended;
    }

    public void setRecommended(Double recommended) {
        this.recommended = recommended;
    }
}

package se.cambio.cds.util;

public class MathFunctionProxy {

    public int abs(int a) {
        return Math.abs(a);
    }
    public long abs(long a) {
        return Math.abs(a);
    }
    public float abs(float a) {
        return Math.abs(a);
    }
    public double abs(double a) {
        return Math.abs(a);
    }

    public double round(double a) {
        return Math.round(a);
    }
    public float round(float a) {
        return Math.round(a);
    }

    public double floor(double a) {
        return Math.floor(a);
    }

    public double ceil(double a) {
        return Math.ceil(a);
    }

    public double log(double a) {
        return Math.log(a);
    }

    public double log10(double a) {
        return Math.log10(a);
    }

    public double log1p(double a) {
        return Math.log1p(a);
    }
}

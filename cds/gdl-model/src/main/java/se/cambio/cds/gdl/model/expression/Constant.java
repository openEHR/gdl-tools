package se.cambio.cds.gdl.model.expression;

public enum Constant {
    E("e", "A mathematical constant that is the base of the natural logarithm");

    private String name;
    private String description;

    Constant(String name, String description) {
        this.name = name;
        this.description = description;
    }

    @Override
    public String toString() {
        return this.name;
    }

    public String description() {
        return this.description;
    }
}
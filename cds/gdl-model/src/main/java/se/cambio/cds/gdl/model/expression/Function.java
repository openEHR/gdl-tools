package se.cambio.cds.gdl.model.expression;

public enum Function {
    LN("ln", "Natural logarithm"),
    LOG("log", "Base 10 logarithm"),
    MAX("max", "Maximum value"),
    MIN("min", "Minimum value");

    private String name;
    private String description;

    Function(String name, String description) {
        this.name = name;
        this.description = description;
    }

    public String description() {
        return this.description;
    }

    @Override
    public String toString() {
        return this.name;
    }
}

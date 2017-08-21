package se.cambio.cds.gdl.model.expression;

import lombok.Data;

import java.io.Serializable;

@Data
public class Function implements Serializable {

    private final String name;

    public Function(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }
}

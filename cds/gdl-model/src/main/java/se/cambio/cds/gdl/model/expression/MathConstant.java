package se.cambio.cds.gdl.model.expression;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class MathConstant extends ConstantExpression {
    private Constant constant;

    MathConstant(Constant constant) {
        super(constant.toString());
        this.constant = constant;
    }

    public static MathConstant create(Constant constant) {
        if (constant == null) {
            throw new IllegalArgumentException("null constant");
        }
        return new MathConstant(constant);
    }
}

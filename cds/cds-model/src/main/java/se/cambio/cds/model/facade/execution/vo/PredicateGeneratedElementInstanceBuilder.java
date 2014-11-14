package se.cambio.cds.model.facade.execution.vo;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ContainerInstance;

public class PredicateGeneratedElementInstanceBuilder {
    private String id;
    private DataValue dataValue;
    private ArchetypeReference archetypeReference;
    private ContainerInstance containerInstance;
    private DvCodedText nullFlavour;
    private OperatorKind operatorKind;

    public PredicateGeneratedElementInstanceBuilder setId(String id) {
        this.id = id;
        return this;
    }

    public PredicateGeneratedElementInstanceBuilder setDataValue(DataValue dataValue) {
        this.dataValue = dataValue;
        return this;
    }

    public PredicateGeneratedElementInstanceBuilder setArchetypeReference(ArchetypeReference archetypeReference) {
        this.archetypeReference = archetypeReference;
        return this;
    }

    public PredicateGeneratedElementInstanceBuilder setContainerInstance(ContainerInstance containerInstance) {
        this.containerInstance = containerInstance;
        return this;
    }

    public PredicateGeneratedElementInstanceBuilder setNullFlavour(DvCodedText nullFlavour) {
        this.nullFlavour = nullFlavour;
        return this;
    }

    public PredicateGeneratedElementInstanceBuilder setOperatorKind(OperatorKind operatorKind) {
        this.operatorKind = operatorKind;
        return this;
    }

    public PredicateGeneratedElementInstance createPredicateGeneratedElementInstance() {
        return new PredicateGeneratedElementInstance(id, dataValue, archetypeReference, containerInstance, nullFlavour, operatorKind);
    }
}
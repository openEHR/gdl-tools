package se.cambio.cds.util;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.TermMapping;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cm.controller.terminology.TerminologyService;

import java.util.*;


public class PredicateFilterManager {

    private TerminologyService terminologyService;
    private RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter;
    private Logger logger = LoggerFactory.getLogger(PredicateFilterManager.class);

    public PredicateFilterManager(
            TerminologyService terminologyService,
            RepeatedArchetypeReferencesFilter repeatedArchetypeReferencesFilter) {
        this.terminologyService = terminologyService;
        this.repeatedArchetypeReferencesFilter = repeatedArchetypeReferencesFilter;
    }

    public void filterByPredicates(
            Collection<ArchetypeReference> definitionArchetypeReferences,
            Collection<ArchetypeReference> ehrArchetypeReferences, Calendar date) {
        boolean filterActive = true;
        repeatedArchetypeReferencesFilter.filter(definitionArchetypeReferences);
        Set<ArchetypeReference> archetypeReferences = new HashSet<>();
        for (ArchetypeReference archetypeReference : definitionArchetypeReferences) {
            Collection<ArchetypeReference> ehrDataForArchetype = getEhrDataForArchetype(ehrArchetypeReferences, archetypeReference.getIdArchetype());
            Collection<ArchetypeReference> filteredEhrData = getFilteredEhrData(archetypeReference, date, ehrDataForArchetype);
            if (filteredEhrData.size() == ehrArchetypeReferences.size()) {
                filterActive = false;
                break;
            }
            archetypeReferences.addAll(filteredEhrData);
        }
        if (filterActive) {
            ehrArchetypeReferences.clear();
            ehrArchetypeReferences.addAll(archetypeReferences);
        }
    }

    private Collection<ArchetypeReference> getEhrDataForArchetype(Collection<ArchetypeReference> ehrArchetypeReferences, String idArchetype) {
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<>();
        for (ArchetypeReference archetypeReference : ehrArchetypeReferences) {
            if (idArchetype.equals(archetypeReference.getIdArchetype())) {
                archetypeReferences.add(archetypeReference);
            }
        }
        return archetypeReferences;
    }

    private void filterEhrData(
            Collection<ArchetypeReference> ehrArchetypeReferences, Calendar date, PredicateGeneratedElementInstance predicate) {
        if (OperatorKind.MAX.equals(predicate.getOperatorKind())) {
            filterMaxMin(predicate.getId(), ehrArchetypeReferences, true);
        } else if (OperatorKind.MIN.equals(predicate.getOperatorKind())) {
            filterMaxMin(predicate.getId(), ehrArchetypeReferences, false);
        } else if (OperatorKind.IS_A.equals(predicate.getOperatorKind())) {
            filterIsA(predicate, ehrArchetypeReferences, false);
        } else if (OperatorKind.IS_NOT_A.equals(predicate.getOperatorKind())) {
            filterIsA(predicate, ehrArchetypeReferences, true);
        } else if (OperatorKind.EQUALITY.equals(predicate.getOperatorKind())) {
            filterEquals(predicate, ehrArchetypeReferences, false);
        } else if (OperatorKind.INEQUAL.equals(predicate.getOperatorKind())) {
            filterEquals(predicate, ehrArchetypeReferences, true);
        } else if (OperatorKind.GREATER_THAN_OR_EQUAL.equals(predicate.getOperatorKind())) {
            filterGreaterLessThanPredicate(predicate.getId(), predicate.getDataValue(), ehrArchetypeReferences, true, date);
        } else if (OperatorKind.LESS_THAN_OR_EQUAL.equals(predicate.getOperatorKind())) {
            filterGreaterLessThanPredicate(predicate.getId(), predicate.getDataValue(), ehrArchetypeReferences, false, date);
        }
    }

    private void filterEquals(
            PredicateGeneratedElementInstance predicate, Collection<ArchetypeReference> ehrArchetypeReferences, boolean negation) {
        final Set<ArchetypeReference> archetypeReferencesToRemove = new HashSet<>();
        for (ArchetypeReference archetypeReference : ehrArchetypeReferences) {
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get(predicate.getId());
            if (elementInstance != null) {
                DataValue dataValue = elementInstance.getDataValue();
                if (dataValue != null) {
                    boolean equals = dataValue.equals(predicate.getDataValue());
                    if ((!equals && !negation) || (equals && negation)) {
                        archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    }
                } else {
                    archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                }
            }
        }
        ehrArchetypeReferences.removeAll(archetypeReferencesToRemove);
    }

    private void filterMaxMin(String elementId, Collection<ArchetypeReference> ehrArchetypeReferences, boolean max) {
        final Set<ArchetypeReference> archetypeReferencesToRemove = new HashSet<>();
        ElementInstance maxElementInstance = null;
        for (ArchetypeReference archetypeReference : ehrArchetypeReferences) {
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get(elementId);
            if (elementInstance != null) {
                if (elementInstance.getDataValue() != null) {
                    if (maxElementInstance == null || isMaxMin(elementInstance, maxElementInstance, max)) {
                        if (maxElementInstance != null) {
                            archetypeReferencesToRemove.add(maxElementInstance.getArchetypeReference());
                        }
                        maxElementInstance = elementInstance;
                    } else {
                        archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    }
                } else {
                    archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                }
            }
        }
        ehrArchetypeReferences.removeAll(archetypeReferencesToRemove);
    }


    private void filterGreaterLessThanPredicate(
            String elementId, DataValue dv,
            Collection<ArchetypeReference> ehrArchetypeReferences,
            boolean greaterThan, Calendar date) {
        if (dv instanceof CurrentTimeExpressionDataValue) {
            OperatorKind operatorKind = greaterThan ? OperatorKind.GREATER_THAN_OR_EQUAL : OperatorKind.LESS_THAN_OR_EQUAL;
            dv = ElementInstanceCollectionManager.resolvePredicate(dv, operatorKind, null, date);
            if (dv == null) {
                LoggerFactory.getLogger(PredicateFilterManager.class).warn("No Data Value returned after resolving predicate!");
            }
        }
        final Set<ArchetypeReference> archetypeReferencesToRemove = new HashSet<>();
        for (ArchetypeReference archetypeReference : ehrArchetypeReferences) {
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get(elementId);
            if (elementInstance != null) {
                if (elementInstance.getDataValue() != null) {
                    int compare = DVUtil.compareDVs(dv, elementInstance.getDataValue());
                    if (compare != 0 && ((greaterThan && compare > 0) || (!greaterThan && compare < 0))) {
                        archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    }
                } else {
                    archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                }
            }
        }
        ehrArchetypeReferences.removeAll(archetypeReferencesToRemove);
    }

    private boolean isMaxMin(ElementInstance elementInstance, ElementInstance maxElementInstance, boolean max) {
        int compare = DVUtil.compareDVs(elementInstance.getDataValue(), maxElementInstance.getDataValue());
        if (max) {
            return compare >= 0;
        } else {
            return compare < 0;
        }
    }

    private void filterIsA(
            PredicateGeneratedElementInstance predicate, Collection<ArchetypeReference> ehrArchetypeReferences, boolean negation) {
        final Set<ArchetypeReference> archetypeReferencesToRemove = new HashSet<>();
        final Set<CodePhrase> codePhrases = getCodePhrases(predicate);
        ElementInstance elementInstance;
        for (ArchetypeReference archetypeReference : ehrArchetypeReferences) {
            elementInstance = archetypeReference.getElementInstancesMap().get(predicate.getId());
            if (elementInstance != null && codePhrases != null) {
                CodePhrase codePhrase = getCodePhrase(elementInstance);
                try {
                    if (codePhrase != null) {
                        if ("local".equalsIgnoreCase(codePhrase.getTerminologyId().name())) {
                            filterIsALocalTerminology(negation, archetypeReferencesToRemove, codePhrases, elementInstance, codePhrase);
                        } else {
                            filterIsAExternalTerminology(negation, archetypeReferencesToRemove, codePhrases, elementInstance, codePhrase);
                        }
                    } else {
                        archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    }
                } catch (Exception exception) {
                    archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    logger.warn("Filter isA ", exception);
                }
            }
        }
        ehrArchetypeReferences.removeAll(archetypeReferencesToRemove);

    }

    private void filterIsALocalTerminology(
            boolean negation, Set<ArchetypeReference> archetypeReferencesToRemove, Set<CodePhrase> codePhrases,
            ElementInstance elementInstance, CodePhrase codePhrase) {
        boolean isA = simpleIsACheck(codePhrase, codePhrases);
        if ((!isA && !negation) || (isA && negation)) {
            archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
        }
    }

    private boolean simpleIsACheck(CodePhrase codePhrase, Set<CodePhrase> codePhrases) {
        for (CodePhrase codePhraseAux: codePhrases) {
            if (codePhrase.getCodeString().equalsIgnoreCase(codePhraseAux.getCodeString())) {
                return true;
            }
        }
        return false;
    }

    private void filterIsAExternalTerminology(
            boolean negation, Set<ArchetypeReference> archetypeReferencesToRemove, Set<CodePhrase> codePhrases,
            ElementInstance elementInstance, CodePhrase codePhrase) {
        boolean isA = terminologyService.isSubclassOf(codePhrase, codePhrases);
        if ((!isA && !negation) || (isA && negation)) {
            archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
        }
    }

    private CodePhrase getCodePhrase(ElementInstance elementInstance) {
        DataValue dataValue = elementInstance.getDataValue();
        if (dataValue instanceof DvCodedText) {
            return ((DvCodedText) dataValue).getDefiningCode();
        } else if (dataValue instanceof DvOrdinal) {
            return ((DvOrdinal) dataValue).getSymbol().getDefiningCode();
        } else {
            return null;
        }
    }

    private Set<CodePhrase> getCodePhrases(PredicateGeneratedElementInstance predicate) {
        DataValue dataValue = predicate.getDataValue();
        Set<CodePhrase> codePhrases = new HashSet<>();
        if (dataValue instanceof DvCodedText) {
            DvCodedText codedText = (DvCodedText) dataValue;
            List<TermMapping> mappings = codedText.getMappings();
            if (mappings != null) {
                for (TermMapping termMapping : mappings) {
                    codePhrases.add(termMapping.getTarget());
                }
            } else {
                codePhrases.add(codedText.getDefiningCode());
            }
        }
        return codePhrases;
    }

    private List<PredicateGeneratedElementInstance> getSortedPredicates(ArchetypeReference archetypeReference) {
        List<PredicateGeneratedElementInstance> predicates = getPredicates(archetypeReference);
        predicates.sort(new PredicateSorter());
        return predicates;
    }

    private List<PredicateGeneratedElementInstance> getPredicates(ArchetypeReference archetypeReference) {
        List<PredicateGeneratedElementInstance> predicates = new ArrayList<>();
        for (ElementInstance elementInstance : archetypeReference.getElementInstancesMap().values()) {
            if (elementInstance instanceof PredicateGeneratedElementInstance) {
                PredicateGeneratedElementInstance pgei = (PredicateGeneratedElementInstance) elementInstance;
                predicates.add(pgei);
            }
        }
        return predicates;
    }

    private Collection<ArchetypeReference> getFilteredEhrData(
            ArchetypeReference archetypeReference, Calendar date, Collection<ArchetypeReference> ehrArchetypeReferences) {
        Collection<ArchetypeReference> ehrData = new ArrayList<>();
        ehrData.addAll(ehrArchetypeReferences);
        List<PredicateGeneratedElementInstance> predicates = getSortedPredicates(archetypeReference);
        for (PredicateGeneratedElementInstance predicate : predicates) {
            filterEhrData(ehrData, date, predicate);
        }
        return ehrData;
    }
}

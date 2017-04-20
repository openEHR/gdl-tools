package se.cambio.cds.gdl.editor.controller.interfaces;

import se.cambio.cds.gdl.model.ResourceDescription;
import se.cambio.cds.gdl.model.Term;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import java.io.InputStream;
import java.util.Collection;
import java.util.Map;

public interface EditorController<E> {

    void init();

    E getEntity();

    void setEntity(E entity);

    String getTitle();

    JPanel getEditorPanel();

    void runIfOKToExit(Runnable runnable);

    String createNextLocalCode();

    Map<String, Term> getCurrentTermsMap();

    void changeLanguage(String language);

    Collection<String> getUsedCodes();

    ResourceDescription getResourceDescription();

    Term getConceptTerm();

    String getCurrentLanguageCode();

    Collection<String> getSupportedLanguageCodes();

    String getEntityId();

    void setEntityId(String entityId);

    void save();

    void saveAs();

    void entitySaved();

    String getSerializedEntity() throws InternalErrorException;

    String getEntityName();

    Collection<String> getSupportedEntityExtensions();

    void updateTerm(Term term);

    Boolean close();

}

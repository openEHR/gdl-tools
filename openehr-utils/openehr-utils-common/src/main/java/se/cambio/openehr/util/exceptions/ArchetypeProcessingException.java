package se.cambio.openehr.util.exceptions;

public class ArchetypeProcessingException extends InternalErrorException {

    public ArchetypeProcessingException(String message) {
        super(new Exception("Parsing exception: " + message));
    }
}

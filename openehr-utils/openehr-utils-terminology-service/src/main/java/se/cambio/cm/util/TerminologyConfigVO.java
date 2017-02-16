package se.cambio.cm.util;

public class TerminologyConfigVO {
    private String terminologyId;
    private boolean simpleParentCheck;
    private boolean codeExistenceCheck;
    private boolean cleanCodes;
    private String clazz;

    public TerminologyConfigVO(String terminologyId, boolean simpleParentCheck, boolean codeExistenceCheck, boolean cleanCodes, String clazz) {
        this.terminologyId = terminologyId;
        this.simpleParentCheck = simpleParentCheck;
        this.codeExistenceCheck = codeExistenceCheck;
        this.cleanCodes = cleanCodes;
        this.clazz = clazz;
    }

    public String getTerminologyId() {
        return terminologyId;
    }

    public boolean isSimpleParentCheck() {
        return simpleParentCheck;
    }

    public boolean isCodeExistenceCheck() {
        return codeExistenceCheck;
    }

    public boolean isCleanCodes() {
        return cleanCodes;
    }

    public String getClazz() {
        return clazz;
    }
}

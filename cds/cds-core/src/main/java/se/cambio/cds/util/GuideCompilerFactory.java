package se.cambio.cds.util;

import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.CDSConfigurationParametersManager;

public class GuideCompilerFactory {
    private static String DELEGATE_CLASS = "GuideCompiler/Class";

    private GuideCompilerFactory() {
    }

    private static Class<?> getDelegateClass() throws InternalErrorException {
        Class<?> theClass = null;
        try {
            String delegateClassName =
                    CDSConfigurationParametersManager.getParameter(DELEGATE_CLASS);
            theClass = Class.forName(delegateClassName);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        } catch (Throwable th) {
            throw new InternalErrorException(new Exception(th.getMessage()));
        }
        return theClass;
    }

    public static GuideCompiler getDelegate()
            throws InternalErrorException {
        try {
            return (GuideCompiler)getDelegateClass().newInstance();
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}

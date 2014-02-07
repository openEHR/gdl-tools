package se.cambio.cds.util;

import org.apache.log4j.Logger;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.util.misc.OpenEHRConfigurationParametersManager;

import java.io.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-04
 * Time: 18:46
 */
public class LocalCM {

    private static String AUTO_SAVE_FILE_CM_PREFIX = "auto.cm";
    private static LocalCM _instance;
    private boolean loaded = false;

    public static void loadLocalCM() throws InternalErrorException {
        loadLocalCM(false);
    }

    public static boolean isLoaded(){
        return getDelegate().loaded;
    }

    public static void loadLocalCM(boolean force) throws InternalErrorException {
        if (!isLoaded() || force){
            String hostname = null;
            try{
                hostname = OpenEHRConfigurationParametersManager.getParameter(OpenEHRConfigurationParametersManager.OPENEHR_SERVER_HOST);
            } catch (MissingConfigurationParameterException e) {
            }
            if (hostname==null){
                hostname = "local";
            }
            String fileName = AUTO_SAVE_FILE_CM_PREFIX+"."+hostname+".zip";
            Logger.getLogger(LocalCM.class).info("Loading data from local CM. Using '"+fileName+"'...");
            InputStream is = null;
            try{
                is = LocalCM.class.getClassLoader().getResourceAsStream(fileName);
                if (is==null){
                    File file = new File(fileName);
                    if (file.exists()){
                        try {
                            is = new FileInputStream(file);
                        } catch (FileNotFoundException e) {
                            ExceptionHandler.handle(e);
                        }
                    }
                }
                if (is!=null){
                    try {
                        CMImportExportManager.importCM(is);
                    } catch (IOException e) {
                        throw new InternalErrorException(e);
                    }
                }
            }finally {
                if (is!=null){
                    try {
                        is.close();
                    } catch (IOException e) {
                        ExceptionHandler.handle(e);
                    }
                }
            }
            getDelegate().loaded = true;
        }
    }

    public static void saveLocalCM() throws InternalErrorException {
        String hostname = null;
        try{
            hostname = OpenEHRConfigurationParametersManager.getParameter(OpenEHRConfigurationParametersManager.OPENEHR_SERVER_HOST);
        } catch (MissingConfigurationParameterException e) {
        }
        try {
            if (hostname==null){
                hostname = "local";
            }
            File file = new File(AUTO_SAVE_FILE_CM_PREFIX+"."+hostname+".zip");
            CMImportExportManager.exportCurrentCM(file, true);
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
    }

    private static LocalCM getDelegate(){
        if (_instance==null){
            _instance = new LocalCM();
        }
        return _instance;
    }
}

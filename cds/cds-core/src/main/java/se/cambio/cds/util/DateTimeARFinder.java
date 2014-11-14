package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.joda.time.DateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRRMUtil;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;


public class DateTimeARFinder {
    private static DateTimeARFinder _delegate = null;
    public static final String CONFIGURATION_FILE = "DateTimePath.properties";
    private static final String CONFIGURATION_FOLDER = "conf";
    private static Map <Object,Object> dvDateTimePathsByArchetypeId;
    private static Logger logger = Logger.getLogger(DateTimeARFinder.class);

    static {
        dvDateTimePathsByArchetypeId = Collections.synchronizedMap(new HashMap<Object, Object>());
        try {
            Class<DateTimeARFinder> configurationParametersManagerClass =
                    DateTimeARFinder.class;
            ClassLoader classLoader =
                    configurationParametersManagerClass.getClassLoader();
            File configFile = getConfigFile();
            InputStream inputStream = null;
            if (configFile!=null) {
                inputStream = new FileInputStream(configFile);
                Logger.getLogger(DateTimeARFinder.class).info("*** Using '"+CONFIGURATION_FOLDER+"' folder for '"+CONFIGURATION_FILE+"'");
            } else {
                inputStream = classLoader.getResourceAsStream(CONFIGURATION_FILE);
                Logger.getLogger(DateTimeARFinder.class).info("*** Using resource for '"+CONFIGURATION_FILE+"'");
            }
            Properties properties = new Properties();
            properties.load(inputStream);
            inputStream.close();
            dvDateTimePathsByArchetypeId.putAll(properties);
        } catch (Exception e) {
            Logger.getLogger(DateTimeARFinder.class).warn("*** Configuration Data Time Path file '" + CONFIGURATION_FILE + "' not found!");
        }
    }

    private DateTimeARFinder() {

    }


    private static File getConfigFile() {
        try{
            File jarFile = new File(DateTimeARFinder.class.getProtectionDomain().getCodeSource().getLocation().getPath());
            //../conf
            if (!jarFile.exists()){
                throw new FileNotFoundException();
            }
            for (File file: jarFile.getParentFile().getParentFile().listFiles()) {
                if (file.isDirectory() && file.getName().equals(CONFIGURATION_FOLDER)) {
                    for (File file2:file.listFiles()) {
                        if (file2.getName().equals(CONFIGURATION_FILE)) {
                            return file2;
                        }
                    }
                }
            }
        } catch(Exception t) {
            logger.debug("CONF Folder not found in jar: " + t.getMessage());
        }
        try {
            //Current folder
            File file = new File(CONFIGURATION_FOLDER+File.separator+CONFIGURATION_FILE);
            if (!file.exists()){
                throw new FileNotFoundException();
            }
            return file;
        } catch(Exception t2) {
            logger.warn("CONF Folder not found in file system: " + t2.getMessage());
        }
        return null;
    }

    public static DateTime getDateTime(ArchetypeReference ar) {
        String dvDateTimePath = getEventTimePath(ar.getIdArchetype());
        if (dvDateTimePath!=null) {
            ElementInstance ei = ar.getElementInstancesMap().get(ar.getIdArchetype()+dvDateTimePath);
            return getDateTime(ei);
        }else{
            return null;
        }
    }

    public static String getEventTimePath(String archetypeId) {
        String rmName = AqlUtil.getKind(archetypeId);
        if (OpenEHRConst.OBSERVATION.equals(rmName)) {
            return OpenEHRRMUtil.EVENT_TIME_PATH;
        }else if (OpenEHRConst.ACTION.equals(rmName)) {
            return OpenEHRRMUtil.TIME_PATH;
        }else if (OpenEHRConst.EVALUATION.equals(rmName) ||
                OpenEHRConst.INSTRUCTION.equals(rmName)) {
            String dateTimePath = (String)dvDateTimePathsByArchetypeId.get(archetypeId);
            if (dateTimePath==null) {
                Logger.getLogger(DateTimeARFinder.class).warn("Unregistered DvDateTime for '"+archetypeId+"', please add the path to '"+CONFIGURATION_FILE+"'");
            }
            return dateTimePath;
        }else{
            Logger.getLogger(DateTimeARFinder.class).warn("Unknown RM '"+rmName+"'");
            return null;
        }
    }

    private static DateTime getDateTime(ElementInstance ei) {
        if (ei!=null) {
            if(ei.getDataValue() instanceof DvDateTime) {
                DvDateTime dvDateTime = ((DvDateTime)ei.getDataValue());
                if (dvDateTime.getDateTime()!=null) {
                    return dvDateTime.getDateTime();
                }else{
                    Logger.getLogger(DateTimeARFinder.class).warn("Element instance '"+ei.getId()+"' has no DVDateTime.");
                }
            }else{
                Logger.getLogger(DateTimeARFinder.class).warn("Element instance '"+ei.getId()+"' data value is not DVDateTime.");
            }
        }else{
            Logger.getLogger(DateTimeARFinder.class).warn("Element instance null");
        }
        return null;
    }

    public static DateTimeARFinder getDelegate() {
        if(_delegate==null) {
            _delegate = new DateTimeARFinder();
        }
        return _delegate;
    }
}

package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.joda.time.DateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.util.OpenEHRConst;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * User: Iago.Corbal
 * Date: 2013-09-26
 * Time: 15:55
 */
public class DateTimeARFinder {
    private static DateTimeARFinder _delegate = null;
    //private static Pattern DATE_TIME_PATH_RE =  Pattern.compile("\\Q/data\\E[\\[.*\\]]?\\Q/events\\E[\\[.*\\]]?\\Q/time\\E[\\[.*\\]]?");
    private static String EVENT_TIME_PATH = "/event/time"; //TODO FIX!!??!!
    public static final String CONFIGURATION_FILE = "DateTimePath.properties";
    private static final String CONFIGURATION_FOLDER = "conf";
    private static Map <Object,Object> dvDateTimePathsByArchetypeId;

    static {
	/*
	 * We use a synchronized map because it will be filled by using a
	 * lazy strategy.
	 */
        dvDateTimePathsByArchetypeId = Collections.synchronizedMap(new HashMap<Object, Object>());
        try {
	    /* Read property file (if exists).*/
            Class<DateTimeARFinder> configurationParametersManagerClass =
                    DateTimeARFinder.class;
            ClassLoader classLoader =
                    configurationParametersManagerClass.getClassLoader();
            File configFile = getConfigFile();
            InputStream inputStream = null;
            if (configFile!=null){
                inputStream = new FileInputStream(configFile);
                Logger.getLogger(DateTimeARFinder.class).info("*** Using '"+CONFIGURATION_FOLDER+"' folder for '"+CONFIGURATION_FILE+"'");
            }else{
                inputStream = classLoader.getResourceAsStream(CONFIGURATION_FILE);
                Logger.getLogger(DateTimeARFinder.class).info("*** Using resource for '"+CONFIGURATION_FILE+"'");
            }
            Properties properties = new Properties();
            properties.load(inputStream);
            inputStream.close();
	        /* We have been able to read the file. */
            dvDateTimePathsByArchetypeId.putAll(properties);
        } catch (Exception e) {
	        /* We have not been able to read the file. */
            Logger.getLogger(DateTimeARFinder.class).warn("*** Configuration Data Time Path file '" + CONFIGURATION_FILE + "' not found!");
        }
    }

    private DateTimeARFinder(){

    }


    private static File getConfigFile(){
        try{
            File jarFile = new File(DateTimeARFinder.class.getProtectionDomain().getCodeSource().getLocation().getPath());
            //../conf
            for (File file:jarFile.getParentFile().getParentFile().listFiles()){
                if (file.isDirectory() && file.getName().equals(CONFIGURATION_FOLDER)){
                    for (File file2:file.listFiles()){
                        if (file2.getName().equals(CONFIGURATION_FILE)){
                            return file2;
                        }
                    }
                }
            }
        }catch(Throwable t){
            //Problem finding config folder
            //Loggr.getLogger(UserConfigurationManager.class).warn("CONF Folder not found "+t.getMessage());
        }
        try{
            //Current folder
            File file = new File(CONFIGURATION_FOLDER+File.separator+CONFIGURATION_FILE);
            if (file.exists()){
                return file;
            }

        }catch(Throwable t2){
            //Problem finding config folder
            //Logger.getLogger(UserConfigurationManager.class).warn("CONF Folder not found "+t.getMessage());
        }
        return null;
    }

    public static DateTime getDateTime(ArchetypeReference ar){
        String dvDateTimePath = getEventTimePath(ar.getIdArchetype());
        if (dvDateTimePath!=null){
            ElementInstance ei = ar.getElementInstancesMap().get(ar.getIdArchetype()+dvDateTimePath);
            return getDateTime(ei);
        }else{
            return null;
        }
    }

    public static String getEventTimePath(String archetypeId){
        String rmName = AqlUtil.getKind(archetypeId);
        if (OpenEHRConst.OBSERVATION.endsWith(rmName)){
            /* If we use a format /data[at****]/events[at****]/time we need to iterate through the different paths looking for the one that matches the regular expression
            Iterator<ElementInstance> i = ar.getElementInstancesMap().values().iterator();
            while(i.hasNext()){
                ElementInstance ei = i.next();
                String path = ei.getId().substring(ei.getId().indexOf("/"));
                if (DATE_TIME_PATH_RE.matcher(path).find()){
                    dvDateTimePath = path;
                    break;
                }
            }
             */
            return EVENT_TIME_PATH;
        }else if (OpenEHRConst.EVALUATION.endsWith(rmName) ||
                OpenEHRConst.ACTION.endsWith(rmName) ||
                OpenEHRConst.INSTRUCTION.endsWith(rmName)){
            String dateTimePath = (String)dvDateTimePathsByArchetypeId.get(archetypeId);
            if (dateTimePath==null){
                Logger.getLogger(DateTimeARFinder.class).warn("Unregistered DvDateTime for '"+archetypeId+"', please add the path to '"+CONFIGURATION_FILE+"'");
            }
            return dateTimePath;
        }else{
            Logger.getLogger(DateTimeARFinder.class).warn("Unknown RM '"+rmName+"'");
            return null;
        }
    }

    private static DateTime getDateTime(ElementInstance ei){
        if (ei!=null){
            if(ei.getDataValue() instanceof DvDateTime){
                DvDateTime dvDateTime = ((DvDateTime)ei.getDataValue());
                if (dvDateTime.getDateTime()!=null){
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

    public static DateTimeARFinder getDelegate(){
        if(_delegate==null){
            _delegate = new DateTimeARFinder();
        }
        return _delegate;
    }
}

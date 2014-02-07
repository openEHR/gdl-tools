/*
 * Created on 19-jul-2006
 *


 */
package se.cambio.openehr.util;

import java.io.File;
import java.io.IOException;

import org.apache.log4j.Logger;

/**
 * @author icorram
 *


 */
public class ErrLog {

    //private static String LOGGIN_PROPPERTIES_FILE = "loggin.properties";
    //private static String LOGGIN_FILE = "java.util.logging.FileHandler.pattern";

    private static ErrLog _instance = null;
    private Logger _logger;
    private File _logFile;
    private String _logFileName = null;

    private ErrLog(){
	_logger = Logger.getLogger(ErrLog.class);
	/*TODO Create error log
	try {
	    Class<ErrLog> SisLog = ErrLog.class;
	    ClassLoader classLoader = SisLog.getClassLoader();
	    InputStream inputStream = classLoader.getResourceAsStream(LOGGIN_PROPPERTIES_FILE);

	    Properties properties = new Properties();
	    properties.load(inputStream);

	    _logFileName = (String)properties.get(LOGGIN_FILE);
	    _logFile = new File(_logFileName);

	    if (!_logFile.exists()){
		_logFile.mkdirs();
		_logFile.delete();
		_logFile.createNewFile();
	    }
	    inputStream = classLoader.getResourceAsStream(LOGGIN_PROPPERTIES_FILE);
	    LogManager lm = LogManager.getLogManager();
	    lm.readConfiguration(inputStream);
	    inputStream.close();
	} catch (Exception e) {
	    e.printStackTrace();
	}*/
    }

    public static File getLogFile(){
	return getDelegate()._logFile;
    }

    public static void log(String msg){
	getDelegate()._logger.error(msg);

    }

    public static void limpiarLog() throws IOException{
	getDelegate()._logFile.delete();
	getDelegate()._logFile=new File(getDelegate()._logFileName);
	getDelegate()._logFile.createNewFile();
    }

    public static void logException(String msg, Throwable th){
	getDelegate()._logger.error(msg,th);
    }

    public static ErrLog getDelegate(){
	if (_instance==null){
	    _instance = new ErrLog();
	}
	return _instance;
    }

}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */
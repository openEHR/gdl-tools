package se.cambio.cds.util;

import java.util.HashMap;
import java.util.Map;

public class LogEventVO {

    public String context = null;
    public String actionName = null;
    public Long duration = null;
    public Map<String, String> parameters = null;

    public LogEventVO(String context, String actionName, Long duration) {
	super();
	this.context = context;
	this.actionName = actionName;
	this.duration = duration;
	parameters = new HashMap<String, String>();
    }

    public String getContext() {
        return context;
    }

    public void setContext(String context) {
        this.context = context;
    }

    public String getActionName() {
        return actionName;
    }

    public void setActionName(String actionName) {
        this.actionName = actionName;
    }

    public Long getDuration() {
        return duration;
    }

    public void setDuration(Long duration) {
        this.duration = duration;
    }

    public void addParameter(String key, String value){
	parameters.put(key, value);
    }
    
    public Map<String, String> getParameters() {
        return parameters;
    }
    
    public String toString(){
	return actionName+" ("+duration+" ms)";
    }  
}

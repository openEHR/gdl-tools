package se.cambio.cds.util;

import org.apache.log4j.Layout;
import org.apache.log4j.spi.LoggingEvent;

import com.google.gson.JsonObject;



public class LogEventJSONLayout extends Layout {
    @Override
    public String format(LoggingEvent le) {
	if (le.getMessage() instanceof LogEventVO){
	    LogEventVO logEventVO = (LogEventVO)le.getMessage();
	    JsonObject eventContainer = new JsonObject();
	    eventContainer.addProperty("id", 1000L);
	    eventContainer.addProperty("createTimestamp", le.timeStamp);

	    JsonObject meta = new JsonObject();
	    eventContainer.add("meta", meta);
	    meta.addProperty("accountId", "cds");
	    meta.addProperty("userAgent", "cds_server");
	    //meta.put("sessionId", "CDSExecution");

	    //JSONObject geotag = new JSONObject();
	    //meta.put("geotag", geotag);
	    //geotag.put("hostname", "CDSExecution");

	    JsonObject user = new JsonObject();
	    eventContainer.add("user", user);
	    user.addProperty("username", "cds_server");
	    user.addProperty("displayName", "CDS Execution");

	    JsonObject event = new JsonObject();
	    eventContainer.add("event", event);
	    event.addProperty("timestamp", le.timeStamp);
	    //event.put("idle", 0);//TODO 
	    event.addProperty("context", logEventVO.getContext());
	    //event.put("referrer", "CDS Execution");
	    event.addProperty("action", logEventVO.getActionName());
	    //event.put("actionType", "CDS Execution");
	    event.addProperty("label", "CDS Execution");
	    event.addProperty("duration", logEventVO.getDuration());
	    
	    JsonObject parameters = new JsonObject();
	    eventContainer.add("parameters", parameters);
	    for (String key : logEventVO.getParameters().keySet()) {
		parameters.addProperty(key, logEventVO.getParameters().get(key));
	    }

	    //JsonObject audit = new JsonObject();
	    //eventContainer.add("audit", audit);
	    //audit.put("eventId", "7");
	    //audit.put("level", le.getLevel().toString());
	    //audit.put("auditSourceId", "CDS Execution");
	    //audit.put("auditSourceType", "CDS Execution");
	    //JSONObject participants = new JSONObject();
	    //audit.put("participants", participants);
	    //participants.put("destination", "cds");

	    String str = eventContainer+"\n";
	    return str;
	}
	return null;
    }

    @Override
    public boolean ignoresThrowable() {
	return false;
    }

    @Override
    public void activateOptions() {

    }


}

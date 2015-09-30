package se.cambio.openehr.util;

import org.apache.log4j.Logger;

import java.util.concurrent.Future;

/**
 * User: Iago.Corbal
 * Date: 2013-12-23
 * Time: 13:33
 */
public class DefaultProgressManager implements ProgressManager {
    private double lastUpdatedProgress = 0.0;
    private String message = "";
    private double progress = 0.0;
    private String id;

    public DefaultProgressManager(String id) {
        this.id = id;
    }

    @Override
    public void changeLoadingText(String description) {
        message = description;
        Logger.getLogger(DefaultProgressManager.class).info("DPM: " + description);
    }

    @Override
    public void start() {
        Logger.getLogger(DefaultProgressManager.class).info("DPM: Starting");
    }

    @Override
    public void stop() {
        message = null;
        progress = 0.0;
        lastUpdatedProgress = 0.0;
        Logger.getLogger(DefaultProgressManager.class).info("DPM: Stopping");
    }

    @Override
    public void setCurrentProgress(String msg, double progress) {
        this.message = msg;
        this.progress = progress;
        if (Math.abs(lastUpdatedProgress - progress) >= 0.2) {
            Logger.getLogger(DefaultProgressManager.class).info("DPM: " + msg + "(progress=" + progress + ")");
            lastUpdatedProgress = progress;
        }
    }

    @Override
    public void setCurrentThread(Future<?> currentThread) {
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public double getCurrentProgress() {
        return progress;
    }

    @Override
    public String getCurrentMessage() {
        return message;
    }

    @Override
    public Future<?> getCurrentThread() {
        return null;
    }
}

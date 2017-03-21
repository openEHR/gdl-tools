package se.cambio.openehr.util;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.Future;

public class DefaultProgressManager implements ProgressManager {
    private double lastUpdatedProgress = 0.0;
    private String message = "";
    private double progress = 0.0;
    private String id;
    private static Logger logger = LoggerFactory.getLogger(DefaultProgressManager.class);


    public DefaultProgressManager(String id) {
        this.id = id;
    }

    @Override
    public void changeLoadingText(String description) {
        message = description;
        logger.info("DPM: " + description);
    }

    @Override
    public void start() {
        logger.info("DPM: Starting");
    }

    @Override
    public void stop() {
        message = null;
        progress = 0.0;
        lastUpdatedProgress = 0.0;
        logger.info("DPM: Stopping");
    }

    @Override
    public void setCurrentProgress(String msg, double progress) {
        this.message = msg;
        this.progress = progress;
        if (Math.abs(lastUpdatedProgress - progress) >= 0.2) {
            logger.info("DPM: " + msg + "(progress=" + progress + ")");
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

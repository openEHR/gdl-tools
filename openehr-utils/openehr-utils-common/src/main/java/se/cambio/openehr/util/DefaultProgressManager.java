package se.cambio.openehr.util;

import org.apache.log4j.Logger;

import java.util.concurrent.Future;

/**
 * User: Iago.Corbal
 * Date: 2013-12-23
 * Time: 13:33
 */
public class DefaultProgressManager implements ProgressManager {
    private double _lastProgress = 0.0;

    @Override
    public void changeLoadingText(String description) {
        Logger.getLogger(DefaultProgressManager.class).info("DPM: "+description);
    }

    @Override
    public void start() {
        Logger.getLogger(DefaultProgressManager.class).info("DPM: Starting");

    }

    @Override
    public void stop() {
        Logger.getLogger(DefaultProgressManager.class).info("DPM: Stopping");
    }

    @Override
    public void setCurrentProgress(String msg, double progress) {
        if (Math.abs(_lastProgress-progress)>=0.2){
            Logger.getLogger(DefaultProgressManager.class).info("DPM: "+msg+"(progress="+progress+")");
            _lastProgress = progress;
        }
    }

    @Override
    public void setCurrentThread(Future<?> currentThread) {
    }

    @Override
    public Future<?> getCurrentThread() {
        return null;
    }
}

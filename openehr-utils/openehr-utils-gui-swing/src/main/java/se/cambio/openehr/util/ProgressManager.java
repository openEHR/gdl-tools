package se.cambio.openehr.util;

import java.util.concurrent.Future;

/**
 * User: Iago.Corbal
 * Date: 2013-10-31
 * Time: 18:20
 */
public interface ProgressManager {

    public void changeLoadingText(String description);
    public void start();
    public void stop();
    public void setCurrentProgress(String msg, double progress);
    public void setCurrentThread(Future<?> currentThread);
    public Future<?> getCurrentThread();
}

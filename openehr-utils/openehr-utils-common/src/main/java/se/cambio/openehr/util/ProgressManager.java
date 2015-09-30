package se.cambio.openehr.util;

import java.util.concurrent.Future;

/**
 * User: Iago.Corbal
 * Date: 2013-10-31
 * Time: 18:20
 */
public interface ProgressManager {

    void changeLoadingText(String description);
    void start();
    void stop();
    void setCurrentProgress(String msg, double progress);
    void setCurrentThread(Future<?> currentThread);
    String getId();
    double getCurrentProgress();
    String getCurrentMessage();
    Future<?> getCurrentThread();
}
